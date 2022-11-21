library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
source('quantgen.R')

###############################################################################
# SETUP                                                                       #
###############################################################################
geo_type <- 'state'
response_data_source = 'hhs'
response_signal = 'confirmed_admissions_influenza_1d_prop_7dav'
ahead = 5 + 7*(0:3)
ntrain = 21
lags = c(0, 7, 14)
tau = evalcast::covidhub_probs()
states_dc_pr_vi = c('al', 'ak', 'az', 'ar', 'ca', 'co', 'ct', 'dc', 'de', 'fl',
                    'ga', 'hi', 'id', 'il', 'in', 'ia', 'ks', 'ky', 'la', 'me',
                    'md', 'ma', 'mi', 'mn', 'ms', 'mo', 'mt', 'ne', 'nv', 'nh',
                    'nj', 'nm', 'ny', 'nc', 'nd', 'oh', 'ok', 'or', 'pa', 'ri',
                    'sc', 'sd', 'tn', 'tx', 'ut', 'vt', 'va', 'wa', 'wv', 'wi',
                    'wy', 'pr', 'vi')
forecast_dates = lubridate::today()

if (strftime(forecast_dates, '%w') != '1') {
  warning('Forecaster being run on a day that is not a Monday.',
          'The forecaster assumes that it is being run on Monday',
          'in which aheads are predicted.')
}

make_start_day_ar = function(ahead, ntrain, lags) {
  offset = eval(1 - max(ahead) - ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}
start_day_ar = make_start_day_ar(ahead, ntrain, lags)

###############################################################################
# SETTINGS FOR DIFFERENT MODELS                                               #
###############################################################################
idx = 1 
signals_df = tribble(
  ~data_source,         ~signal,        ~lags,            ~name,
  'chng','smoothed_adj_outpatient_flu', list(lags, lags), 'CMU-TimeSeries',
)

###############################################################################
# LEARN THE QAR MODEL AND SAVE OUTPUT                                         #
###############################################################################
forecaster_name = signals_df$name[idx]

signals_ar = tibble::tibble(
                      data_source = unique(c(response_data_source,
                                             signals_df$data_source[idx])),
                      signal = unique(c(response_signal,
                                        signals_df$signal[idx])),
                      start_day = list(start_day_ar),
                      geo_values=list(states_dc_pr_vi),
                      geo_type=geo_type)

t0 = Sys.time()
preds_state <- get_predictions(quantgen_forecaster,
                      forecaster_name,
                      signals_ar,
                      forecast_dates,
                      incidence_period='day',
                      forecaster_args=list(
                          signals=signals_ar,
                          incidence_period='day',
                          ahead=ahead,
                          geo_type=geo_type,
                          tau=tau,
                          n=ntrain,
                          lags=signals_df$lags[[idx]], 
                          lambda=0,
                          nonneg=TRUE,
                          sort=TRUE,
                          lp_solver='glpk' # Docker doesn't support Gurobi
                          )
                      )
t1 = Sys.time()
print(t1-t0)

state_pop = readr::read_csv('state_pop.csv', show_col_types = FALSE) %>% rename (
      geo_value=state_id,
    ) %>% select (
      -state_name,
    )
INCIDENCE_RATE = 100000

preds_state$quantile = signif(preds_state$quantile, 4) # Eliminate rounding issues

# Transform to weekly incidence counts (not prop)
preds_state_processed = preds_state %>% inner_join (
      state_pop,
      by='geo_value',
    ) %>% transmute (
      geo_value = geo_value,
      ahead = ahead,
      forecaster = "flu-model",
      data_source = "hhs",
      signal = "confirmed_admissions_influenza_1d_7dav",
      incidence_period = "day",
      forecast_date = forecast_date,
      target = sprintf('%d wk ahead inc flu hosp', (ahead-5)/7+1),
      target_end_date = target_end_date,
      location=state_code,
      type='quantile',
      quantile=quantile,
      value=value*pop/INCIDENCE_RATE*7,
    )

# US-level forecasts
preds_us_unsorted = preds_state_processed %>% group_by (
      forecast_date,
      target,
      target_end_date,
      type,
      quantile,
    ) %>% summarize (
      location='US',
      value=sum(value),
    ) %>% ungroup
preds_us_list = preds_us_unsorted %>% group_split (
      forecast_date,
      target,
      target_end_date,
      type,
    )
for (idx in 1:length(preds_us_list)) {
  preds_us_list[[idx]]$quantile = sort(preds_us_list[[idx]]$quantile)
  preds_us_list[[idx]]$value = sort(preds_us_list[[idx]]$value)
}
preds_us <- bind_rows(preds_us_list)
preds_us$data_source <- "hhs"
preds_us$signal <- "confirmed_admissions_influenza_1d_7dav"
preds_us$forecaster <- "flu-model"
preds_us$incidence_period <- "day"
preds_us$geo_value <- "us"

preds_full = bind_rows(preds_state_processed, preds_us) %>% arrange(
      location,
  ) %>%
  # Remove VI as data has just been zeroes and forecaster time window leads to
  # following trends from other locations and doesn't cover 0. (But keep it when
  # forming the national predictions above.)
  filter(.data$geo_value != "vi") 
  # Prod run exclude geos
  # filter(!.data$geo_value %in% c("hi", "ga", "fl", "la", "sc", "va", "tx", "al"))

readr::write_csv(preds_full,
                 sprintf('data-forecasts/CMU-TimeSeries/%s-CMU-TimeSeries-prediction-cards.csv', forecast_dates),
                 # quote='all' is important to make sure the location column is quoted.
                 quote='all')

drops <- c("incidence_period", "geo_value", "ahead", "forecaster", "data_source", "signal")
preds_full <- preds_full[, !(names(preds_full) %in% drops)]

readr::write_csv(preds_full,
                 sprintf('data-forecasts/CMU-TimeSeries/%s-CMU-TimeSeries.csv', forecast_dates),
                 # quote='all' is important to make sure the location column is quoted.
                 quote='all')

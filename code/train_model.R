library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
source(here::here("code", "quantgen.R"))
source(here::here("code", "ensemble.R"))

# This script generates forecasts for today, but production runs shouldn't run
# this directly; instead, they should run forecaster.py, which sets the
# FLU_CACHE to 'production'.

###############################################################################
# SETUP                                                                       #
###############################################################################
geo_type <- 'state'
response_data_source = 'hhs'
response_signal = 'confirmed_admissions_influenza_1d_prop_7dav'
ahead = 4 + 7*(0:3)
ntrain_reference = 21
ntrain_nowindow = 20L * 365L
lags = c(0, 7, 14)
tau = evalcast::covidhub_probs()
states_dc_pr_vi = c('al', 'ak', 'az', 'ar', 'ca', 'co', 'ct', 'dc', 'de', 'fl',
                    'ga', 'hi', 'id', 'il', 'in', 'ia', 'ks', 'ky', 'la', 'me',
                    'md', 'ma', 'mi', 'mn', 'ms', 'mo', 'mt', 'ne', 'nv', 'nh',
                    'nj', 'nm', 'ny', 'nc', 'nd', 'oh', 'ok', 'or', 'pa', 'ri',
                    'sc', 'sd', 'tn', 'tx', 'ut', 'vt', 'va', 'wa', 'wv', 'wi',
                    'wy', 'pr', 'vi')
forecast_date <- as.Date(Sys.getenv("FORECAST_DATE", unset = Sys.Date()))
cache_dir <- Sys.getenv("FLU_CACHE", unset="exploration")
offline_signal_dir <- here::here(paste0("cache/", cache_dir, "/signals"))
forecast_cache_dir <- here::here("cache", cache_dir, "tuesday-forecasts")

if (strftime(forecast_date, '%w') != '2') {
  warning('Forecaster being run on a day that is not a Tuesday. ',
          'The forecaster assumes that it is being run on Tuesday ',
          'in which aheads are predicted.')
}

make_start_day_ar = function(ahead, ntrain, lags) {
  # NOTE: Why eval?
  offset = eval(1 - max(ahead) - ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}
# Use the larger `ntrain` value to make sure we always request and store in
# the cache the full date range for each signal.
start_day_ar = make_start_day_ar(ahead, ntrain_nowindow, lags)

###############################################################################
# LEARN THE QAR MODEL AND SAVE OUTPUT                                         #
###############################################################################
cmu_forecaster_name = "CMU-TimeSeries"

signals_ar = tibble::tibble(
                      data_source = unique(c(response_data_source)),
                      signal = unique(c(response_signal)),
                      start_day = list(start_day_ar),
                      geo_values = list(states_dc_pr_vi),
                      geo_type = geo_type)

production_forecaster_reference = list(
  forecaster = quantgen_forecaster %>%
  make_forecaster_with_prespecified_args(
    signals=signals_ar,
    incidence_period='day',
    ahead=ahead,
    geo_type=geo_type,
    tau=tau,
    n=ntrain_reference,
    lags=lags,
    lambda=0,
    nonneg=TRUE,
    sort=TRUE,
    lp_solver='gurobi'
  ) %>%
    make_named_forecaster("reference"),
  signals=signals_ar
)

production_forecaster_nowindow_latencyfix = list(
  forecaster = quantgen_forecaster %>%
  make_forecaster_account_for_response_latency() %>%
  make_forecaster_with_prespecified_args(
    signals=signals_ar,
    incidence_period='day',
    ahead=ahead,
    geo_type=geo_type,
    tau=tau,
    n=ntrain_nowindow,
    lags=lags,
    lambda=0,
    nonneg=TRUE,
    sort=TRUE,
    lp_solver='gurobi'
  ) %>%
    make_named_forecaster("nowindow_latencyfix"),
  signals=signals_ar
)

ens1 = make_ensemble_forecaster(
  list(production_forecaster_reference, production_forecaster_nowindow_latencyfix),
  offline_signal_dir = offline_signal_dir
) %>% make_caching_forecaster("ens1", forecast_cache_dir)

t0 = Sys.time()
preds_state <- get_predictions(ens1,
                      cmu_forecaster_name,
                      signals_ar,
                      forecast_date,
                      incidence_period='day',
                      forecaster_args=list()
                )
t1 = Sys.time()
print(t1-t0)


if (!dir.exists('data-forecasts/CMU-TimeSeries')) {
  dir.create('data-forecasts/CMU-TimeSeries', recursive = TRUE)
}
readr::write_csv(
  preds_state %>%
    dplyr::mutate(forecast_date = forecast_date - 1),
  sprintf('data-forecasts/CMU-TimeSeries/%s-CMU-TimeSeries-prediction-state.csv', forecast_date - 1),
)

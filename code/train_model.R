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
forecast_dates = Sys.Date()
if (strftime(forecast_dates, '%w') != '1') {
  warning('Forecaster being run on a day that is not a Monday.',
          'The forecaster assumes that it is being run on Monday',
          'in which aheads are predicted'.)
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
idx = 3 # Ultimately we only use model 3
as_of_fd = function(x) {x}
as_of_td = function(x) { lubridate::today() }
signals_df = tribble(
  ~data_source,         ~signal,          ~name,        ~lags,
        ~as_of,
  response_data_source, response_signal,  'AR3',        list(lags),
        list(as_of_fd),
  'chng','smoothed_adj_outpatient_flu',   'AR3+CHNG1',  list(lags, c(0)),
        list(as_of_fd, as_of_td),
  'chng','smoothed_adj_outpatient_flu',   'AR3+CHNG3',  list(lags, lags),
        list(as_of_fd, as_of_td),
)

###############################################################################
# LEARN THE QAR MODEL AND SAVE OUTPUT                                         #
###############################################################################
# TODO:
forecaster_name = sprintf('%s%s_%d', signals_df$name[idx], tt, length(tau))
signals_ar = tibble::tibble(
                      data_source = unique(c(response_data_source,
                                             #TODO:
                                             signals_df$data_source[idx])),
                      signal = unique(c(response_signal,
                                        #TODO:
                                        signals_df$signal[idx])),
                      start_day = list(start_day_ar),
                      geo_values=list(states_dc_pr_vi),
                      #TODO:
                      as_of=signals_df$as_of[[idx]],
                      geo_type=geo_type)
preds <- get_predictions(quantgen_forecaster,
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
                          lags=signals_df$lags[[idx]], # TODO:
                          lambda=0,
                          nonneg=TRUE,
                          sort=TRUE,
                          lp_solver='gurobi', # TODO: Change to GLPK
                          )
                      )
t1 = Sys.time()
print(t1-t0)
saveRDS(preds, sprintf('predictions/preds_%s.RDS', forecaster_name))


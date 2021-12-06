library(furrr)
future::plan(multisession)
library(progressr)
library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
source('quantgen.R')

source('common_params.R')

ntrain = 21
lags = c(0, 7, 14)
tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
debug_dir = './debug_results/'

# To user with future_map, we must evaluate these globals ahead of time

make_start_day_ar = function(ahead, ntrain, lags) {
  offset = eval(1 - max(ahead) - ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}

start_day_ar = make_start_day_ar(ahead, ntrain, lags)
signals_ar = tibble::tibble(
                      data_source = response_data_source,
                      signal = response_signal,
                      start_day = list(start_day_ar),
                      geo_values=list(states_dc_pr_vi),
                      geo_type=geo_type)

t0 = Sys.time()
preds <- get_predictions(quantgen_forecaster,
                      'AR',
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
                          lags=lags,
                          lambda=0,
                          nonneg=TRUE,
                          sort=TRUE,
                          lp_solver='gurobi',
                          debug=sprintf('%s%s', debug_dir, 'AR')
                          )
                      )
t1 = Sys.time()
print(t1-t0)
saveRDS(preds, 'predictions/preds_AR.RDS')

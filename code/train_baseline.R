library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)

source('common_params.R')

ntrain = 28

# To user with future_map, we must evaluate these globals ahead of time
make_start_day_baseline = function(ntrain) {
  offset = eval(1 - ntrain - 4)
  start_day_baseline = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_baseline)
}

start_day_baseline = make_start_day_baseline(ntrain)

signals_baseline = tibble::tibble(
                      data_source = response_data_source, 
                      signal = response_signal,
                      start_day = list(start_day_baseline),
                      geo_values=list(states_dc_pr_vi),
                      as_of = list(function(x) x),
                      geo_type='state')

#offline_signal_dir = sprintf('./data/%s_as_of/', train_type)
t0 = Sys.time()
preds <- get_predictions(baseline_forecaster,
                      'Baseline',
                      signals_baseline,
                      forecast_dates,
                      incidence_period='day',
                      #offline_signal_dir=offline_signal_dir,
                      forecaster_args=list(
                          incidence_period='day',
                          ahead=ahead)
                      )
t1 = Sys.time()
print(t1-t0)
saveRDS(preds, 'predictions/preds_Baseline.RDS')

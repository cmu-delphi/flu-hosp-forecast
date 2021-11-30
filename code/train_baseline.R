library(furrr)
future::plan(multisession)

library(progressr)

library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)

states_dc_pr_vi = c('al', 'ak', 'az', 'ar', 'ca', 'co', 'ct', 'dc', 'de', 'fl',
                    'ga', 'hi', 'id', 'il', 'in', 'ia', 'ks', 'ky', 'la', 'me',
                    'md', 'ma', 'mi', 'mn', 'ms', 'mo', 'mt', 'ne', 'nv', 'nh',
                    'nj', 'nm', 'ny', 'nc', 'nd', 'oh', 'ok', 'or', 'pa', 'ri',
                    'sc', 'sd', 'tn', 'tx', 'ut', 'vt', 'va', 'wa', 'wv', 'wi',
                    'wy', 'pr', 'vi')

geo_type <- "state" 
ntrain = 28
forecast_dates <- seq(as.Date('2021-10-01'),
                      as.Date('2021-10-31'),
                      by = "day")
ahead = 5:28
response_data_source = 'hhs'
response_signal = 'confirmed_admissions_influenza_1d_prop_7dav'


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

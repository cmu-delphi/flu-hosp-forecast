library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
source('quantgen.R')

source('common_params.R')

ntrain = 21
lags = c(0, 7, 14)
tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
tau = evalcast::covidhub_probs()
debug_dir = './debug_results/'

Id = function(x) {x}

# To user with future_map, we must evaluate these globals ahead of time

make_start_day_ar = function(ahead, ntrain, lags) {
  offset = eval(1 - max(ahead) - ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}

start_day_ar = make_start_day_ar(ahead, ntrain, lags)

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

train_type = c('', '_logtrans')

for (tt in train_type) {
  for (idx in 1:nrow(signals_df)) {
    t0 = Sys.time()
    forecaster_name = sprintf('%s%s_%d', signals_df$name[idx], tt, length(tau))
    if (tt == '') {
      transform = Id
      inv_trans = Id
    } else if (tt == '_logtrans') {
      transform = log_pad()
      inv_trans = exp_pad()
    } else {
      stop()
    }
    message(forecaster_name)
    signals_ar = tibble::tibble(
                          data_source = unique(c(response_data_source,
                                                 signals_df$data_source[idx])),
                          signal = unique(c(response_signal,
                                            signals_df$signal[idx])),
                          start_day = list(start_day_ar),
                          geo_values=list(states_dc_pr_vi),
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
                              lags=signals_df$lags[[idx]],
                              lambda=0,
                              nonneg=TRUE,
                              sort=TRUE,
                              lp_solver='gurobi',
                              transform = transform,
                              inv_trans = inv_trans,
                              debug=sprintf('%s%s', debug_dir, forecaster_name)
                              )
                          )
    t1 = Sys.time()
    print(t1-t0)
    saveRDS(preds, sprintf('predictions/preds_%s.RDS', forecaster_name))
  }
}


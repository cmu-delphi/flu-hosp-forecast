library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
source('quantgen.R')

source('common_params.R')

# First date that as_of data is available for Change Healthcare
forecast_dates = forecast_dates[forecast_dates >= '2021-12-06']
ntrain = 21
lags = c(0, 7, 14)
tau = evalcast::covidhub_probs()

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
        list(as_of_fd), '',
  'chng','smoothed_adj_outpatient_flu',   'AR3+CHNG3_Cheating',  list(lags, lags),
        list(as_of_fd, as_of_td), ''
  'chng','smoothed_adj_outpatient_flu',   'AR3+CHNG3_Honest',  list(lags, lags),
        list(as_of_fd, as_of_fd), ''
  'chng','smoothed_adj_outpatient_flu',   'AR3+CHNG3_Honest',  list(lags, lags),
        list(as_of_fd, as_of_fd), '_GLPK'
)

for (idx in 1:nrow(signals_df)) {
  t0 = Sys.time()
  forecaster_name = sprintf('%s%s', signals_df$name[idx], signals_df$solver[idx])
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
  lp_solver = ifelse(signals_df$solver[idx] == '', 'gurobi', 'glpk')
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
                            )
                        )
  t1 = Sys.time()
  print(t1-t0)
  saveRDS(preds, sprintf('predictions_sensitivity_analysis/preds_%s.RDS', forecaster_name))
}


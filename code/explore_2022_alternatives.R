library(dplyr)
library(tibble)
library(evalcast)
library(purrr)
source('quantgen.R')

#### Duplicates a lot of code from other files in this repo as this is meant to
#### be a one-off analysis and it was easier. We may want to restructure things
#### to de-duplicate at some point.

forecast_dates <- seq(
  # a Monday at/after we've had issues for both signals for 56 days:
  as.Date('2022-01-31'),
  # end bound from `make_common_params.py` on 2022-10-17:
  as.Date('2022-09-17'),
  # only Mondays for now:
  by = "week")
stopifnot(all(as.POSIXlt(forecast_dates)$wday == 1L))

# most recent date with settled/finalized as_of data, as of sometime on
# 2022-10-17, is 2022-10-16 (unless there are replication hiccups):
eval_date <- '2022-10-16'

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

make_start_day_ar = function(ahead, ntrain, lags) {
  offset = eval(1 - max(ahead) - ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}
start_day_ar = make_start_day_ar(ahead, ntrain, lags)

idx = 1
auxiliary_signals_df = tribble(
  ~data_source, ~signal,                       ~lags,
  'chng',       'smoothed_adj_outpatient_flu', list(lags, lags),
  )

signals_ar = tibble::tibble(
  data_source = unique(c(response_data_source,
                         auxiliary_signals_df$data_source[idx])),
  signal = unique(c(response_signal,
                    auxiliary_signals_df$signal[idx])),
  start_day = list(start_day_ar),
  geo_values=list(states_dc_pr_vi),
  geo_type=geo_type)

# bettermc::mclapply inside `get_predictions` can make it harder to debug, even
# when using only one core; just use a workaround disable it for now.
invisible(bettermc::mclapply) # make sure bettermc is loaded (but not necessarily attached)
if (!exists("bmc_mclapply_backup")) {
  bmc_mclapply_backup = bettermc::mclapply
  bmc_mclapply_replacement = rlang::new_function(
    rlang::fn_fmls(bettermc::mclapply),
    quote(lapply(X, FUN, ...))
  )
  assignInNamespace(ns="bettermc", "mclapply", bmc_mclapply_replacement)
}

exploration_preds_state =
  get_predictions(quantgen_forecaster %>%
                    make_caching_forecaster("production_forecaster_reference", here::here("cache/forecasts")),
                  "production_forecaster_reference",
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
                    lags=auxiliary_signals_df$lags[[idx]],
                    lambda=0,
                    nonneg=TRUE,
                    sort=TRUE,
                    lp_solver='glpk' # Docker doesn't support Gurobi
                  ),
                  offline_signal_dir = here::here("cache/signals"))

quantgen_forecaster_latencyfix = make_forecaster_account_for_response_latency(quantgen_forecaster)

exploration_preds_state_latencyfix =
  get_predictions(quantgen_forecaster_latencyfix %>%
                    make_caching_forecaster("production_forecaster_latencyfix", here::here("cache/forecasts")),
                  "production_forecaster_latencyfix",
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
                    lags=auxiliary_signals_df$lags[[idx]],
                    lambda=0,
                    nonneg=TRUE,
                    sort=TRUE,
                    lp_solver='glpk' # Docker doesn't support Gurobi
                  ),
                  offline_signal_dir = here::here("cache/signals"))

ntrain_baseline = 28

# To user with future_map, we must evaluate these globals ahead of time
make_start_day_baseline = function(ntrain_baseline) {
  offset = eval(1 - ntrain_baseline - 4)
  start_day_baseline = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_baseline)
}

start_day_baseline = make_start_day_baseline(ntrain_baseline)

signals_baseline = tibble::tibble(
  data_source = response_data_source,
  signal = response_signal,
  start_day = list(start_day_baseline),
  geo_values=list(states_dc_pr_vi),
  geo_type='state')

exploration_preds_state_baseline =
  get_predictions(baseline_forecaster %>%
                    make_caching_forecaster("baseline", here::here("cache/forecasts")),
                  'Baseline',
                  signals_baseline,
                  forecast_dates,
                  incidence_period='day',
                  forecaster_args=list(
                    incidence_period='day',
                    ahead=ahead),
                  offline_signal_dir = here::here("cache/signals"))

eval_snapshot =
  evalcast::download_signal(
    response_data_source,
    response_signal,
    start_day = min(forecast_dates)+min(ahead),
    end_day = max(forecast_dates)+max(ahead),
    geo_type = "state",
    geo_values = "*",
    as_of = eval_date,
    offline_signal_dir = here::here("cache/signals")
  )

eval_actuals = eval_snapshot %>%
  mutate(incidence_period = "day") %>%
  rename(target_end_date = time_value,
         actual = value) %>%
  full_join(tibble(ahead = .env$ahead), by=character(0L)) %>%
  mutate(forecast_date = .data$target_end_date - .data$ahead)

predictions_by_forecaster = list(
  production = exploration_preds_state,
  latencyfix = exploration_preds_state_latencyfix,
  baseline = exploration_preds_state_baseline
)

common_set_evaluations =
  predictions_by_forecaster %>%
  map(function(predictions) {
    evalcast::evaluate_predictions(predictions, eval_actuals)
  }) %>%
  bind_rows() %>%
  group_by(.data$data_source, .data$signal, .data$incidence_period, .data$forecast_date, .data$geo_value, .data$ahead) %>%
  filter(sum(!is.na(.data$wis)) == length(.env$predictions_by_forecaster)) %>%
  ungroup()

common_set_evaluations %>%
  group_by(ahead, forecaster) %>%
  summarize(across(c(wis, ae, coverage_80), mean)) %>%
  arrange(ahead, wis)

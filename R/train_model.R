library(covidcast)
library(dplyr)
library(evalcast)
library(magrittr)
library(tibble)

source(here::here("R", "quantgen.R"))
source(here::here("R", "ensemble.R"))
source(here::here("R", "utils.R"))


get_quantile_predictions <- function(
    forecast_due_date,
    reference_date,
    horizons) {
  geo_type <- "state"
  response_data_source <- "hhs"
  response_signal <- "confirmed_admissions_influenza_1d_prop_7dav"
  forecast_target_dates <- reference_date + 7 * horizons
  ahead <- as.integer(forecast_target_dates - forecast_due_date)
  ntrain_shortwindow <- 21
  ntrain_nowindow <- 20L * 365L
  lags <- c(0, 7, 14)
  tau <- c(0.01, .025, 1:19 / 20, .975, .99)
  states_dc_pr_vi <- c(
    "al", "ak", "az", "ar", "ca", "co", "ct", "dc", "de", "fl",
    "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me",
    "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh",
    "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri",
    "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi",
    "wy", "pr", "vi"
  )
  offline_signal_dir <- here::here("cache", "evalcast")
  forecast_cache_dir <- here::here("cache", "forecasters")
  n_core <- parallel::detectCores() - 1

  ###############################################################################
  # LEARN THE QAR MODEL AND SAVE OUTPUT                                         #
  ###############################################################################
  cmu_forecaster_name <- "CMU-TimeSeries"

  signals_ar <- tibble::tibble(
    data_source = unique(c(response_data_source)),
    signal = unique(c(response_signal)),
    start_day = as.Date("2021-12-04"),
    geo_values = list(states_dc_pr_vi),
    geo_type = geo_type
  )

  production_forecaster_shortwindow_latencyfix <- list(
    forecaster = quantgen_forecaster %>%
      make_forecaster_account_for_response_latency() %>%
      make_latency_enforced_forecaster(
        min_latency_to_enforce = 4
      ) %>%
      make_forecaster_with_prespecified_args(
        signals = signals_ar,
        incidence_period = "day",
        ahead = ahead,
        geo_type = geo_type,
        tau = tau,
        n = ntrain_shortwindow,
        lags = lags,
        lambda = 0,
        nonneg = TRUE,
        sort = TRUE,
        lp_solver = "gurobi",
        n_core = n_core
      ) %>%
      make_named_forecaster("shortwindow_latencyfix"),
    signals = signals_ar
  )

  production_forecaster_nowindow_latencyfix <- list(
    forecaster = quantgen_forecaster %>%
      make_forecaster_account_for_response_latency() %>%
      make_latency_enforced_forecaster(
        min_latency_to_enforce = 4
      ) %>%
      make_forecaster_with_prespecified_args(
        signals = signals_ar,
        incidence_period = "day",
        ahead = ahead,
        geo_type = geo_type,
        tau = tau,
        n = ntrain_nowindow,
        lags = lags,
        lambda = 0,
        nonneg = TRUE,
        sort = TRUE,
        lp_solver = "gurobi",
        n_core = n_core
      ) %>%
      make_named_forecaster("nowindow_latencyfix"),
    signals = signals_ar
  )

  ens1 <- make_ensemble_forecaster(
    list(
      production_forecaster_shortwindow_latencyfix,
      production_forecaster_nowindow_latencyfix
    ),
    offline_signal_dir = offline_signal_dir
  ) %>% make_caching_forecaster("ens1", forecast_cache_dir)

  t0 <- Sys.time()
  preds_state <- get_predictions(ens1,
    cmu_forecaster_name,
    signals_ar,
    forecast_due_date,
    incidence_period = "day",
    forecaster_args = list()
  )
  t1 <- Sys.time()
  print(t1 - t0)

  preds_state %<>% mutate(
    forecast_date = reference_date,
    # Address unlikely rounding issues in quantile values.
    quantile = as.character(signif(quantile, 4))
  ) %>%
    get_postprocessed_forecasts()

  return(preds_state)
}

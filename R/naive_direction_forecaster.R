library(checkmate)
library(dplyr)
library(epidatr)
library(epiprocess)
library(readr)
library(rlang)
library(tibble)
library(tidyr)
source(here::here("R", "approx-cdf.R"))
source(here::here("R", "utils.R"))


get_direction_predictions <- function(
    forecast_due_date,
    reference_date,
    horizons,
    exclude_geos,
    quantile_predictions) {
  # Load state population data
  augmented_location_data <- get_flusight_location_data()
  state_pop <- get_state_data()

  forecast_target_dates <- reference_date + 7 * horizons
  ahead <- as.integer(forecast_target_dates - forecast_due_date)

  cache_dir <- Sys.getenv("FLU_CACHE", unset = "exploration")
  forecaster_cached_output <- here::here(
    "cache",
    cache_dir,
    "tuesday-forecasts",
    "ens1",
    paste0(forecast_due_date, ".RDS")
  )

  # We need to combine the quantile forecasts with recent observations in order to
  # do direction calculations; fetch that data now:
  short_snapshot <- bind_rows(
    epidatr::pub_covidcast(
      "hhs",
      "confirmed_admissions_influenza_1d",
      "state",
      "day",
      "*",
      epirange(forecast_due_date - 20L, forecast_due_date),
      # TODO: Workaround for https://github.com/cmu-delphi/epidatr/issues/194
      as_of = strftime(forecast_due_date, "%Y-%m-%d")
    ),
    epidatr::pub_covidcast(
      "hhs",
      "confirmed_admissions_influenza_1d",
      "nation",
      "day",
      "*",
      epirange(forecast_due_date - 20L, forecast_due_date),
      # TODO: Workaround for https://github.com/cmu-delphi/epidatr/issues/194
      as_of = strftime(forecast_due_date, "%Y-%m-%d")
    ) %>% as_tibble()
  )

  # Ideally, we want to compare forecast 7dsum for Saturday 2 weeks ahead to the
  # 7dsum for the Saturday directly preceding the forecast date. However, we might
  # have larger data latency and not have data for the preceding Saturday, in
  # which case we back off to the most recent 7dsum we have available, and just
  # accept the mismatch.
  reference_7d_counts <- short_snapshot %>%
    # reference by forecast Wednesday - 4L = Saturday, else whatever is soonest before then
    filter(time_value <= forecast_due_date - 4L) %>%
    group_by(geo_value) %>%
    complete(time_value = full_seq(time_value, 1L)) %>%
    slice_max(time_value, n = 7L) %>%
    summarize(
      reference_7dcount = sum(value),
      time_value = max(time_value),
      .groups = "drop"
    )

  # rule of three for probability of "novel behavior"; mixing weight for uniform
  uniform_forecaster_weight <- 3 / (
    # roughly, how many years of hhs influenza are available
    as.numeric(forecast_due_date - as.Date("2020-10-16")) / (365 + 1 / 4 - 1 / 100 + 1 / 400) *
      # say there's one wave/season per year
      1 *
      # say states from separate HHS Regions are "separate" enough to count as different data points, while those within the same region aren't separate enough; 10 HHS Regions = 10 buckets
      10
  )

  # TODO:
  # - Augmented location data has an extra column "...5". The schema must've
  # changed. Need to fix.
  # - Understand the columns in the schema. They have different names. Do they
  # mean different things?
  # - reference_7d_counts is only meant for 2 weeks ahead. We need to generalize
  # this to all horizons.

  browser()
  direction_predictions <- quantile_predictions %>%
    group_by(
      forecaster,
      data_source,
      signal,
      geo_value,
      incidence_period,
      reference_date,
      target,
      horizon,
      target_end_date,
      location
    ) %>%
    summarize(
      forecast = list(approx_cdf_from_quantiles(value, output_type_id)),
      .groups = "keep"
    ) %>%
    left_join(augmented_location_data %>% select(-geo_value), by = "location") %>%
    left_join(reference_7d_counts, by = "geo_value") %>%
    reframe(
      output_type = "pmf",
      output_type_id = as.factor(c("large_decrease", "decrease", "stable", "increase", "large_increase")),
      value = {
        stopifnot(length(forecast) == 1L)
        p_large_dec <- p_le(forecast[[1L]], reference_7dcount - large_change_count_thresh)
        p_large_or_nonlarge_dec <- p_le(forecast[[1L]], reference_7dcount - nonlarge_change_count_thresh)
        p_large_or_nonlarge_inc <- p_ge(forecast[[1L]], reference_7dcount + nonlarge_change_count_thresh)
        p_large_inc <- p_ge(forecast[[1L]], reference_7dcount + large_change_count_thresh)
        result <- c(
          p_large_dec,
          p_large_or_nonlarge_dec - p_large_dec,
          1 - p_large_or_nonlarge_dec - p_large_or_nonlarge_inc,
          p_large_or_nonlarge_inc - p_large_inc,
          p_large_inc
        )
        result
      }
    ) %>%
    group_by(reference_date, location) %>%
    # mix with uniform
    mutate(
      value = (1 - uniform_forecaster_weight) * value + uniform_forecaster_weight * 1 / n()
    ) %>%
    ungroup()

  # Sanity check that the output probabilities sum to 1.
  stopifnot(
    all(abs(1 -
      direction_predictions %>%
      group_by(forecast_due_date, target, location) %>%
      summarize(value = sum(value)) %>%
      pull(value)) < 1e-8)
  )

  return(direction_predictions)
}

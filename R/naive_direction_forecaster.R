# Naive direction forecaster.
#
# This forecaster tries to convert predictions for the FluSight quantile targets
# into predictions for the categorical trends targets in a revision-unaware way.
# The directions are obtained as follows:
#
#  - For each state, we compute the 7-day sum of the most recent Sat-Fri run of
#    data available and call that the reference count.
#  - We compare the quantile forecasts of the 7-day sum Sat-Fri with the
#    reference count. For each geo and horizon, we calculate the probability
#    that the forecast is in each of the following categories:
#
#    - large decrease: 2 * thresh below the reference count
#    - decrease: thresh below the reference count
#    - stable: not in any of the categories above or below
#    - increase: thresh above the reference count
#    - large increase: 2 * thresh above the reference count
#
#   where thresh is the increase count threshold for the given horizon and geo.
#   These thresholds are defined in R/utils.R in get_flusight_location_data().
#   Looking at the previous year's data for the same thresholds, we saw that the
#   thresholds for count_rate1per100k and count_rate1per100k were related by a
#   factor of 2, so we use the same factor of 2 here.
#

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
    quantile_predictions) {
  augmented_location_data <- get_flusight_location_data()
  state_pop <- get_state_data()

  # We need to combine the quantile forecasts with recent observations in order to
  # do direction calculations.
  # TODO: Using a workaround for https://github.com/cmu-delphi/epidatr/issues/194
  short_snapshot <- bind_rows(
    epidatr::pub_covidcast(
      "hhs",
      "confirmed_admissions_influenza_1d",
      "state",
      "day",
      "*",
      epirange(forecast_due_date - 20L, forecast_due_date),
      as_of = strftime(forecast_due_date, "%Y-%m-%d")
    ),
    epidatr::pub_covidcast(
      "hhs",
      "confirmed_admissions_influenza_1d",
      "nation",
      "day",
      "*",
      epirange(forecast_due_date - 20L, forecast_due_date),
      as_of = strftime(forecast_due_date, "%Y-%m-%d")
    )
  )

  # By the cadence of HHS data, we expect to have "official"/non-"preliminary"
  # versions of data up to Friday 12 days back from the forecast due date. We
  # get the latest 7 day sum of the "official" data available.
  reference_7d_counts <- short_snapshot %>%
    filter(time_value <= forecast_due_date - 12L) %>%
    group_by(geo_value) %>%
    complete(time_value = full_seq(time_value, 1L)) %>%
    slice_max(time_value, n = 7L) %>%
    summarize(
      reference_7dcount = sum(value),
      time_value = max(time_value),
      .groups = "drop"
    )

  # Rule of three for probability of "novel behavior"; mixing weight for uniform
  uniform_forecaster_weight <- 3 / (
    # roughly, how many years of hhs influenza are available
    as.numeric(forecast_due_date - as.Date("2020-10-16")) / (365 + 1 / 4 - 1 / 100 + 1 / 400) *
      # say there's one wave/season per year
      1 *
      # say states from separate HHS Regions are "separate" enough to count as different data points, while those within the same region aren't separate enough; 10 HHS Regions = 10 buckets
      10
  )

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
      forecast_acdf = list(approx_cdf_from_quantiles(value, output_type_id)),
      .groups = "keep"
    ) %>%
    left_join(augmented_location_data %>% select(-geo_value), by = "location") %>%
    left_join(reference_7d_counts, by = "geo_value") %>%
    reframe(
      output_type = "pmf",
      output_type_id = c("large_decrease", "decrease", "stable", "increase", "large_increase"),
      value = {
        stopifnot(length(forecast_acdf) == 1L)
        if (horizon == -1) {
          thresh <- increase_count_1_thresh
        } else if (horizon == 0) {
          thresh <- increase_count_2_thresh
        } else if (horizon == 1) {
          thresh <- increase_count_3_thresh
        } else if (horizon == 2) {
          thresh <- increase_count_4_thresh
        } else if (horizon == 3) {
          thresh <- increase_count_5_thresh
        } else {
          stop("horizon must be in -1:3")
        }
        p_large_dec <- p_le(forecast_acdf[[1L]], reference_7dcount - 2 * thresh)
        p_large_or_nonlarge_dec <- p_le(forecast_acdf[[1L]], reference_7dcount - thresh)
        p_large_or_nonlarge_inc <- p_ge(forecast_acdf[[1L]], reference_7dcount + thresh)
        p_large_inc <- p_ge(forecast_acdf[[1L]], reference_7dcount + 2 * thresh)
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
    group_by(reference_date, horizon, location) %>%
    # mix with uniform
    mutate(
      value = (1 - uniform_forecaster_weight) * value + uniform_forecaster_weight * 1 / n()
    ) %>%
    ungroup()

  # Sanity check that the output probabilities sum to 1.
  stopifnot(
    all(abs(1 -
      direction_predictions %>%
      group_by(reference_date, horizon, location) %>%
      summarize(value = sum(value)) %>%
      pull(value)) < 1e-8)
  )

  return(direction_predictions)
}

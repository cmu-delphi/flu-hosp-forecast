# Naive direction forecaster.
#
# This forecaster converts predictions for the FluSight quantile targets into
# predictions for the categorical trends targets in a revision-unaware way. The
# directions are obtained as follows:
#
#  - For each state, we compute the 7-day sum of the most recent Sat-Fri run of
#    data available and call that the reference count.
#  - We compare the quantile forecasts of the 7-day sum Sat-Fri with the
#    reference count. For each geo and horizon, we calculate the probability
#    that the forecast is in each of the following categories:
#
#    - large decrease: P(forecast <= reference count - large_thresh)
#    - decrease: P(reference count - large_thresh < forecast <= reference count
#      - stable_thresh)
#    - stable: P(reference count - stable_thresh < forecast < reference count +
#      stable_thresh)
#    - increase: P(reference count + stable_thresh <= forecast < reference count
#      + large_thresh)
#    - large increase: P(reference count + large_thresh <= forecast)
#
#   where large_thresh and stable_thresh are thresholds based on horizon and geo
#   population. These thresholds are defined in R/utils.R in
#   get_flusight_location_data() and in the FluSight guidelines document
#   Appendix 1.
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
    quantile_predictions,
    enforced_latency,
    data_1d_override = NULL) {
  augmented_location_data <- get_flusight_location_data()
  state_pop <- get_state_data()

  if (is.null(data_1d_override)) {
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
  } else {
    stopifnot(all(c(tolower(state.abb),"dc","pr") %in% data_1d_override$geo_value))
    stopifnot("us" %in% data_1d_override$geo_value)
    short_snapshot <- data_1d_override %>%
      filter(forecast_due_date - 20L <= time_value,
             time_value <= forecast_due_date)
  }

  # By the cadence of HHS data, we expect to have "official"/non-"preliminary"
  # versions of data up to Friday 4 days back from the forecast due date (this
  # is in enforced_latency). We get the latest Sat-Fri sum of the "official"
  # data available.
  if (max(short_snapshot$time_value) < forecast_due_date - enforced_latency) {
    cli::cli_abort("We should have time values with latency equal to (or less than but that's not expected either) the enforced latency.  Did the API or other upstream source not get an update yet?")
  }
  reference_7d_counts <- short_snapshot %>%
    filter(time_value <= forecast_due_date - enforced_latency) %>%
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
      # say states from separate HHS Regions are "separate" enough to count as
      # different data points, while those within the same region aren't
      # separate enough; 10 HHS Regions = 10 buckets
      10
  )

  direction_predictions <- quantile_predictions %>%
    filter(horizon != -1) %>%
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
      forecast_acdf = list(approx_cdf_from_quantiles(value, parse_number(output_type_id))),
      .groups = "keep"
    ) %>%
    left_join(augmented_location_data %>% select(-geo_value), by = "location") %>%
    left_join(reference_7d_counts, by = "geo_value") %>%
    reframe(
      target = "wk flu hosp rate change",
      output_type = "pmf",
      output_type_id = c("large_decrease", "decrease", "stable", "increase", "large_increase"),
      value = {
        stopifnot(length(forecast_acdf) == 1L)
        if (horizon == 0) {
          large_thresh <- increase_count_2_thresh
          stable_thresh <- increase_count_1_thresh
        } else if (horizon == 1) {
          large_thresh <- increase_count_3_thresh
          stable_thresh <- increase_count_1_thresh
        } else if (horizon == 2) {
          large_thresh <- increase_count_4_thresh
          stable_thresh <- increase_count_2_thresh
        }
        else if (horizon == 3) {
          large_thresh <- increase_count_5_thresh
          stable_thresh <- increase_count_2p5_thresh
        } else {
          stop("horizon must be in 0:3")
        }
        p_large_dec <- p_le(forecast_acdf[[1L]], reference_7dcount - large_thresh)
        p_large_or_nonlarge_dec <- p_le(forecast_acdf[[1L]], reference_7dcount - stable_thresh)
        p_large_or_nonlarge_inc <- p_ge(forecast_acdf[[1L]], reference_7dcount + stable_thresh)
        p_large_inc <- p_ge(forecast_acdf[[1L]], reference_7dcount + large_thresh)
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
    all(
      direction_predictions %>%
        group_by(reference_date, horizon, location, geo_value) %>%
        summarize(value = abs(1 - sum(value)) < 1e-8) %>%
        # Ignore NAs here.
        filter(!is.na(value)) %>%
        pull(value)
    )
  )

  return(direction_predictions)
}

library(checkmate)
library(dplyr)
library(epidatr)
library(epiprocess)
library(readr)
library(rlang)
library(tibble)
library(tidyr)

source(here::here("code", "R", "approx-cdf.R"))
source(here::here("code", "R", "utils.R"))


# The code below is from postprocess_forecasts.R
INCIDENCE_RATE <- 100000
# NOTE: VI data has been just zeroes for a long time, so we exclude it.
# Add extra states to this list if needed.
# They will be included in the national forecast, but not in the state-level forecast.
exclude_geos <- tolower(c("vi"))
# NOTE: While we make predictions on Tuesday, we want to label the file with a Monday, hence the -1's below.

# Load state population data
state_pop <- readr::read_csv(here::here("code", "state_pop.csv"), show_col_types = FALSE) %>%
  rename(
    geo_value = state_id,
  ) %>%
  select(
    -state_name,
  )

get_preds_full <- function(preds_state) {
  preds_state$quantile <- signif(preds_state$quantile, 4) # Eliminate rounding issues

  # Transform to weekly incidence counts (not prop)
  preds_state_processed <- preds_state %>%
    inner_join(
      state_pop,
      by = "geo_value",
    ) %>%
    transmute(
      geo_value = geo_value,
      ahead = ahead,
      forecaster = "flu-model",
      data_source = "hhs",
      signal = "confirmed_admissions_influenza_1d_7dav",
      incidence_period = "day",
      forecast_date = forecast_date,
      target = sprintf("%d wk ahead inc flu hosp", (ahead - 4) / 7 + 1),
      target_end_date = target_end_date,
      location = state_code,
      type = "quantile",
      quantile = quantile,
      value = value * pop / INCIDENCE_RATE * 7,
    )

  # US-level forecasts
  preds_us_unsorted <- preds_state_processed %>%
    group_by(
      forecast_date,
      target,
      target_end_date,
      type,
      quantile,
    ) %>%
    summarize(
      location = "US",
      value = sum(value),
    ) %>%
    ungroup()
  preds_us_list <- preds_us_unsorted %>% group_split(
    forecast_date,
    target,
    target_end_date,
    type,
  )
  for (idx in 1:length(preds_us_list)) {
    preds_us_list[[idx]]$quantile <- sort(preds_us_list[[idx]]$quantile)
    preds_us_list[[idx]]$value <- sort(preds_us_list[[idx]]$value)
  }
  preds_us <- bind_rows(preds_us_list)
  preds_us$data_source <- "hhs"
  preds_us$signal <- "confirmed_admissions_influenza_1d_7dav"
  preds_us$forecaster <- "flu-model"
  preds_us$incidence_period <- "day"
  preds_us$geo_value <- "us"

  preds_full <- bind_rows(preds_state_processed, preds_us) %>%
    arrange(location) %>%
    filter(.data$geo_value %in% exclude_geos == FALSE)

  return(preds_full)
}

augmented_location_data <- get_flusight_location_data()

# These locations will not be evaluated, and I believe that they do not want
# submissions for these locations. (And there may not be the threshold/any data
# for them in the location data above.)
# exclude_geos is set in postprocess_forecasts.R
nonevaluated_geo_values <- c(c("as", "gu", "mp", "vi"), exclude_geos)
nonevaluated_locations <- c("60", "66", "69", "78")

today <- Sys.Date()
forecast_date <- as.Date(Sys.getenv("FORECAST_DATE", unset = Sys.Date()))
if (as.POSIXlt(forecast_date)$wday != 2L) {
  warning("This script was designed to be run on a Tuesday, but `today` is not a Tuesday.")
}

# Set the `nominal_forecast_date` and the `forecast_as_of_date`. The
# `nominal_forecast_date` determines what the output files should be named and
# what the forecast target(_end_date)s are. The `forecast_as_of_date` determines
# (through) what `as_of` we can use to prepare the forecast.
#
# Ideally, we would warn / stop to confirm instead of stop. But we might need to
# check that this script does something reasonable. But this might be a moot
# point if we're moving to production because we'd also need to tinker with the
# "exploration" script to make things run on non-Tuesdays here.
nominal_forecast_date <- forecast_date - 1L
if (as.POSIXlt(nominal_forecast_date)$wday != 1L) {
  warning("The `nominal_forecast_date` should be a Monday.")
}
forecast_as_of_date <- nominal_forecast_date + 1L
if (as.POSIXlt(forecast_as_of_date)$wday != 2L) {
  warning("The `forecast_as_of_date` should be a Tuesday.")
}

# Also record the forecast generation date (extra metadata / to make sure we
# don't clobber things if we want to compare real-time vs. as-of).
forecast_generation_date <- today

cache_dir <- Sys.getenv("FLU_CACHE", unset = "exploration")
forecaster_cached_output <- here::here("cache", cache_dir, "tuesday-forecasts", "ens1", paste0(forecast_as_of_date, ".RDS"))

# XXX We should move this script to production and just use the production
# forecasters here. Maybe call a caching forecaster or read a saved file.
preds_state_prop_7dav <- readRDS(forecaster_cached_output) %>%
  {
    out <- .
    # Reproduce evalcast post-processing because we're working around it reading
    # directly from the cache file. (Not sure why it was done this way; maybe
    # because we did not have a setups file defining all the forecasters as
    # caching forecasters.)
    assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
      msg = paste(
        "Your forecaster must return a data frame with",
        "(at least) the columnns `ahead`, `geo_value`,",
        "`quantile`, and `value`."
      )
    )
    out$forecast_date <- nominal_forecast_date
    names(out$value) <- NULL
    out <- out %>%
      mutate(
        forecaster = "CMU-TimeSeries",
        data_source = "hhs",
        signal = "hhs_confirmed_admissions_influenza_1d_prop_7dav",
        target_end_date = evalcast:::get_target_period(
          .data$forecast_date,
          "day",
          .data$ahead
        )$end,
        incidence_period = "day"
      ) %>%
      relocate(forecaster, .before = forecast_date)
    class(out) <- c("predictions_cards", class(out))
    out
  }

# TODO verify whether this is indeed identical to one of the `preds_full`s in
# `train_model.R` / to the result of writing `train_model.R`'s `preds_full` then
# reading it back in with `read_csv`.
preds_full <- get_preds_full(preds_state_prop_7dav)

# We need to combine the quantile forecasts with recent observations in order to
# do direction calculations; fetch that data now:
short_snapshot <-
  bind_rows(
    evalcast::download_signal(
      "hhs", "confirmed_admissions_influenza_1d",
      nominal_forecast_date - 20L, nominal_forecast_date,
      "state", "*",
      as_of = forecast_as_of_date,
      offline_signal_dir = here::here("cache", "short_signals_for_direction")
    ) %>% as_tibble(),
    evalcast::download_signal(
      "hhs", "confirmed_admissions_influenza_1d",
      nominal_forecast_date - 20L, nominal_forecast_date,
      "nation", "*",
      as_of = forecast_as_of_date,
      offline_signal_dir = here::here("cache", "short_signals_for_direction")
    ) %>% as_tibble()
  )

# Ideally, we want to compare forecast 7dsum for Saturday 2 weeks ahead to the
# 7dsum for the Saturday directly preceding the forecast date. However, we might
# have larger data latency and not have data for the preceding Saturday, in
# which case we back off to the most recent 7dsum we have available, and just
# accept the mismatch.
reference_7d_counts <-
  short_snapshot %>%
  # reference by forecast Monday - 2L = Saturday, else whatever is soonest before then
  filter(time_value <= nominal_forecast_date - 2L) %>%
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
  as.numeric(forecast_as_of_date - as.Date("2020-10-16")) / (365 + 1 / 4 - 1 / 100 + 1 / 400) *
    # say there's one wave/season per year
    1 *
    # say states from separate HHS Regions are "separate" enough to count as different data points, while those within the same region aren't separate enough; 10 HHS Regions = 10 buckets
    10
)

unfiltered_direction_predictions <-
  preds_full %>%
  # filter(! geo_value %in% .env$nonevaluated_geo_values) %>%
  # filter(ahead == 7L*2L - 2L) %>%
  # group_by(geo_value) %>%
  filter(!location %in% .env$nonevaluated_locations) %>%
  filter(target == "2 wk ahead inc flu hosp") %>%
  group_by(forecast_date, location) %>%
  summarize(
    forecast = list(approx_cdf_from_quantiles(value, quantile)),
    .groups = "keep"
  ) %>%
  # left_join(reference_7d_counts, by="geo_value") %>%
  # left_join(augmented_location_data, by="geo_value") %>%
  left_join(augmented_location_data, by = "location") %>%
  left_join(reference_7d_counts, by = "geo_value") %>%
  summarize(
    type = "category",
    type_id = c("large_decrease", "decrease", "stable", "increase", "large_increase") %>%
      {
        factor(., levels = .)
      },
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
    },
    .groups = "keep"
  ) %>%
  # mix with uniform
  mutate(
    value =
      (1 - uniform_forecaster_weight) * value +
        uniform_forecaster_weight * 1 / n()
  ) %>%
  ungroup() %>%
  mutate(target = "2 wk flu hosp rate change") %>%
  select(forecast_date, target, location, type, type_id, value)

direction_predictions_dir <- here::here("code", "data-forecasts", "direction-predictions", paste0("generated-", forecast_generation_date))
if (!dir.exists(direction_predictions_dir)) {
  dir.create(direction_predictions_dir, recursive = TRUE)
}

write_csv(
  unfiltered_direction_predictions,
  file.path(direction_predictions_dir, paste0("prefilter-", nominal_forecast_date, "-CMU-TimeSeries.csv")),
  # quote='all' is important to make sure the location column is quoted.
  quote = "all"
)

excluded_locations <-
  c(
    # for now, always exclude VI (as, at time of last check, it was all zeros
    # for months) (--- note that this exclusion should actually be redundant, as
    # VI is a nonevaluated location):
    augmented_location_data %>%
      filter(geo_value == "vi") %>%
      pull(location),
    # additional set of locations to exclude this run ("week-to-week"
    # exclusions for this week):
    augmented_location_data %>%
      filter(geo_value %in% rlang::chr()) %>%
      pull(location)
  )

filtered_direction_predictions <- unfiltered_direction_predictions %>%
  filter(!location %in% excluded_locations)

# Sanity check that the output probabilities sum to 1.
stopifnot(
  all(abs(1 -
    filtered_direction_predictions %>%
    group_by(forecast_date, target, location) %>%
    summarize(value = sum(value)) %>%
    pull(value)) < 1e-8)
)

write_csv(
  filtered_direction_predictions,
  file.path(direction_predictions_dir, paste0(nominal_forecast_date, "-CMU-TimeSeries.csv")),
  # quote='all' is important to make sure the location column is quoted.
  quote = "all"
)

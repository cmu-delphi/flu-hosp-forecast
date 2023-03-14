
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)
library(readr)
library(checkmate)

library(epidatr)
library(epiprocess)
devtools::load_all(here::here("code","direction.forecaster"), export_all=FALSE)

source(here::here("code","approx-cdf.R"))
source(here::here("code","postprocess_forecasts.R"))

augmented_location_data = fetch_updating_resource(
  function() {
    read_csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv",
             col_types = cols(
               abbreviation = col_character(),
               location = col_character(),
               location_name = col_character(),
               population = col_integer(),
               count_rate1per100k = col_integer(),
               count_rate2per100k = col_integer()
             ))
  },
  function(response) {
    assert_tibble(response)
  },
  here::here("cache","location_data")
) %>%
  mutate(large_change_count_thresh = pmax(count_rate2per100k, 40L),
         nonlarge_change_count_thresh = pmax(count_rate1per100k, 20L),
         geo_type = dplyr::if_else(location == "US", "nation", "state"),
         geo_value = dplyr::if_else(location == "US", "us", tolower(covidcast::fips_to_abbr(location))))

# These locations will not be evaluated, and I believe that they do not want
# submissions for these locations. (And there may not be the threshold/any data
# for them in the location data above.)
nonevaluated_geo_values = c("as","gu","mp","vi")
nonevaluated_locations = c("60","66","69","78")

today = Sys.Date()
if (as.POSIXlt(today)$wday != 2L) {
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
nominal_forecast_date = today - 1L
stopifnot(as.POSIXlt(nominal_forecast_date)$wday == 1L) # Monday
forecast_as_of_date = nominal_forecast_date + 1L
stopifnot(as.POSIXlt(forecast_as_of_date)$wday == 2L) # Tuesday

# Also record the forecast generation date (extra metadata / to make sure we
# don't clobber things if we want to compare real-time vs. as-of).
forecast_generation_date = today

cache_dir <- Sys.getenv("FLU_CACHE", unset="exploration")
forecaster_cached_output <- here::here("cache", cache_dir, "tuesday-forecasts", "ens1", paste0(forecast_as_of_date, ".RDS"))

# XXX We should move this script to production and just use the production
# forecasters here. Maybe call a caching forecaster or read a saved file.
preds_state_prop_7dav = readRDS(forecaster_cached_output) %>%
  {
    out = .
    # Reproduce evalcast post-processing because we're working around it reading
    # directly from the cache file. (Not sure why it was done this way; maybe
    # because we did not have a setups file defining all the forecasters as
    # caching forecasters.)
    assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
                msg = paste("Your forecaster must return a data frame with",
                            "(at least) the columnns `ahead`, `geo_value`,",
                            "`quantile`, and `value`."))
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
          .data$ahead)$end,
        incidence_period = "day"
      ) %>%
      relocate(forecaster, .before = forecast_date)
    class(out) <- c("predictions_cards", class(out))
    out
  }

# TODO verify whether this is indeed identical to one of the `preds_full`s in
# `train_model.R` / to the result of writing `train_model.R`'s `preds_full` then
# reading it back in with `read_csv`.
preds_full = get_preds_full(preds_state_prop_7dav)

# We need to combine the quantile forecasts with recent observations in order to
# do direction calculations; fetch that data now:
short_snapshot =
  bind_rows(
    evalcast::download_signal(
      "hhs", "confirmed_admissions_influenza_1d",
      nominal_forecast_date-20L, nominal_forecast_date,
      "state", "*",
      as_of = forecast_as_of_date,
      offline_signal_dir = here::here("cache","short_signals_for_direction")
    ) %>% as_tibble(),
    evalcast::download_signal(
      "hhs", "confirmed_admissions_influenza_1d",
      nominal_forecast_date-20L, nominal_forecast_date,
      "nation", "*",
      as_of = forecast_as_of_date,
      offline_signal_dir = here::here("cache","short_signals_for_direction")
    ) %>% as_tibble()
  )

# Ideally, we want to compare forecast 7dsum for Saturday 2 weeks ahead to the
# 7dsum for the Saturday directly preceding the forecast date. However, we might
# have larger data latency and not have data for the preceding Saturday, in
# which case we back off to the most recent 7dsum we have available, and just
# accept the mismatch.
reference_7d_counts =
  short_snapshot %>%
  # reference by forecast Monday - 2L = Saturday, else whatever is soonest before then
  filter(time_value <= nominal_forecast_date - 2L) %>%
  group_by(geo_value) %>%
  complete(time_value = full_seq(time_value, 1L)) %>%
  slice_max(time_value, n=7L) %>%
  summarize(reference_7dcount = sum(value),
            time_value = max(time_value),
            .groups="drop")

# rule of three for probability of "novel behavior"; mixing weight for uniform
uniform_forecaster_weight = 3/(
  # roughly, how many years of hhs influenza are available
  as.numeric(forecast_as_of_date - as.Date("2020-10-16"))/(365+1/4-1/100+1/400) *
    # say there's one wave/season per year
    1 *
    # say states from separate HHS Regions are "separate" enough to count as different data points, while those within the same region aren't separate enough; 10 HHS Regions = 10 buckets
   10
)

unfiltered_direction_predictions =
  preds_full %>%
  # filter(! geo_value %in% .env$nonevaluated_geo_values) %>%
  # filter(ahead == 7L*2L - 2L) %>%
  # group_by(geo_value) %>%
  filter(! location %in% .env$nonevaluated_locations) %>%
  filter(target == "2 wk ahead inc flu hosp") %>%
  group_by(forecast_date, location) %>%
  summarize(
    forecast = list(approx_cdf_from_quantiles(value, quantile)),
    .groups="keep"
  ) %>%
  # left_join(reference_7d_counts, by="geo_value") %>%
  # left_join(augmented_location_data, by="geo_value") %>%
  left_join(augmented_location_data, by="location") %>%
  left_join(reference_7d_counts, by="geo_value") %>%
  summarize(
    type = "category",
    type_id = c("large_decrease", "decrease", "stable", "increase", "large_increase") %>% {factor(., levels=.)},
    value =
      {
        stopifnot(length(forecast) == 1L)
        p_large_dec = p_le(forecast[[1L]], reference_7dcount - large_change_count_thresh)
        p_large_or_nonlarge_dec = p_le(forecast[[1L]], reference_7dcount - nonlarge_change_count_thresh)
        p_large_or_nonlarge_inc = p_ge(forecast[[1L]], reference_7dcount + nonlarge_change_count_thresh)
        p_large_inc = p_ge(forecast[[1L]], reference_7dcount + large_change_count_thresh)
        result = c(p_large_dec,
          p_large_or_nonlarge_dec - p_large_dec,
          1 - p_large_or_nonlarge_dec - p_large_or_nonlarge_inc,
          p_large_or_nonlarge_inc - p_large_inc,
          p_large_inc)
        result
      },
    .groups="keep"
  ) %>%
  # mix with uniform
  mutate(value =
           (1-uniform_forecaster_weight) * value +
           uniform_forecaster_weight * 1/n()) %>%
  ungroup() %>%
  mutate(target = "2 wk flu hosp rate change") %>%
  select(forecast_date, target, location, type, type_id, value)

direction_predictions_dir = here::here("code","data-forecasts","direction-predictions",paste0("generated-",forecast_generation_date,"-as-of-",forecast_as_of_date))
if (!dir.exists(direction_predictions_dir)) {
  dir.create(direction_predictions_dir, recursive=TRUE)
}

write_csv(
  unfiltered_direction_predictions,
  file.path(direction_predictions_dir, paste0("prefilter-",nominal_forecast_date,"-CMU-TimeSeries.csv")),
  # quote='all' is important to make sure the location column is quoted.
  quote="all"
)

warn("WARNING: Did you remember to update the geo exclusions?")
excluded_locations =
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
# TODO validate week-to-week exclusions aren't misspelled

filtered_direction_predictions = unfiltered_direction_predictions %>%
  filter(! location %in% excluded_locations)

# Sanity check that the output probabilities sum to 1.
stopifnot(
  all(abs(1 -
            filtered_direction_predictions %>%
            group_by(forecast_date, target, location) %>%
            summarize(value = sum(value)) %>%
            pull(value)
          ) < 1e-8)
)

write_csv(
  filtered_direction_predictions,
  file.path(direction_predictions_dir, paste0(nominal_forecast_date,"-CMU-TimeSeries.csv")),
  # quote='all' is important to make sure the location column is quoted.
  quote="all"
)

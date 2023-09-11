# retrospective_naive_direction_forecaster.R
#
# This script generates retrospective direction predictions using our forecast
# submissions from the Flusight repo:
#
#   https://github.com/cdcepi/Flusight-forecast-data/tree/master/data-forecasts/CMU-TimeSeries
#
# There are no inputs to this script, instead there is a hard-coded list of
# forecast dates to be generated at the top. The output is a set of CSV files
# with the path format
#
#   "data-forecasts/direction-predictions/generated-{today}-as-of-{actual_forecast_date}/{nominal_forecast_date}-CMU-TimeSeries.csv".
#
# See the comments around make_retrospective_forecast() for details about the
# forecast dates. The CSV files have the format, which is the submission format
# requested by the CDC:
#
#   "forecast_date","target","location","type","type_id","value"
#
# This script was made by modifying the naive_direction_forecaster.R script.
# It's a hacky one-off script and should probably not be used for future work.
# Some of its changes may be worth porting back to naive_direction_forecaster.R
# at some point.
#
library(checkmate)
library(dplyr)
library(glue)
library(magrittr)
library(readr)
library(rlang)
library(stringr)
library(tibble)
library(tidyr)

library(epidatr)
library(epiprocess)
devtools::load_all(here::here("code", "direction.forecaster"), export_all = FALSE)

source(here::here("code", "approx-cdf.R"))


# These are the dates for which we will produce retrospective forecasts.
retrospective_forecast_dates <- seq(as.Date("2022-10-17"), as.Date("2022-12-19"), by = "week")

# get_preds_full was extracted from postprocess_forecasts.R and modified.
incidence_rate <- 100000

# Will need this to convert from state_code (01) to state_id (al)
state_pop <- readr::read_csv(here::here("code", "state_pop.csv"), show_col_types = FALSE) %>%
  rename(geo_value = state_id) %>%
  select(-state_name)

# These locations will not be evaluated, and I believe that they do not want
# submissions for these locations. (And there may not be the threshold/any data
# for them in the location data above.)
nonevaluated_geo_values <- c("as", "gu", "mp", "vi")
nonevaluated_locations <- state_pop %>%
  filter(geo_value %in% nonevaluated_geo_values) %>%
  pull(state_code)

augmented_location_data <- fetch_updating_resource(
  function() {
    read_csv(
      glue::glue("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv"),
      col_types = cols(
        abbreviation = col_character(),
        location = col_character(),
        location_name = col_character(),
        population = col_integer(),
        count_rate1per100k = col_integer(),
        count_rate2per100k = col_integer()
      )
    )
  },
  function(response) {
    assert_tibble(response)
  },
  here::here("cache", "location_data")
) %>%
  mutate(
    large_change_count_thresh = pmax(count_rate2per100k, 40L),
    nonlarge_change_count_thresh = pmax(count_rate1per100k, 20L),
    geo_type = dplyr::if_else(location == "US", "nation", "state"),
    geo_value = dplyr::if_else(location == "US", "us", tolower(covidcast::fips_to_abbr(location)))
  )

get_preds_full2 <- function(preds_state, exclude_geos = nonevaluated_geo_values) {
  # Eliminate rounding issues
  preds_state$quantile <- signif(preds_state$quantile, 4)

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
      value = value,
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
  for (idx in seq_along(preds_us_list)) {
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


# Get predictions from Flusight repo and format slightly. The resulting tibble
# has the columns: geo_value, value, ahead, quantile
get_flu_predictions <- function(forecast_date) {
  forecasts_web <- read_csv(
    glue::glue(
      "https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-forecasts/CMU-TimeSeries/",
      "{forecast_date}-CMU-TimeSeries.csv"
    )
  ) %>%
    # Convert "x wk ahead inc flu hosp" to "x" and then to "x*7+4"
    mutate(ahead = (as.double((str_replace(target, " wk ahead inc flu hosp", ""))) - 1) * 7 + 4) %>%
    inner_join(state_pop, by = c("location" = "state_code")) %>%
    # Find the "US"-marked locations and give them the state_id "us"
    select(c("geo_value", "value", "ahead", "quantile"))
  forecasts_web
}


# The forecasts were generated on Tuesdays for Mondays, so the
# actual_forecast_date is the day after the nominal_forecast_date. (We change
# this from the way it is handled in postprocess_forecasts.R so that the dates
# used here match the dates in the Flusight submission folder.)
make_retrospective_forecast <- function(nominal_forecast_date) {
  assert_that(inherits(nominal_forecast_date, c("Date", "POSIXt")), msg = "blah")

  # We started forecasting on Tuesday at some point, it's not clear when,
  # so to be safe, we changed actual and nominal to match.
  # actual_forecast_date <- nominal_forecast_date + 1L
  actual_forecast_date <- nominal_forecast_date
  forecaster_cached_output <- get_flu_predictions(nominal_forecast_date)
  preds_state <- forecaster_cached_output %>%
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

  preds_full <- get_preds_full2(preds_state)

  # We need to combine the quantile forecasts with recent observations in order to
  # do direction calculations; fetch that data now:
  short_snapshot <-
    bind_rows(
      evalcast::download_signal(
        "hhs", "confirmed_admissions_influenza_1d",
        nominal_forecast_date - 20L, nominal_forecast_date,
        "state", "*",
        as_of = actual_forecast_date,
        offline_signal_dir = here::here("cache", "short_signals_for_direction")
      ) %>% as_tibble(),
      evalcast::download_signal(
        "hhs", "confirmed_admissions_influenza_1d",
        nominal_forecast_date - 20L, nominal_forecast_date,
        "nation", "*",
        as_of = actual_forecast_date,
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
    # filter(time_value <= nominal_forecast_date - 2L) %>%
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
    as.numeric(actual_forecast_date - as.Date("2020-10-16")) / (365 + 1 / 4 - 1 / 100 + 1 / 400) *
      # say there's one wave/season per year
      1 *
      # say states from separate HHS Regions are "separate" enough to count as
      # different data points, while those within the same region aren't
      # separate enough; 10 HHS Regions = 10 buckets
      10
  )

  unfiltered_direction_predictions <-
    preds_full %>%
    filter(!location %in% .env$nonevaluated_locations) %>%
    filter(target == "2 wk ahead inc flu hosp") %>%
    group_by(forecast_date, location) %>%
    summarize(
      forecast = list(approx_cdf_from_quantiles(value, quantile)),
      .groups = "keep"
    ) %>%
    left_join(augmented_location_data, by = "location") %>%
    left_join(reference_7d_counts, by = "geo_value") %>%
    reframe(
      type = "category",
      type_id = as.factor(c("large_decrease", "decrease", "stable", "increase", "large_increase")),
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
    group_by(forecast_date, location) %>%
    # mix with uniform
    mutate(
      value = (1 - uniform_forecaster_weight) * value + uniform_forecaster_weight * 1 / n()
    ) %>%
    ungroup() %>%
    mutate(target = "2 wk flu hosp rate change") %>%
    select(forecast_date, target, location, type, type_id, value)

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
    all(
      abs(
        1 - filtered_direction_predictions %>%
          group_by(forecast_date, target, location) %>%
          summarize(value = sum(value)) %>%
          pull(value)
      ) < 1e-8
    )
  )

  dir_path <- here::here(
    "code",
    "data-forecasts",
    "direction-predictions",
    glue("generated-{Sys.Date()}-as-of-{actual_forecast_date}")
  )
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

  write_csv(
    filtered_direction_predictions,
    file.path(dir_path, glue("{nominal_forecast_date}-CMU-TimeSeries.csv")),
    # quote='all' is important to make sure the location column is quoted.
    quote = "all"
  )
}

map(retrospective_forecast_dates, make_retrospective_forecast)

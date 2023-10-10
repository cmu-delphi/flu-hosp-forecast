#' Returns the next instance of a given weekday. If that is today, return today.
#' Weekday 0 is a Sunday.
get_next_weekday <- function(date, wday) {
  return(as.Date(date) + (wday - lubridate::wday(date)) %% 7)
}

#' Returns the previous instance of a given weekday. If that is today, return today.
#' Weekday 0 is a Sunday.
get_previous_weekday <- function(date, wday) {
  return(as.Date(date) - (7 - (wday - lubridate::wday(date)) %% 7))
}

#' Uploads or downloads a folder to the s3bucket; direction can be c("upload", "download")
manage_forecast_cache <- function(
    rel_cache_dir,
    bucket_name = "forecasting-team-data",
    direction = "download",
    verbose = FALSE) {
  cache_path <- here::here(rel_cache_dir)
  if (!dir.exists(cache_path)) dir.create(cache_path)

  s3b <- aws.s3::get_bucket(bucket_name)
  if (verbose) {
    aws.s3::s3sync(
      cache_path,
      s3b,
      paste0("covid-hosp-forecast/", rel_cache_dir),
      direction = direction
    )
  } else {
    sink("/dev/null")
    aws.s3::s3sync(
      cache_path, s3b,
      paste0("covid-hosp-forecast/", rel_cache_dir),
      direction = direction,
      verbose = FALSE
    )
    sink()
  }
  return(TRUE)
}

#' get_postprocessed_forecasts
#'
#' We postprocess by:
#' - adapting the quantile forecasts from a 7-day average of population scaled
#'   data to a 7-day sum of incidence counts
#' - converting columns to the format required for submission
#' - aggregating to US-level forecasts
#' - correcting for non-crossing quantiles
#'
get_postprocessed_forecasts <- function(preds_state) {
  incidence_rate <- 100000
  state_pop <- get_state_data()

  # Transform to weekly incidence counts
  preds_state_processed <- preds_state %>%
    inner_join(
      state_pop,
      by = "geo_value",
    ) %>%
    transmute(
      geo_value = geo_value,
      # Everything below is required for submission.
      reference_date = forecast_date,
      target = "wk inc flu hosp",
      horizon = (target_end_date - forecast_date) / 7,
      target_end_date = target_end_date,
      location = state_code,
      output_type = "quantile",
      output_type_id = quantile,
      value = value * pop / incidence_rate * 7,
    ) %>%
    {
      assert_integerish(as.numeric(.$horizon, units = "days"))
      .$horizon <- as.numeric(.$horizon, units = "days")
      .
    }

  # Aggregate to US-level forecasts
  preds_us_unsorted <- preds_state_processed %>%
    group_by(
      reference_date,
      target,
      horizon,
      target_end_date,
      output_type,
      output_type_id
    ) %>%
    summarize(
      location = "US",
      value = sum(value),
    ) %>%
    ungroup()
  # Non-crossing correction for quantiles (i.e. make sure value increases with
  # quantile)
  preds_us_list <- preds_us_unsorted %>% group_split(
    reference_date,
    target_end_date,
    output_type,
  )
  for (idx in 1:length(preds_us_list)) {
    preds_us_list[[idx]]$output_type_id <- sort(preds_us_list[[idx]]$output_type_id)
    preds_us_list[[idx]]$value <- sort(preds_us_list[[idx]]$value)
  }
  preds_us <- bind_rows(preds_us_list) %>%
    mutate(
      geo_value = "us",
    )

  preds_full <- bind_rows(preds_state_processed, preds_us) %>%
    mutate(
      # Extra metadata for dev purposes.
      data_source = "hhs",
      signal = "confirmed_admissions_influenza_1d_7dav",
      forecaster = "flu-model",
      incidence_period = "day",
    ) %>%
    arrange(location, reference_date, target, horizon, output_type, output_type_id)
}

#' Location and population data as specified in the 2023-2024 season repo.
get_flusight_location_data <- function() {
  read_csv(
    "https://raw.githubusercontent.com/cmu-delphi/FluSight-forecast-hub/main/auxiliary-data/locations.csv",
    col_types = cols(
      abbreviation = col_character(),
      location = col_character(),
      location_name = col_character(),
      population = col_integer(),
      count_rate1 = col_integer(),
      count_rate2 = col_integer(),
      count_rate2p5 = col_integer(),
      count_rate3 = col_integer(),
      count_rate4 = col_integer(),
      count_rate5 = col_integer(),
    ),
    col_select = c(
      abbreviation,
      location,
      location_name,
      population,
      count_rate1,
      count_rate2,
      count_rate2p5,
      count_rate3,
      count_rate4,
      count_rate5,
    )
  ) %>%
    mutate(
      increase_count_1_thresh = pmax(count_rate1, 10),
      increase_count_2_thresh = pmax(count_rate2, 10),
      increase_count_2p5_thresh = pmax(count_rate2p5, 10),
      increase_count_3_thresh = pmax(count_rate3, 10),
      increase_count_4_thresh = pmax(count_rate4, 10),
      increase_count_5_thresh = pmax(count_rate5, 10),
      geo_type = dplyr::if_else(location == "US", "nation", "state"),
      geo_value = dplyr::if_else(
        location == "US",
        "us", tolower(covidcast::fips_to_abbr(location))
      )
    )
}

#' Get state abbreviation to state code to state population map from
#' delphi_utils.geo_mapper
#'
#' There are slight differences with the population data provided by the CDC.
#' Only 8 states have an absolute relative population difference bigger than
#' 3%. But since our signal used prop based on the file above, we should use it
#' to invert.
#'
#' inner_join(
#'   state_pop,
#'   augmented_location_data %>% mutate(abbreviation = tolower(abbreviation)),
#'   by = c("geo_value" = "abbreviation")
#' ) %>%
#'   mutate(
#'     rel_pop_diff = abs(pop - population) / population
#'   ) %>%
#'   select(geo_value, rel_pop_diff) %>%
#'   filter(rel_pop_diff > 0.03)
#'
get_state_data <- function() {
  readr::read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/main/_delphi_utils_python/delphi_utils/data/2020/state_pop.csv", show_col_types = FALSE) %>%
    rename(geo_value = state_id) %>%
    select(-state_name)
}

#' make_latency_enforced_forecaster
#'
#' This function takes a forecaster and returns a forecaster that enforces a
#' minimum latency. This allows us to do backcasting on training data that did
#' not have latency as severe as what we have in 2023.
make_latency_enforced_forecaster <- function(forecaster, min_latency_to_enforce, ...) {
  function(df_list, forecast_date, ...) {
    forecaster(
      df_list %>%
        map(~ .x %>% filter(time_value <= forecast_date - min_latency_to_enforce)),
      forecast_date,
      ...
    )
  }
}

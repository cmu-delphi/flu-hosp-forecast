#' Returns the next instance of a given weekday. If that is today, return today.
#' Weekday 0 is a Saturday.
get_next_weekday <- function(date, wday) {
  return(as.Date(date) + (wday - lubridate::wday(date)) %% 7)
}

#' Returns the previous instance of a given weekday. If that is today, return
#' today. Weekday 0 is a Saturday.
get_previous_weekday <- function(date, wday) {
  return(as.Date(date) + (((wday - lubridate::wday(date)) %% 7) - 7) + 7 * (wday == lubridate::wday(date)))
}

#' Syncs with the s3bucket based on timestamps. The paste0 command below needs
#' to have the slash at the end or else s3sync will mangle the directory
#' structure.
manage_forecast_cache <- function(rel_cache_dir, bucket_name = "forecasting-team-data", verbose = FALSE) {
  cache_path <- here::here(rel_cache_dir)
  if (!dir.exists(cache_path)) dir.create(cache_path)

  s3b <- aws.s3::get_bucket(bucket_name)
  if (verbose) {
    aws.s3::s3sync(cache_path, s3b, paste0("flu-hosp-forecast/", rel_cache_dir, "/"))
  } else {
    sink("/dev/null")
    aws.s3::s3sync(cache_path, s3b, paste0("flu-hosp-forecast/", rel_cache_dir, "/"), verbose = FALSE)
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
#' minimum latency. This allows us to do backtesting on training data that did
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

#' get_health_data
#'
#' This function gets the health data directly from healthdata.gov (bypassing
#' some errors in Delphi pipelines). First, we consult the metadata store, which
#' allows us to find the most recent update date to the as_of date provided.
#' Finding that points us to the versioned data in AWS bucket, which we
#' download. Then we transform the data for consistency with the Delphi Epidata
#' API (transforming it from counts to a ).
#'
#' Metadata archive link: https://healthdata.gov/dataset/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/qqte-vkut
#' Healthdata archive link: https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh
#'
get_health_data <- function(as_of) {
  checkmate::assert_date(as_of, min.len = 1, max.len = 1)

  cache_path <- here::here("cache", "healthdata")
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }

  metadata_path <- here::here(cache_path, "metadata.csv")
  if (!file.exists(metadata_path)) {
    meta_data <- readr::read_csv("https://healthdata.gov/resource/qqte-vkut.csv?$query=SELECT%20update_date%2C%20days_since_update%2C%20user%2C%20rows%2C%20row_change%2C%20columns%2C%20column_change%2C%20metadata_published%2C%20metadata_updates%2C%20column_level_metadata%2C%20column_level_metadata_updates%2C%20archive_link%20ORDER%20BY%20update_date%20DESC%20LIMIT%2010000")
    readr::write_csv(meta_data, metadata_path)
  } else {
    meta_data <- readr::read_csv(metadata_path)
  }

  most_recent_row <- meta_data %>%
    filter(update_date <= as_of + 1) %>%
    arrange(desc(update_date)) %>%
    slice(1)

  if (nrow(most_recent_row) == 0) {
    cli::cli_abort("No data available for the given date.")
  }

  data_filepath <- here::here(cache_path, sprintf("g62h-syeh-%s.csv", as.Date(most_recent_row$update_date)))
  if (!file.exists(data_filepath)) {
    data <- readr::read_csv(most_recent_row$archive_link)
    readr::write_csv(data, data_filepath)
  } else {
    data <- readr::read_csv(data_filepath)
  }

  data %>%
    # Minor data adjustments and column renames. The date also needs to be dated
    # back one, since the columns we use report previous day hospitalizations.
    transmute(
      geo_value = tolower(state),
      time_value = date - 1L,
      value = previous_day_admission_influenza_confirmed
    ) %>%
    # API seems to complete state level with 0s in some cases rather than NAs.
    # Get something sort of compatible with that by summing to national with
    # na.omit = TRUE. As otherwise we have some NAs from probably territories
    # propagated to US level.
    bind_rows(
      (.) %>%
        group_by(time_value) %>%
        summarize(geo_value = "us", value = sum(value, na.rm = TRUE))
    )
}

process_healthdata <- function(data) {
  data %>%
    # Make sure all the time_values are represented.
    group_by(geo_value) %>%
    tidyr::complete(time_value = tidyr::full_seq(time_value, period = 1L)) %>%
    ungroup() %>%
    # Convert to epidataframe. Then apply a rolling 7-day sum to the data.
    as_epi_df() %>%
    group_by(geo_value) %>%
    epi_slide(
      before = 6L,
      ~ if (nrow(.x) == 7L) sum(.x$value) else NA_real_
    ) %>%
    ungroup() %>%
    # Convert back to tibble so we can use left_join. Then join with state
    # population information. Finally, change slide_value to a proportion of the
    # population and remove the slide_value column.
    as_tibble() %>%
    left_join(
      state_census %>%
        transmute(
          geo_value = abbr,
          pop = pop
        ),
      by = c("geo_value")
    ) %>%
    mutate(
      value = slide_value / 7 / pop * 100e3,
      slide_value = NULL
    ) %>%
    select(-pop)
}

make_forecaster_use_data_window <- function(forecaster, window) {
  function(df_list, forecast_date, ...) {
    dots <- list(...)
    offset <- 1 - max(dots$ahead) - max(dots$lags) - window
    forecaster(
      df_list %>%
        map(~ .x %>% filter(time_value >= forecast_date + offset)),
      forecast_date,
      ...
    )
  }
}
### Ported from covid-hosp-forecast on 2024-01-24

#' This is a replacement for evalcast::get_predictions.
get_predictions_new <- function(
    df_list,
    forecaster,
    name_of_forecaster,
    forecast_date,
    response_data_source = "hhs",
    response_data_signal = "confirmed_admissions_influenza_1d_prop_7dav",
    incidence_period = "day",
    forecaster_args = list()) {
  out <- rlang::inject(
    forecaster(
      df_list = df_list,
      forecast_date = forecast_date,
      !!!forecaster_args
    )
  ) %>%
    mutate(
      forecaster = name_of_forecaster,
      forecast_date = forecast_date,
      data_source = response_data_source,
      signal = response_data_signal,
      target_end_date = forecast_date + ahead,
      incidence_period = incidence_period,
    )
  class(out) <- c("predictions_cards", class(out))
  out
}

#' Wrapper around epidatr::pub_covidcast that simplifies the input and formats
#' the output.
get_data <- function(source, signal, start_date, forecast_date) {
  epidatr::pub_covidcast(
    source,
    signal,
    "state",
    "day",
    "*",
    epidatr::epirange(start_date, forecast_date),
    as_of = strftime(forecast_date, format = "%Y-%m-%d")
  ) %>%
    {
      if (nrow(.) == 0) {
        stop(sprintf(
          "No data found for source %s, signal %s, start_date %s, forecast_date %s",
          source, signal, start_date, forecast_date
        ))
      }
      .
    } %>%
    rename(
      "data_source" = "source",
    ) %>%
    select(-direction) %>%
    covidcast::as.covidcast_signal(signal)
}

make_forecaster_filter_geos <- function(forecaster, include_geos) {
  function(df_list, forecast_date, ...) {
    forecaster(
      df_list %>%
        map(~ .x %>% filter(geo_value %in% include_geos)),
      forecast_date,
      ...
    )
  }
}

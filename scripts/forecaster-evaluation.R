  library(dotenv)
  library(dplyr)
  library(epidatr)
  library(magrittr)
  library(hubValidations)

  source(here::here("R", "train_model.R"))
  source(here::here("R", "naive_direction_forecaster.R"))


make_forecasts <- function(
  forecast_generation_date,
  forecast_cache_dir = here::here("cache", "forecaster")
  ) {
  checkmate::assert_date(forecast_generation_date)

  ##### Set parameters.
  forecast_due_date <- as.Date(Sys.getenv(
    "FORECAST_DUE_DATE",
    unset = get_next_weekday(forecast_generation_date, 4)
  ))
  cdc_reference_date <- get_next_weekday(forecast_due_date, 0)
  delphi_reference_date <- cdc_reference_date - 1L
  horizons <- 0:3 # no -1 as it may break the forecaster
  exclude_geos <- tolower(c(
    c("as", "gu", "mp", "vi"),
    c()
  ))
  enforced_latency <- 5L # Friday of preceding week, calculated from `forecast_due_date` (expected to be a Wednesday)


  patch_around_delayed_api_data <- TRUE
  if (patch_around_delayed_api_data) {
    converted_healthdata_1d <- get_health_data()

    converted_healthdata_prop_7dav <-
      converted_healthdata_1d %>%
      filter(geo_value != "us") %>%
      group_by(geo_value) %>%
      tidyr::complete(time_value = tidyr::full_seq(time_value, period = 1L)) %>%
      ungroup() %>%
      as_epi_df() %>%
      group_by(geo_value) %>%
      epi_slide(
        before = 6L,
        ~ if (nrow(.x) == 7L) sum(.x$value) else NA_real_
      ) %>%
      ungroup() %>%
      as_tibble() %>%
      left_join(
        state_census %>%
          transmute(
            geo_value = tolower(ABBR),
            pop = POPESTIMATE2019
          ),
        by = c("geo_value")
      ) %>%
      mutate(
        value = slide_value,
        slide_value = NULL
      ) %>%
      mutate(value = value / 7 / pop * 100e3) %>%
      select(-pop)

    df_list <- list(
      converted_healthdata_prop_7dav %>% covidcast::as.covidcast_signal("confirmed_admissions_influenza_1d_prop_7dav")
    )

  } else {
    converted_healthdata_1d <- NULL
    df_list <- list(
      get_data(
        "hhs",
        "confirmed_admissions_influenza_1d_prop_7dav",
        as.Date("2021-12-04"),
        forecast_generation_date
      ) %>% covidcast::as.covidcast_signal("confirmed_admissions_influenza_1d_prop_7dav")
    )
  }

  if (as.POSIXlt(forecast_due_date)$wday != 3L) {
    cli::cli_alert_warning("forecast_due_date is expected to be a Wednesday, but it's not")
    Sys.sleep(3)
  }

  output_dir <- here::here("data-forecasts")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  ##### Cache setup.
  if (as.logical(Sys.getenv("FLU_HOSP_CLEAR_CACHE", unset = FALSE))) {
    # Clear epidatr cache.
    if (!is.null(epidatr:::cache_environ$epidatr_cache)) {
      epidatr::clear_cache(confirm = FALSE)
    }
    # Clear evalcast cache.
    if (dir.exists(here::here("cache", "evalcast"))) {
      fs::dir_delete(here::here("cache", "evalcast"))
    }
    # Clear forecaster cache.
    if (file.exists(here::here("cache", "forecaster", "ens1", sprintf("%s.RDS", forecast_due_date)))) {
      fs::file_delete(here::here("cache", "forecaster", "ens1", sprintf("%s.RDS", forecast_due_date)))
    }
  }

  ##### Make quantile forecasts.
  quantile_predictions <- get_quantile_predictions(
    df_list,
    forecast_due_date,
    delphi_reference_date,
    horizons,
    enforced_latency,
    forecast_cache_dir = forecast_cache_dir
  )

  ##### Make direction forecasts.
  direction_predictions <- get_direction_predictions(
    forecast_due_date,
    delphi_reference_date,
    quantile_predictions,
    enforced_latency,
    data_1d_override = converted_healthdata_1d
  )

  ##### Combine and write the submissions file.
  combined_predictions <- bind_rows(
    quantile_predictions,
    direction_predictions
  ) %>%
    mutate(
      # Fix the reference date and the corresponding target dates to conform with
      # the CDC's definition.
      reference_date = reference_date + 1L,
      target_end_date = target_end_date + 1L,
    ) %>%
    arrange(location, reference_date, target, horizon, output_type, output_type_id)
  unfiltered_csv_path <- fs::path(
    output_dir,
    sprintf("%s-CMU-TimeSeries-unfiltered.csv", cdc_reference_date)
  )
  write_csv(
    combined_predictions,
    unfiltered_csv_path,
    # quote='all' makes sure the location column is quoted.
    quote = "all"
  )

  combined_predictions
}


forecast_dates <- seq.Date(
  as.Date("2021-01-01"),
  Sys.Date(),
  by = "week"
)
purrr::walk(
  forecast_dates,
  function(forecast_generation_date) {
    make_forecasts(forecast_generation_date)
  }
)


# TODO: Evaluate.
evaluation_data <-

#' Get production forecasts
#'
#' This function generates the production forecasts for the CDC's COVID-19 Forecast Hub.
#' It handles the data downloading, processing, forecast generation (both quantile and direction), and
get_production_forecasts <- function(
    forecast_generation_date,
    forecast_due_date,
    delphi_reference_date,
    horizons,
    enforced_latency) {
  # Healthdata workaround
  converted_healthdata_1d <- get_health_data(forecast_generation_date)
  converted_healthdata_prop_7dav <- converted_healthdata_1d %>% process_healthdata()

  if (!exists("download_signal_bu")) {
    download_signal_bu <- evalcast::download_signal
  }
  unlockBinding("download_signal", asNamespace("evalcast"))
  assignInNamespace(
    ns = asNamespace("evalcast"),
    "download_signal",
    function(data_source, signal, start_day = NULL, end_day = NULL, geo_type = "county", geo_values = "*", as_of = NULL, time_type = "day", offline_signal_dir = NULL, ...) {
      stopifnot(identical(data_source, "hhs"))
      stopifnot(identical(signal, "confirmed_admissions_influenza_1d_prop_7dav"))
      print(as_of)
      stopifnot(identical(as_of, NULL) || identical(as_of, forecast_generation_date))
      stopifnot(identical(time_type, "day"))
      # don't care about offline_signal_dir
      rlang::check_dots_empty()
      result_tbl <-
        if (identical(geo_type, "state")) {
          converted_healthdata_prop_7dav %>%
            filter(
              if (identical(geo_values, "*")) {
                geo_value != "us"
              } else {
                geo_value %in% geo_values
              },
              start_day <= time_value,
              time_value <= end_day
            )
        } else if (identical(geo_type, "nation")) {
          converted_healthdata_prop_7dav %>%
            filter(
              if (identical(geo_values, "*")) {
                geo_value == "us"
              } else {
                geo_value %in% geo_values
              },
              start_day <= time_value,
              time_value <= end_day
            )
        } else {
          cli::cli_abort("Can't handle geo_type of {geo_type}")
        }
      # convert to covidcast signal the lazy way, by stealing from the API result:
      evalcast_result <- download_signal_bu(data_source, signal, start_day, end_day, geo_type, geo_values, as_of, time_type, offline_signal_dir, ...)
      result_ccs <- evalcast_result %>%
        filter(FALSE) %>%
        bind_rows(result_tbl)
      return(result_ccs)
    }
  )

  if (as.POSIXlt(forecast_generation_date)$wday != 3L) {
    cli::cli_alert_warning("forecast_due_date is expected to be a Wednesday, but it's not")
    Sys.sleep(3)
  }

  ##### Make quantile forecasts.
  quantile_predictions <- get_quantile_predictions(
    forecast_generation_date,
    delphi_reference_date,
    horizons,
    enforced_latency
  )

  ##### Make direction forecasts.
  direction_predictions <- get_direction_predictions(
    forecast_generation_date,
    delphi_reference_date,
    quantile_predictions,
    enforced_latency,
    data_1d_override = converted_healthdata_1d
  )

  ##### Combine the forecasts.
  combined_predictions <- bind_rows(
    quantile_predictions,
    direction_predictions
  )

  return(combined_predictions)
}

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

  df_list <- list(
    converted_healthdata_prop_7dav %>% covidcast::as.covidcast_signal("confirmed_admissions_influenza_1d_prop_7dav")
  )

  if (as.POSIXlt(forecast_generation_date)$wday != 3L) {
    cli::cli_alert_warning("forecast_due_date is expected to be a Wednesday, but it's not")
    Sys.sleep(3)
  }

  ##### Make quantile forecasts.
  quantile_predictions <- get_quantile_predictions(
    df_list,
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

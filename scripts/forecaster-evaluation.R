# This script will generate forecasts for every Wednesday starting with the
# first Wednesday of 2021 and ending in February 2024. The forecasts will be
# cached in the `cache/forecaster` directory, which will allow the evaluation
# to pick up where it left off if it is interrupted. The actual forecasts will
# be written to the `data-forecasts` directory as CSV files named
# `YYYY-MM-DD-CMU-TimeSeries-unfiltered.csv`.
#
# The body of the script is taken from `run.R` and simplified.
source(here::here("R", "load_all.R"))


make_forecasts <- function(
    forecast_generation_date,
    forecast_cache_dir = here::here("cache", "forecaster")) {
  checkmate::assert_date(forecast_generation_date)
  forecast_due_date <- get_next_weekday(forecast_generation_date, 4)
  delphi_reference_date <- get_next_weekday(forecast_due_date, 0) - 1L

  combined_predictions <- get_production_forecasts(
    forecast_generation_date,
    forecast_due_date,
    delphi_reference_date,
    horizons = 0:3,
    enforced_latency = 5L,
  ) %>%
    # Fix the reference date and the corresponding target dates to conform with
    # the CDC's definition. Only needed if you really care about reproducing the
    # CDC's forecasts.
    # mutate(
    #   reference_date = reference_date + 1L,
    #   target_end_date = target_end_date + 1L,
    # ) %>%
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

# Make a forecast every Wednesday starting with the first Wednesday of 2021 and to February 2024.
forecast_dates <- seq.Date(get_next_weekday(as.Date("2021-01-01"), 4), as.Date("2024-03-01"), by = "week")
purrr::walk(
  forecast_dates,
  function(forecast_generation_date) {
    make_forecasts(forecast_generation_date)
  }
)

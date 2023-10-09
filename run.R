# Production run script.
#
# Will produce the quantile and direction forecasts for the current week (unless
# parameters are set otherwise.)
#
# The forecast generation date is usually today. The due date is the date of the
# forecast, which should be the most recent next Wednesday (it is also the
# effective as of date for requesting API data). The CDC defines the reference
# date to be the Saturday after the due date; this refers to unshifted data from
# "previous day hospital admissions" data provided by NHSN, which Delphi has
# already shifted. Therefore Delphi and CDC reference dates are off by one. The
# horizons are the number of weeks ahead to forecast, relative to the reference
# date.
#
# To exclude a state from the forecast, add its two-letter state code to the
# exclude_geos vector below. "as", "gu", "mp", and "vi" are excluded by default
# due to lack of data.
#
# Outputs:
#
#  - submission file:
#    data-forecasts/CMU-TimeSeries/YYYY-MM-DD-CMU-TimeSeries.csv
#  - ensemble file:
#    data-forecasts/CMU-TimeSeries/generated-%s-reference-date-%s-CMU-TimeSeries-quantile-predictions.csv
#  - notebook file:
#    data-forecasts/CMU-TimeSeries/YYYY-MM-DD-flu-forecast.html
#  - cache files in cache/
#

library(dplyr)
library(magrittr)

source(here::here("R", "train_model.R"))
source(here::here("R", "naive_direction_forecaster.R"))


epidatr::set_cache(here::here("cache", "epidatr"), confirm = FALSE)

##### Set parameters.
forecast_generation_date <- as.Date(Sys.getenv(
  "FORECAST_GENERATION_DATE",
  unset = Sys.Date()
))
forecast_due_date <- as.Date(Sys.getenv(
  "FORECAST_DUE_DATE",
  unset = get_next_weekday(forecast_generation_date, 4)
))
cdc_reference_date <- get_next_weekday(forecast_due_date, 0)
delphi_reference_date <- cdc_reference_date - 1L
horizons <- -1:3
exclude_geos <- tolower(c(
  c("as", "gu", "mp", "vi"),
  c()
))

if (as.POSIXlt(forecast_due_date)$wday != 3L) {
  cli::cli_alert_warning("forecast_due_date is expected to be a Wednesday, but it's not")
  Sys.sleep(3)
}

output_dir <- here::here("data-forecasts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

##### Make quantile forecasts.
quantile_predictions <- get_quantile_predictions(
  forecast_due_date,
  delphi_reference_date,
  horizons
)

##### Make direction forecasts.
direction_predictions <- get_direction_predictions(
  forecast_due_date,
  delphi_reference_date,
  quantile_predictions
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

write_csv(
  combined_predictions,
  fs::path(
    output_dir,
    sprintf("%s-CMU-TimeSeries-unfiltered.csv", cdc_reference_date)
  ),
  # quote='all' makes sure the location column is quoted.
  quote = "all"
)

##### Filter geos and columns.
keeps <- c(
  "reference_date",
  "target",
  "horizon",
  "target_end_date",
  "location",
  "output_type",
  "output_type_id",
  "value"
)
filtered_combined_predictions <- combined_predictions %>%
  filter(!geo_value %in% exclude_geos) %>%
  select(all_of(keeps))

write_csv(
  filtered_combined_predictions,
  fs::path(
    output_dir,
    sprintf("%s-CMU-TimeSeries.csv", cdc_reference_date)
  ),
  # quote='all' makes sure the location column is quoted.
  quote = "all"
)

##### Render notebook.
rmarkdown::render(
  fs::path("scripts", "plot_prediction_cards.Rmd"),
  output_file = fs::path(
    output_dir,
    sprintf("%s-flu-forecast.html", cdc_reference_date)
  ),
  params = list(
    exclude_geos = exclude_geos,
    predictions_file = fs::path(
      output_dir,
      sprintf("%s-CMU-TimeSeries-unfiltered.csv", cdc_reference_date)
    )
  )
)

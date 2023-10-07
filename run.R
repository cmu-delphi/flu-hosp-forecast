# Production run script.
#
# Will produce the quantile and direction forecasts for the current week (unless
# parameters are set otherwise.)
#
# The forecast generation date is usually today. The due date is the date of the
# forecast, which should be a Wednesday (it is also the effective as of date for
# requesting API data). The reference date is the Saturday after the due date.
# The horizons are the number of weeks ahead to forecast, relative to the
# reference date.
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
  unset = get_previous_weekday(forecast_generation_date, 4)
))
reference_date <- get_next_weekday(forecast_due_date, 0)
horizons <- -1:3
exclude_geos <- tolower(c(
  c("as", "gu", "mp", "vi"),
  c()
))

output_dir <- here::here("data-forecasts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

##### Make quantile forecasts.
quantile_predictions <- get_quantile_predictions(
  forecast_due_date,
  reference_date,
  horizons
)

##### Make direction forecasts.
direction_predictions <- get_direction_predictions(
  forecast_due_date,
  reference_date,
  quantile_predictions
)

##### Combine and write the submissions file.
combined_predictions <- bind_rows(
  quantile_predictions %>% mutate(output_type_id = as.character(output_type_id)),
  direction_predictions
)

write_csv(
  combined_predictions,
  fs::path(
    output_dir,
    sprintf("%s-CMU-TimeSeries-unfiltered.csv", reference_date)
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
    sprintf("%s-CMU-TimeSeries.csv", reference_date)
  ),
  # TODO: Quote all except for value?
  # quote='all' makes sure the location column is quoted.
  quote = "all"
)

##### Render notebook.
rmarkdown::render(
  fs::path("scripts", "plot_prediction_cards.Rmd"),
  output_file = fs::path(
    output_dir,
    sprintf("%s-flu-forecast.html", reference_date)
  ),
  params = list(
    exclude_geos = exclude_geos,
    predictions_file = fs::path(
      output_dir,
      sprintf("%s-CMU-TimeSeries-unfiltered.csv", reference_date)
    )
  )
)

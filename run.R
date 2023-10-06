# Production run script.
#
# Will produce the quantile and direction forecasts for the current week (unless
# parameters are set otherwise.)
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
# Generation date should usually be today.
forecast_generation_date <- as.Date(Sys.getenv(
  "FORECAST_GENERATION_DATE",
  unset = Sys.Date()
))
# The due date is the date of the forecast, which should be a Wednesday. It is
# also the effective as of date for requesting API data.
forecast_due_date <- as.Date(Sys.getenv(
  "FORECAST_DUE_DATE",
  unset = get_previous_weekday(forecast_generation_date, 4)
))
# The reference date is the Saturday after the due date.
reference_date <- get_next_weekday(forecast_due_date, 0)
# These are week difference targets from the reference date.
horizons <- -1:3
# The first four geos are excluded due to lack of data. If any others misbehave,
# add them to the list.
exclude_geos <- tolower(c(
  c("as", "gu", "mp", "vi"),
  c()
))

output_dir <- fs::path("data-forecasts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

##### Make quantile forecasts.
quantile_predictions <- get_quantile_predictions(
  forecast_due_date,
  reference_date,
  horizons
)

##### Filter geos.
quantile_predictions %<>% filter(!geo_value %in% exclude_geos)

quantile_predictions %>% glimpse()

##### Make direction forecasts.
direction_predictions <- get_direction_predictions(
  forecast_due_date,
  reference_date,
  horizons,
  exclude_geos,
  quantile_predictions
)

##### Combine and write the submissions file.
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
combined_predictions <- bind_rows(
  quantile_predictions,
  direction_predictions
) %>%
  select(all_of(keeps))

write_csv(
  combined_predictions,
  fs::path(
    output_dir,
    sprintf("%s-CMU-TimeSeries.csv", reference_date)
  ),
  # quote='all' is important to make sure the location column is quoted.
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
    predictions_file = output_file,
  )
)

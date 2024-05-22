# Production run script.
#
# Will produce the quantile and direction forecasts for the current week (unless
# parameters are set otherwise.)
#
# The FORECAST_GENERATION_DATE is usually today. The FORECAST_DUE_DATE is the
# date of the forecast, which should be the most recent next Wednesday (it is
# also the effective as of date for requesting API data). The CDC defines the
# reference date to be the Saturday after the due date; this refers to unshifted
# data from "previous day hospital admissions" data provided by NHSN, which
# Delphi has already shifted in our API. Therefore DELPHI_REFERENCE_DATE and
# CDC_REFERENCE_DATE are off by one. The horizons are the number of weeks ahead
# to forecast, relative to the reference date.
#
# To exclude a state from the forecast, add its two-letter state code to the
# exclude_geos vector below. "as", "gu", "mp", and "vi" are excluded by default
# due to lack of data.
#
# If you want to clear the cache, set the FLU_HOSP_CLEAR_CACHE environment to
# TRUE.
#
# Outputs:
#
#  - submission file:
#    data-forecasts/CMU-TimeSeries/YYYY-MM-DD-CMU-TimeSeries.csv
#  - ensemble file:
#    data-forecasts/CMU-TimeSeries/generated-%s-reference-date-%s-CMU-TimeSeries-quantile-predictions.csv
#  - notebook file: data-forecasts/CMU-TimeSeries/YYYY-MM-DD-flu-forecast.html
#  - cache files in cache/
#
source(here::here("R", "load_all.R"))


##### Set parameters.
FORECAST_GENERATION_DATE <- as.Date(Sys.getenv(
  "FORECAST_GENERATION_DATE",
  unset = Sys.Date()
  # unset = as.Date("2023-06-01")
))
FORECAST_DUE_DATE <- as.Date(Sys.getenv(
  "FORECAST_DUE_DATE",
  unset = get_next_weekday(FORECAST_GENERATION_DATE, 4)
))
CDC_REFERENCE_DATE <- get_next_weekday(FORECAST_DUE_DATE, 0)
DELPHI_REFERENCE_DATE <- CDC_REFERENCE_DATE - 1L
HORIZONS <- 0:3 # no -1 as it may break the forecaster
EXCLUDE_GEOS <- tolower(c(
  c("as", "gu", "mp", "vi"),
  c()
))
ENFORCED_LATENCY <- 5L # Friday of preceding week, calculated from `forecast_due_date` (expected to be a Wednesday)

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
  if (file.exists(here::here("cache", "forecaster", "ens1", sprintf("%s.RDS", FORECAST_GENERATION_DATE)))) {
    fs::file_delete(here::here("cache", "forecaster", "ens1", sprintf("%s.RDS", FORECAST_GENERATION_DATE)))
  }
}

##### Get, modify, and write forecasts.
combined_predictions <- get_production_forecasts(
  FORECAST_GENERATION_DATE,
  FORECAST_DUE_DATE,
  DELPHI_REFERENCE_DATE,
  HORIZONS,
  ENFORCED_LATENCY
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
  sprintf("%s-CMU-TimeSeries-unfiltered.csv", CDC_REFERENCE_DATE)
)
write_csv(
  combined_predictions,
  unfiltered_csv_path,
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
  filter(!geo_value %in% EXCLUDE_GEOS) %>%
  select(all_of(keeps))
filtered_csv_path <- fs::path(
  output_dir,
  sprintf("%s-CMU-TimeSeries.csv", CDC_REFERENCE_DATE)
)
write_csv(
  filtered_combined_predictions,
  filtered_csv_path,
  # quote='all' makes sure the location column is quoted.
  quote = "all"
)

##### Render notebook.
rmarkdown::render(
  fs::path("scripts", "plot_prediction_cards.Rmd"),
  output_file = fs::path(
    output_dir,
    sprintf("%s-flu-forecast.html", CDC_REFERENCE_DATE)
  ),
  params = list(
    exclude_geos = EXCLUDE_GEOS,
    predictions_file = unfiltered_csv_path,
    forecast_generation_date = FORECAST_GENERATION_DATE
  )
)

# ##### Copy submission file to repo.
# submission_path <- fs::path(
#   Sys.getenv("FLU_SUBMISSIONS_DIR", unset = "."),
#   "model-output",
#   "CMU-TimeSeries",
#   sprintf("%s-CMU-TimeSeries.csv", cdc_reference_date)
# )
# fs::file_copy(
#   filtered_csv_path,
#   submission_path,
#   overwrite = TRUE
# )

# ##### Validate the submission file.
# hubValidations::validate_submission(
#   hub_path = Sys.getenv("FLU_SUBMISSIONS_DIR", unset = "."),
#   file_path = fs::path(
#     "CMU-TimeSeries",
#     sprintf("%s-CMU-TimeSeries.csv", cdc_reference_date)
#   )
# )

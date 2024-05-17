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
  # unset = as.Date("2022-12-01")
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


healthdata_filepath <- here::here("cache", "healthdata", sprintf("g62h-syeh-%s.csv", FORECAST_GENERATION_DATE))
if (!file.exists(healthdata_filepath)) {
  if (!dir.exists(dirname(healthdata_filepath))) {
    dir.create(dirname(healthdata_filepath), recursive = TRUE)
  }
  download.file(
    sprintf(
      "https://healthdata.gov/api/views/g62h-syeh/rows.csv?date=%s&accessType=DOWNLOAD",
      format(FORECAST_GENERATION_DATE, "%Y%m%d")
    ),
    healthdata_filepath
  )
}

converted_healthdata_1d <-
  readr::read_csv(healthdata_filepath) %>%
  transmute(
    geo_value = tolower(state),
    time_value = date - 1L,
    value = previous_day_admission_influenza_confirmed
  ) %>%
  bind_rows(
    # API seems to complete state level with 0s in some cases rather than NAs.
    # Get something sort of compatible with that by summing to national with
    # na.omit = TRUE. As otherwise we have some NAs from probably territories
    # propagated to US level.
    (.) %>%
      group_by(time_value) %>%
      summarize(geo_value = "us", value = sum(value, na.rm = TRUE))
  )

converted_healthdata_prop_7dav <-
  converted_healthdata_1d %>%
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

## comparison_snapshot_prop_7dav <- bind_rows(
##   epidatr::pub_covidcast(
##              "hhs",
##              "confirmed_admissions_influenza_1d_prop_7dav",
##              "state",
##              "day",
##              "*",
##              epirange(forecast_due_date - 2000L, forecast_due_date),
##              as_of = strftime(forecast_due_date, "%Y-%m-%d")
##            ),
##   epidatr::pub_covidcast(
##              "hhs",
##              "confirmed_admissions_influenza_1d_prop_7dav",
##              "nation",
##              "day",
##              "*",
##              epirange(forecast_due_date - 2000L, forecast_due_date),
##              as_of = strftime(forecast_due_date, "%Y-%m-%d")
##            )
## )

## full_join(
##   converted_healthdata_prop_7dav,
##   comparison_snapshot_prop_7dav %>% select(geo_value, time_value, value),
##   by = c("geo_value", "time_value")
## ) %>%
##   filter(is.na(value.x) != is.na(value.y) |
##          !is.na(value.x) & abs(value.x-value.y) > 1e-1) %>%
##   filter(time_value <= forecast_generation_date - 7L * 4L) %>%
##   # count(time_value) %>%
##   # tail()
##   # print(n=1500L)
##   filter(geo_value != "us") %>%
##   arrange(time_value, geo_value) %>%
##   tail()
##   # ^ these look like known mismatches

## full_join(
##   converted_healthdata,
##   comparison_snapshot %>% select(geo_value, time_value, value),
##   by = c("geo_value", "time_value")
## ) %>%
##   filter(is.na(value.x) != is.na(value.y) |
##          !is.na(value.x) & abs(value.x-value.y) > 1e-1) %>%
##   filter(time_value <= forecast_generation_date - 7L * 4L) %>%
##   # count(time_value) %>%
##   # tail()
##   # print(n=1500L)
##   filter(geo_value == "us") %>%
##   arrange(time_value, geo_value) %>%
##   tail()

## full_join(
##   converted_healthdata_prop_7dav,
##   comparison_snapshot_prop_7dav %>% select(geo_value, time_value, value),
##   by = c("geo_value", "time_value")
## ) %>%
##   filter(is.na(value.x) != is.na(value.y) |
##          !is.na(value.x) & abs(value.x-value.y) > 1e-1) %>%
##   filter(time_value <= forecast_generation_date - 7L * 4L) %>%
##   summarize(max(abs(value.x - value.y), na.rm = TRUE))

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
    stopifnot(identical(as_of, NULL) || identical(as_of, FORECAST_GENERATION_DATE))
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

if (as.POSIXlt(FORECAST_DUE_DATE)$wday != 3L) {
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
  if (file.exists(here::here("cache", "forecaster", "ens1", sprintf("%s.RDS", FORECAST_DUE_DATE)))) {
    fs::file_delete(here::here("cache", "forecaster", "ens1", sprintf("%s.RDS", FORECAST_DUE_DATE)))
  }
}

##### Make quantile forecasts.
quantile_predictions <- get_quantile_predictions(
  FORECAST_DUE_DATE,
  DELPHI_REFERENCE_DATE,
  HORIZONS,
  ENFORCED_LATENCY
)

##### Make direction forecasts.
direction_predictions <- get_direction_predictions(
  FORECAST_DUE_DATE,
  DELPHI_REFERENCE_DATE,
  quantile_predictions,
  ENFORCED_LATENCY,
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

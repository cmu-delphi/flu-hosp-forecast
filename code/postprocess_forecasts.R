library(dplyr)
library(magrittr)

INCIDENCE_RATE <- 100000
# NOTE: VI data has been just zeroes for a long time, so we exclude it.
# Add extra states to this list if needed.
# They will be included in the national forecast, but not in the state-level forecast.
exclude_geos <- tolower(c("vi"))
# NOTE: While we make predictions on Tuesday, we want to label the file with a Monday, hence the -1's below.
if (Sys.getenv("FORECAST_DATE", "") != "") {
  forecast_dates <- as.Date(Sys.getenv("FORECAST_DATE"))
} else {
  forecast_dates <- lubridate::today()
}

# Load state population data
state_pop = readr::read_csv(here::here("code","state_pop.csv"), show_col_types = FALSE) %>% rename (
  geo_value=state_id,
  ) %>% select (
    -state_name,
    )

get_preds_full = function(preds_state) {
  preds_state$quantile = signif(preds_state$quantile, 4) # Eliminate rounding issues

  # Transform to weekly incidence counts (not prop)
  preds_state_processed = preds_state %>% inner_join (
    state_pop,
    by='geo_value',
    ) %>% transmute (
      geo_value = geo_value,
      ahead = ahead,
      forecaster = "flu-model",
      data_source = "hhs",
      signal = "confirmed_admissions_influenza_1d_7dav",
      incidence_period = "day",
      forecast_date = forecast_date,
      target = sprintf("%d wk ahead inc flu hosp", (ahead - 4) / 7 + 1),
      target_end_date = target_end_date,
      location=state_code,
      type='quantile',
      quantile=quantile,
      value=value*pop/INCIDENCE_RATE*7,
      )

  # US-level forecasts
  preds_us_unsorted = preds_state_processed %>% group_by (
    forecast_date,
    target,
    target_end_date,
    type,
    quantile,
    ) %>% summarize (
      location='US',
      value=sum(value),
      ) %>% ungroup
  preds_us_list = preds_us_unsorted %>% group_split (
    forecast_date,
    target,
    target_end_date,
    type,
    )
  for (idx in 1:length(preds_us_list)) {
    preds_us_list[[idx]]$quantile = sort(preds_us_list[[idx]]$quantile)
    preds_us_list[[idx]]$value = sort(preds_us_list[[idx]]$value)
  }
  preds_us <- bind_rows(preds_us_list)
  preds_us$data_source <- "hhs"
  preds_us$signal <- "confirmed_admissions_influenza_1d_7dav"
  preds_us$forecaster <- "flu-model"
  preds_us$incidence_period <- "day"
  preds_us$geo_value <- "us"

  preds_full <- bind_rows(preds_state_processed, preds_us) %>%
    arrange(location) %>%
    filter(.data$geo_value %in% exclude_geos == FALSE)

  return(preds_full)
}

preds_state <- readr::read_csv(
    sprintf('data-forecasts/CMU-TimeSeries/%s-CMU-TimeSeries-prediction-full.csv', forecast_dates - 1),
)
preds_full <- get_preds_full(preds_state)

readr::write_csv(preds_full,
                 sprintf('data-forecasts/CMU-TimeSeries/%s-CMU-TimeSeries-prediction-cards.csv', forecast_dates - 1),
                 # quote='all' is important to make sure the location column is quoted.
                 quote='all')

drops <- c("incidence_period", "geo_value", "ahead", "forecaster", "data_source", "signal")
preds_full <- preds_full[, !(names(preds_full) %in% drops)]

readr::write_csv(preds_full,
                 sprintf('data-forecasts/CMU-TimeSeries/%s-CMU-TimeSeries.csv', forecast_dates - 1),
                 # quote='all' is important to make sure the location column is quoted.
                 quote='all')

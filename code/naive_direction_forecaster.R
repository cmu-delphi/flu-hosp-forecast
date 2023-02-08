
library(dplyr)
library(tibble)
library(tidyr)
library(rlang)
library(readr)
library(checkmate)

library(epidatr)
library(epiprocess)
devtools::load_all(here::here("code","direction.forecaster"), export_all=FALSE)

source(here::here("code","approx-cdf.R"))
source(here::here("code","process-state-preds.R"))

augmented_location_data = fetch_updating_resource(
  function() {
    read_csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv",
             col_types = cols(
               abbreviation = col_character(),
               location = col_character(),
               location_name = col_character(),
               population = col_integer(),
               count_rate1per100k = col_integer(),
               count_rate2per100k = col_integer()
             ))
  },
  function(response) {
    assert_tibble(response)
  },
  here::here("cache","location_data")
) %>%
  mutate(large_change_count_thresh = pmax(count_rate2per100k, 40L),
         nonlarge_change_count_thresh = pmax(count_rate1per100k, 20L),
         geo_type = dplyr::if_else(location == "US", "nation", "state"),
         geo_value = dplyr::if_else(location == "US", "us", tolower(covidcast::fips_to_abbr(location))))

nonevaluated_geo_values = c("as","gu","mp","vi")
nonevaluated_locations = c("60","66","69","78")
  # tibble(abbreviation = toupper(nonevaluated_geo_values)) %>%
  # left_join(location_data %>% select(abbreviation, location), by="abbreviation") %>%
  # pull(location) %>%
  # .[!is.na(.)]

# if (Sys.Date() != as.Date("2022-12-19")) stop("need to update date")
# preds = read_csv("~/Downloads/2022-12-19-CMU-TimeSeries.csv", col_types=cols(location=col_character()))
# forecast_date = Sys.Date()
# short_snapshot = covidcast("hhs", "confirmed_admissions_influenza_1d", "day", "state", epirange(as.integer(format(Sys.Date()-20L, "%Y%m%d")), as.integer(format(Sys.Date(), "%Y%m%d"))), "*", as_of=as.integer(format(Sys.Date(),"%Y%m%d"))) %>% fetch_tbl() # FIXME * 7?
today = Sys.Date()
if (today != as.Date("2023-01-31")) stop("need to check/update dates")

# Set the `nominal_forecast_date` and the `forecast_as_of_date`. The
# `nominal_forecast_date` determines what the output files should be named and
# what the forecast target(_end_date)s are. The `forecast_as_of_date` determines
# (through) what `as_of` we can use to prepare the forecast.
nominal_forecast_date = today - 1L
stopifnot(as.POSIXlt(nominal_forecast_date)$wday == 1L) # Monday
forecast_as_of_date = nominal_forecast_date + 1L
stopifnot(as.POSIXlt(forecast_as_of_date)$wday == 2L) # Tuesday

# Also record the forecast generation date (extra metadata / to make sure we
# don't clobber things if we want to compare real-time vs. as-of).
forecast_generation_date = today

preds_state_prop_7dav = readRDS(here::here("cache","tuesday-forecasts","ens1",paste0(forecast_as_of_date,".RDS"))) %>%
  {
    out = .
    # evalcast post-processing:
    assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
                msg = paste("Your forecaster must return a data frame with",
                            "(at least) the columnns `ahead`, `geo_value`,",
                            "`quantile`, and `value`."))
    out$forecast_date <- nominal_forecast_date
    names(out$value) <- NULL
    out <- out %>%
      mutate(
        forecaster = "CMU-TimeSeries",
        data_source = "hhs",
        signal = "hhs_confirmed_admissions_influenza_1d_prop_7dav",
        target_end_date = evalcast:::get_target_period(
          .data$forecast_date,
          "day",
          .data$ahead)$end,
        incidence_period = "day"
      ) %>%
      relocate(forecaster, .before = forecast_date)
    class(out) <- c("predictions_cards", class(out))
    out
  }

preds_full = get_preds_full(preds_state_prop_7dav)
short_snapshot =
  bind_rows(
    evalcast::download_signal(
      "hhs", "confirmed_admissions_influenza_1d",
      nominal_forecast_date-20L, nominal_forecast_date,
      "state", "*",
      as_of = forecast_as_of_date,
      offline_signal_dir = here::here("cache","short_signals_for_direction")
    ) %>% as_tibble(),
    evalcast::download_signal(
      "hhs", "confirmed_admissions_influenza_1d",
      nominal_forecast_date-20L, nominal_forecast_date,
      "nation", "*",
      as_of = forecast_as_of_date,
      offline_signal_dir = here::here("cache","short_signals_for_direction")
    ) %>% as_tibble()
  )

reference_7d_counts =
  short_snapshot %>%
  # reference by forecast Monday - 2L = Saturday, else whatever is soonest before then
  filter(time_value <= nominal_forecast_date - 2L) %>%
  group_by(geo_value) %>%
  complete(time_value = full_seq(time_value, 1L)) %>%
  slice_max(time_value, n=7L) %>%
  summarize(reference_7dcount = sum(value),
            time_value = max(time_value),
            .groups="drop")

# rule of three for probability of "novel behavior"; mixing weight for uniform
uniform.forecaster.weight = 3/(
  # roughly, how many years of hhs influenza are available
  as.numeric(forecast_as_of_date - as.Date("2020-10-16"))/(365+1/4-1/100+1/400) *
    # say there's one wave/season per year
    1 *
    # say states from separate HHS Regions are "separate" enough to count as different data points, while those within the same region aren't separate enough; 10 HHS Regions = 10 buckets
   10
)

unfiltered_direction_predictions =
  preds_full %>%
  # filter(! geo_value %in% .env$nonevaluated_geo_values) %>%
  # filter(ahead == 7L*2L - 2L) %>%
  # group_by(geo_value) %>%
  filter(! location %in% .env$nonevaluated_locations) %>%
  filter(target == "2 wk ahead inc flu hosp") %>%
  group_by(forecast_date, location) %>%
  summarize(
    forecast = list(approx_cdf_from_quantiles(value, quantile)),
    .groups="keep"
  ) %>%
  # left_join(reference_7d_counts, by="geo_value") %>%
  # left_join(augmented_location_data, by="geo_value") %>%
  left_join(augmented_location_data, by="location") %>%
  left_join(reference_7d_counts, by="geo_value") %>%
  summarize(
    type = "category",
    type_id = c("large_decrease", "decrease", "stable", "increase", "large_increase") %>% {factor(., levels=.)},
    value =
      {
        stopifnot(length(forecast) == 1L)
        p_large_dec = p_le(forecast[[1L]], reference_7dcount - large_change_count_thresh)
        p_large_or_nonlarge_dec = p_le(forecast[[1L]], reference_7dcount - nonlarge_change_count_thresh)
        p_large_or_nonlarge_inc = p_ge(forecast[[1L]], reference_7dcount + nonlarge_change_count_thresh)
        p_large_inc = p_ge(forecast[[1L]], reference_7dcount + large_change_count_thresh)
        result = c(p_large_dec,
          p_large_or_nonlarge_dec - p_large_dec,
          1 - p_large_or_nonlarge_dec - p_large_or_nonlarge_inc,
          p_large_or_nonlarge_inc - p_large_inc,
          p_large_inc)
        result
      },
    .groups="keep"
  ) %>%
  # mix with uniform
  mutate(value =
           (1-uniform.forecaster.weight) * value +
           uniform.forecaster.weight * 1/n()) %>%
  ungroup() %>%
  mutate(target = "2 wk flu hosp rate change") %>%
  select(forecast_date, target, location, type, type_id, value)

direction_predictions_dir = here::here("code","data-forecasts","direction-predictions",paste0("generated-",forecast_generation_date,"-as-of-",forecast_as_of_date))
if (!dir.exists(direction_predictions_dir)) {
  dir.create(direction_predictions_dir, recursive=TRUE)
}

write_csv(
  unfiltered_direction_predictions,
  file.path(direction_predictions_dir, paste0("prefilter-",nominal_forecast_date,"-CMU-TimeSeries.csv")),
  # quote='all' is important to make sure the location column is quoted.
  quote="all"
)

if (today != as.Date("2023-01-31")) stop("need to update exclusions")
excluded_locations =
  c(
    augmented_location_data %>%
      filter(geo_value == "vi") %>%
      pull(location),
    # week-to-week exclusions:
    augmented_location_data %>%
      filter(geo_value %in% c("dc", "nd", "nh", "nv", "ri", "vt")) %>%
      pull(location)
  )
# TODO validate week-to-week exclusions aren't misspelled

filtered_direction_predictions = unfiltered_direction_predictions %>%
  filter(! location %in% excluded_locations)

stopifnot(
  all(abs(1 -
            filtered_direction_predictions %>%
            group_by(forecast_date, target, location) %>%
            summarize(value = sum(value)) %>%
            pull(value)
          ) < 1e-8)
)

write_csv(
  filtered_direction_predictions,
  file.path(direction_predictions_dir, paste0(nominal_forecast_date,"-CMU-TimeSeries.csv")),
  # quote='all' is important to make sure the location column is quoted.
  quote="all"
)

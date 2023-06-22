library(dplyr)
library(tibble)
library(tidyr)
library(rlang)
library(readr)
library(checkmate)
library(data.table)

library(epidatr)
library(epiprocess)
devtools::load_all(here::here("code", "direction.forecaster"), export_all = FALSE)

states_dc_pr_vi <- c(
  "al", "ak", "az", "ar", "ca", "co", "ct", "dc", "de", "fl",
  "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me",
  "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh",
  "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri",
  "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi",
  "wy", "pr", "vi"
)

response_to_archive <- function(response) {
  response %>%
    unite(source_signal, c(source, signal)) %>%
    rename(version = issue) %>%
    pivot_wider(id_cols = c(geo_value, time_value, version), names_from = source_signal, values_from = value) %>%
    as_epi_archive(compactify = TRUE)
}

fetch_updating_covidcast_archive <- function(data_source, signal) {
  fetch_updating_resource(
    function() {
      covidcast(data_source, signal,
        "day", "state",
        epirange(12340101, 34560101),
        "*",
        issues = epirange(12340101, as.integer(format(Sys.Date(), "%Y%m%d")))
      ) %>%
        fetch_tbl() %>%
        filter(.data$geo_value %in% .env$states_dc_pr_vi)
    },
    function(response) {
      if (!is_tibble(response)) {
        abort("result was not a tibble")
      }
      if (nrow(response) == 0L) {
        abort("result had no rows")
      }
    },
    cache_file_prefix = here::here(
      "cache", "issue-data",
      paste0(data_source, "_", signal)
    )
  ) %>%
    response_to_archive()
}

# NOTE that these are different signals than those pulled by train_model.R!

archive_hhs_confirmed_admissions_influenza_1d <-
  fetch_updating_covidcast_archive("hhs", "confirmed_admissions_influenza_1d") %>%
  # filter out some bad data for time_values before reporting became mandatory
  # the first time; not sure exactly when influenza reporting compliance was
  # broadly achieved, but this is better than nothing:
  {
    new_min_time_value <- .$DT[
      !is.na(hhs_confirmed_admissions_influenza_1d) &
        hhs_confirmed_admissions_influenza_1d != 0L &
        geo_value != "nv",
      min(time_value)
    ]
    .$DT <- .$DT[.$DT$time_value >= new_min_time_value, names(.$DT), with = FALSE]
    .
  }

archive_chng_smoothed_adj_outpatient_flu <-
  fetch_updating_covidcast_archive("chng", "smoothed_adj_outpatient_flu")

archive <-
  epix_merge(archive_hhs_confirmed_admissions_influenza_1d, archive_chng_smoothed_adj_outpatient_flu,
    sync = "locf"
  )

location_data <- fetch_updating_resource(
  function() {
    read_csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv",
      col_types = cols(
        abbreviation = col_character(),
        location = col_character(),
        location_name = col_character(),
        population = col_integer(),
        count_rate1per100k = col_integer(),
        count_rate2per100k = col_integer()
      )
    )
  },
  function(response) {
    assert_tibble(response)
  },
  here::here("cache", "location_data")
)

forecast_dates <- seq(
  # a Monday at/after we've had issues for both signals for 56 days:
  as.Date("2022-01-31"),
  # end bound from `make_common_params.py` on 2022-10-17:
  Sys.Date() - 2L,
  # only Mondays for now:
  by = "week"
)
stopifnot(all(as.POSIXlt(forecast_dates)$wday == 1L))

forecast_date <- forecast_dates[[1L]]

truncated_archive <- archive %>%
  epix_truncate_versions_after(forecast_date)

latest_snapshot <-
  truncated_archive %>%
  epix_as_of(.$versions_end)

# max time_value with any non-NA observation for both signals, for any nonzero
# number of geos. Relies on there never being a small set of geos that are
# "ahead". We can exclude forecasts for locations without all observations
# available for this time_value (or other covariate time_value we will derive).
max_shared_time_value <-
  latest_snapshot %>%
  filter(!if_any(c(hhs_confirmed_admissions_influenza_1d, chng_smoothed_adj_outpatient_flu), is.na)) %>%
  .$time_value %>%
  max()

min_shared_lag <- as.integer(truncated_archive$verions_end - max_shared_time_value)

# truncated_archive %>%
#   epix_slide(before = min_shared_lag + )

# FIXME finish

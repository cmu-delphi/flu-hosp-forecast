library(checkmate)
library(data.table)
library(dplyr)
library(epidatr)
library(epiprocess)
library(readr)
library(rlang)
library(tibble)
library(tidyr)


states_dc_pr_vi <- c(
  "al", "ak", "az", "ar", "ca", "co", "ct", "dc", "de", "fl",
  "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me",
  "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh",
  "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri",
  "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi",
  "wy", "pr", "vi"
)


# NOTE that these are different signals than those pulled by train_model.R!
archive_hhs_confirmed_admissions_influenza_1d <- pub_covidcast(
  "hhs",
  "confirmed_admissions_influenza_1d",
  "state",
  "day",
  "*",
  epirange(12340101, 34560101),
  issues = epirange(12340101, as.integer(format(Sys.Date(), "%Y%m%d")))
) %>%
  filter(.data$geo_value %in% .env$states_dc_pr_vi) %>%
  unite(source_signal, c(source, signal)) %>%
  rename(version = issue) %>%
  pivot_wider(id_cols = c(geo_value, time_value, version), names_from = source_signal, values_from = value) %>%
  as_epi_archive(compactify = TRUE) %>%
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

archive_chng_smoothed_adj_outpatient_flu <- pub_covidcast(
  "chng",
  "smoothed_adj_outpatient_flu",
  "state",
  "day",
  "*",
  epirange(12340101, 34560101),
  issues = epirange(12340101, as.integer(format(Sys.Date(), "%Y%m%d")))
) %>%
  filter(.data$geo_value %in% .env$states_dc_pr_vi) %>%
  unite(source_signal, c(source, signal)) %>%
  rename(version = issue) %>%
  pivot_wider(id_cols = c(geo_value, time_value, version), names_from = source_signal, values_from = value) %>%
  as_epi_archive(compactify = TRUE) %>%
  archive() <-
  epix_merge(archive_hhs_confirmed_admissions_influenza_1d, archive_chng_smoothed_adj_outpatient_flu,
    sync = "locf"
  )

location_data <- get_flusight_location_data()

forecast_dates <- seq(
  # a Monday at/after we've had issues for both signals for 56 days:
  as.Date("2022-01-31"),
  Sys.Date() - 2L,
  # only Mondays for now:
  by = "week"
)
stopifnot(all(as.POSIXlt(forecast_dates)$wday == 1L))

forecast_date <- forecast_dates[[1L]]

epix_truncate_versions_after <- function(archive, version) {
  checkmate::assert(epiprocess::is_epi_archive(archive, grouped_okay = TRUE))
  checkmate::assert(identical(class(archive$DT$version), class(version)))

  result <- archive$clone()
  result$DT <- result$DT[result$DT$version <= version, colnames(result$DT), with = FALSE]
  if (!is.na(result$clobberable_versions_start) &&
    result$clobberable_versions_start > version) {
    result$clobberable_versions_start <- NA
  }
  result$versions_end <- version

  return(result)
}

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

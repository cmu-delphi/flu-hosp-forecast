# This file is meant to AB test the workaround we used in run.R to get the most
# recent data from healthdata.gov. This requires manually downloading the right
# file and transforming it in the way the HHS pipeline would. All that is done
# in `get_health_data`.
#
# Note that this comparison is limited to a single covariate. It will need to be
# updated if more covariates are used.

source(here::here("R", "load_all.R"))

forecast_generation_date <- as.Date("2023-12-10")

workaround_data <- get_health_data(forecast_generation_date)

epidatr_data <- bind_rows(
  epidatr::pub_covidcast(
    "hhs",
    "confirmed_admissions_influenza_1d",
    "state",
    "day",
    "*",
    epirange(forecast_generation_date - 2000L, forecast_generation_date),
    as_of = forecast_generation_date
  ),
  epidatr::pub_covidcast(
    "hhs",
    "confirmed_admissions_influenza_1d",
    "nation",
    "day",
    "*",
    epirange(forecast_generation_date - 2000L, forecast_generation_date),
    as_of = forecast_generation_date
  )
)

epidatr_data %>% summarize(max(issue))

# Are there differences in the overlapping (time_value, geo_keys)? No.
diff <- full_join(
  workaround_data,
  epidatr_data %>% select(geo_value, time_value, value),
  by = c("geo_value", "time_value")
) %>%
  filter(!is.na(value.x), !is.na(value.y)) %>%
  filter(!dplyr::near(value.x, value.y)) %>%
  filter(!geo_value %in% c("us")) %>%
  arrange(time_value, geo_value) %>%
  print(n = 1000)

# Are there keys where the (time_value, geo_keys) don't overlap? Yes, but they
# are sparse.
full_join(
  workaround_data,
  epidatr_data %>% select(geo_value, time_value, value),
  by = c("geo_value", "time_value")
) %>%
  filter(is.na(value.x) != is.na(value.y)) %>%
  arrange(time_value, geo_value) %>%
  print(n = 1000)

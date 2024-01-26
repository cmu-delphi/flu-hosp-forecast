# to actually run this, you need to have some content in the ".forecast-cache/cdfens5p_nil/" folder, which can be found on mentat
library(ggplot2)
library(tidyverse)
library(magrittr)
library(epidatr)
fips <- read.csv("scripts/fips_to_state.csv") %>%
  as_tibble() %>%
  mutate(location = as.integer(location))
fips
labelled_predictions <- tibble(
  geo_value = character(),
  value = numeric(),
  ahead = integer(),
  quantile = numeric(),
  status = character(),
  forecast_date = Date(),
  target_end_date = Date()
)
always_excluded <- c("as", "gu", "vi")
usage <- function(x, excluded) {
  ifelse(x %in% excluded, "excluded", ifelse(x %in% always_excluded, "auto_exclude", "submitted"))
}
base_folder <- "cache/forecasters/ens1/"
file <- list.files(base_folder)[10]
check_fuzzed_date <- function(given_date, location) {
  for (offset in -5:5) {
    hypothetical_filename <- paste0(location, given_date + offset, "-CMU-TimeSeries.csv")
    if (file.exists(hypothetical_filename)) {
      return(hypothetical_filename)
    }
  }
  return(NULL)
}
for (file in list.files(base_folder)) {
  if (grepl("from_api", file)) next
  forecast_date <- as.Date(substr(file, 1, 10))
  # get just the forecasts we submitted
  filename <- check_fuzzed_date(forecast_date, "../FluSight-forecast-hub/model-output/CMU-TimeSeries/")
  if (!is.null(filename)) {
    submitted <- read.csv(filename) %>% as_tibble()
  } else {
    filename <- check_fuzzed_date(forecast_date, "../Flusight-forecast-data/data-forecasts/CMU-TimeSeries/")
    if (!is.null(filename)) {
      submitted_file <- read.csv(filename) %>% as_tibble()
    } else {
      print(paste0(file, " has no matching submitted file, skipping"))
      next
    }
  }
  print(file)

  submitted$target %>% unique()
  submitted %<>% filter(output_type == "quantile") %>%
    mutate(ahead = horizon) %>%
    mutate(location = as.integer(ifelse(location == "US", "-1", location))) %>%
    left_join(fips, by = "location") %>%
    select(-c(horizon, target, location, output_type)) %>%
    rename(geo_value = abbreviation, quantile = output_type_id)

  submitted$target_end_date %>% unique()
  # get all of the forecasts produced
  generated <- read_rds(paste0(base_folder, file))
  submitted_geos <- submitted %>%
    pull(geo_value) %>%
    unique()
  generated_geos <- generated %>%
    pull(geo_value) %>%
    unique()
  excluded <- setdiff(setdiff(generated_geos, submitted_geos), always_excluded)
  labelled <- generated %>% mutate(status = usage(geo_value, excluded), forecast_date = as.Date(forecast_date), target_end_date = ahead + forecast_date)
  #ggplot(labelled %>% group_by(geo_value,quantile,target_end_date) %>% count(), aes(x = target_end_date, y = n)) + geom_line() + facet_wrap(vars(geo_value))
  labelled_predictions %<>% add_row(labelled)
}
labelled_predictions %<>% arrange(target_end_date)
label_counts <- labelled_predictions %>%
  group_by(status) %>%
  count(status)
# the amount we've excluded:
label_counts
label_counts[2, 2] / sum(label_counts[2:3, 2])
# or about 7% of all forecasts
ggplot(labelled_predictions %>% group_by(geo_value,quantile,target_end_date, forecast_date) %>% count() %>% arrange(target_end_date), aes(x = target_end_date, y = n, color = quantile)) + geom_line() + facet_wrap(vars(geo_value)) + labs(title = "total #")
labelled_predictions %>% group_by(geo_value,quantile,target_end_date, forecast_date) %>% count() %>% arrange(desc(n))

eval_time <- epidatr::epirange(from = min(labelled_predictions$forecast_date), to = max(labelled_predictions$forecast_date))
fetch_args <- epidatr::fetch_args_list(return_empty = TRUE, timeout_seconds = 400)
hhs_signal <- epidatr::pub_covidcast(
  source = "hhs",
  signals = "confirmed_admissions_influenza_1d_7dav",
  geo_type = "state",
  time_type = "day",
  geo_values = "*",
  time_values = eval_time,
  fetch_args = fetch_args
)
hhs_signal %<>% select(-c("source", "geo_type", "signal", "direction", "lag", "missing_stderr", "missing_value", "missing_sample_size", "stderr", "sample_size", "time_type", "issue"))
hhs_signal %<>% rename(actual_value = value)
#ggplot(labelled_predictions %>% group_by(geo_value, target_end_date) %>% count(target_end_date) %>% ungroup(), aes(x = target_end_date, y = n)) + facet_wrap(vars(geo_value))
with_actual_value <- labelled_predictions %>% inner_join(hhs_signal, by = join_by(geo_value, target_end_date == time_value))
with_actual_value %>% group_by(geo_value, target_end_date) %>% count(target_end_date)
is_equal_tol <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}
interval_coverage <- function(actual_value, value, quantile, coverage = .9) {
  alpha <- 1 - coverage
  lower_interval <- alpha / 2
  upper_interval <- 1 - (alpha / 2)
  return((actual_value[1] >= value[is_equal_tol(quantile, lower_interval)]) &
    (actual_value[1] <= value[is_equal_tol(quantile, upper_interval)]))
}
scored <- with_actual_value %>%
  group_by(geo_value, forecast_date, target_end_date) %>%
  summarise(
    wis = 2 * mean(pmax(quantile * (actual_value - value), (1 -
      quantile) * (value - actual_value))),
    nintyPI = interval_coverage(actual_value, value, quantile, .9),
    fiftyPI = interval_coverage(actual_value, value, quantile, .5),
    status = status[1],
    .groups = "keep"
  )
mean(scored$nintyPI)
mean(scored$fiftyPI)

scored$ahead <- scored$target_end_date - scored$forecast_date
ggplot(scored, aes(x = target_end_date, y = wis)) + geom_line() + facet_wrap(vars(geo_value))
n_excluded <- scored %>%
  group_by(forecast_date, status) %>%
  summarize(n = n(), wis = mean(wis), nintyPI = mean(nintyPI), fiftyPI = mean(fiftyPI), .groups = "keep") %>%
  mutate(n = n / 5)
n_excluded$forecast_date %>% unique

scale_coeff <- max(n_excluded$wis) / max(n_excluded$n) * 2
ggplot(n_excluded, aes(x = forecast_date, y = n, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_line(aes(y = wis / scale_coeff, linewidth = 2.01)) + # adding a border so its distinguishable
  geom_line(aes(y = wis / scale_coeff, color = status, linewidth = 2)) +
  scale_y_continuous(
    # Features of the first axis
    name = "status",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . * scale_coeff, name = "mean WIS")
  ) +
  labs(title = "Fraction Excluded vs mean WIS score")
ggsave("fracExcludedVsMeanWISForecastDate.png")

scale_coeff <- max(n_excluded$nintyPI) / max(n_excluded$n) * 2
ggplot(n_excluded, aes(x = forecast_date, y = n, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_line(aes(y = nintyPI / scale_coeff, linewidth = 2.01)) + # adding a border so its distinguishable
  geom_line(aes(y = nintyPI / scale_coeff, color = status, linewidth = 2)) +
  scale_y_continuous(
    # Features of the first axis
    name = "status",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . * scale_coeff, name = "90% CI")
  ) +
  labs(title = "Fraction Excluded vs 90CI")
ggsave("fracExcludedVs90IntervalCovForecastDate.png")

scale_coeff <- max(n_excluded$fiftyPI) / max(n_excluded$n) * 2
ggplot(n_excluded, aes(x = forecast_date, y = n, fill = status)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_line(aes(y = fiftyPI / scale_coeff, linewidth = 2.01)) + # adding a border so its distinguishable
  geom_line(aes(y = fiftyPI / scale_coeff, color = status, linewidth = 2)) +
  scale_y_continuous(
    # Features of the first axis
    name = "status",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . * scale_coeff, name = "50% CI")
  ) +
  labs(title = "Fraction Excluded vs 50CI")
ggsave("fracExcludedVs50IntervalCovForecastDate.png")

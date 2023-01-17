
state_pop = readr::read_csv(here::here("code","state_pop.csv"), show_col_types = FALSE) %>% rename (
  geo_value=state_id,
  ) %>% select (
    -state_name,
    )

INCIDENCE_RATE = 100000


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
      target = sprintf('%d wk ahead inc flu hosp', (ahead-5)/7+1),
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

  preds_full = bind_rows(preds_state_processed, preds_us) %>% arrange(
    location,
    ) %>%
    # Remove VI as data has just been zeroes and forecaster time window leads to
    # following trends from other locations and doesn't cover 0. (But keep it when
    # forming the national predictions above.)
    filter(.data$geo_value != "vi") 
  # Prod run exclude geos
  # filter(!.data$geo_value %in% c("hi", "ga", "fl", "la", "sc", "va", "tx", "al"))
}

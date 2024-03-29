#### BEGIN copied/adapted content from cmu-delphi/hospitalization-forecaster
#### production-scripts/plotting.R as of 2022-10-17


get_quantiles_df <- function(predictions_cards, intervals = c(.5, .9), ...) {
  predictions_cards <- predictions_cards %>%
    dplyr::select(
      .data$geo_value, .data$quantile,
      .data$value, .data$forecaster, .data$forecast_date,
      .data$target_end_date
    )

  lower_bounds <- predictions_cards %>%
    select(.data$quantile) %>%
    filter(.data$quantile < 0.5) %>%
    unique() %>%
    pull()
  quantiles_to_plot <- as.integer(sort(
    round(500L * (1 + intervals %o% c(-1L, 1L)))
  ))

  quantiles_df <- predictions_cards %>%
    filter(as.integer(round(.data$quantile * 1000)) %in% c(quantiles_to_plot)) %>%
    mutate(
      endpoint_type = if_else(.data$quantile < 0.5, "lower", "upper"),
      alp = if_else(.data$endpoint_type == "lower",
        format(2 * .data$quantile, digits = 3, nsmall = 3),
        format(2 * (1 - .data$quantile), digits = 3, nsmall = 3)
      ),
      interval = forcats::fct_rev(
        paste0((1 - as.numeric(.data$alp)) * 100, "%")
      )
    ) %>%
    select(-.data$quantile, -.data$alp) %>%
    pivot_wider(names_from = "endpoint_type", values_from = "value")

  return(quantiles_df)
}

get_points_df <- function(predictions_cards) {
  points_df <- predictions_cards %>%
    filter(as.integer(round(.data$quantile * 1000)) == 500L |
      is.na(.data$quantile))
  if (any(is.na(points_df$quantile))) {
    points_df <- points_df %>%
      pivot_wider(names_from = "quantile", values_from = "value") %>%
      mutate(value = if_else(!is.na(.data$`NA`), .data$`NA`, .data$`0.5`)) %>%
      select(-.data$`0.5`, -.data$`NA`)
  } else {
    points_df <- points_df %>%
      select(-.data$quantile)
  }

  return(points_df)
}

plot_quantiles <- function(g, quantiles_df) {
  n_quantiles <- nlevels(quantiles_df$interval)
  l_quantiles <- levels(quantiles_df$interval)

  alp <- c(.4, .2, .1)
  for (qq in n_quantiles:1) {
    g <- g +
      geom_ribbon(
        data = quantiles_df %>%
          filter(.data$interval == l_quantiles[qq]),
        mapping = aes(
          ymin = .data$lower,
          ymax = .data$upper,
          group = interaction(.data$forecast_date, .data$forecaster)
        ),
        alpha = alp[qq]
      )
  }

  return(g)
}

plot_points <- function(g, points_df) {
  g <- g + geom_point(
    data = points_df,
    mapping = aes(
      y = .data$value,
      group = interaction(.data$forecast_date, .data$forecaster)
    )
  )

  return(g)
}

plot_state_forecasters <- function(predictions_cards, exclude_geos = c(), start_day = NULL, ncol = 5) {
  if (nrow(predictions_cards) == 0) {
    return(NULL)
  }

  predictions_cards %<>% filter(
    !geo_value %in% exclude_geos
  )

  td1 <- epidatr::pub_covidcast(
    "hhs",
    "confirmed_admissions_influenza_1d_7dav",
    "state",
    "day",
    "*",
    epirange(start_day, 20300101)
  ) %>%
    mutate(
      value = 7L * .data$value,
      data_source = "hhs"
    ) %>%
    rename(target_end_date = time_value)
  td2 <- epidatr::pub_covidcast(
    "chng",
    "smoothed_adj_outpatient_flu",
    "state",
    "day",
    "*",
    epirange(start_day, 20300101)
  ) %>%
    rename(target_end_date = time_value) %>%
    mutate(
      data_source = "chng"
    )
  td1.max <- td1 %>%
    group_by(geo_value) %>%
    summarize(max_value = max(value))
  td2.max <- td2 %>%
    group_by(geo_value) %>%
    summarize(max_value = max(value))
  td2.max <- td2.max %>%
    left_join(td1.max, by = "geo_value", suffix = c(".2", ".1")) %>%
    mutate(max_ratio = max_value.1 / max_value.2)
  td2 <- td2 %>%
    left_join(td2.max, by = "geo_value") %>%
    mutate(scaled_value = value * max_ratio)
  td1 <- td1 %>% mutate(forecaster = "hhs hosp truth")
  td2 <- td2 %>% mutate(forecaster = "chng smoothed_adj_outpatient_flu current, scaled")

  # Setup plot
  g <- ggplot(td1, mapping = aes(x = .data$target_end_date, color = .data$forecaster, fill = .data$forecaster))

  points_df <- get_points_df(predictions_cards)
  g <- plot_points(g, points_df)

  quantiles_df <- get_quantiles_df(predictions_cards)
  g <- plot_quantiles(g, quantiles_df)

  # Plot truth data by geo
  g <- g +
    geom_line(mapping = aes(y = .data$value)) +
    geom_line(data = td2, mapping = aes(x = .data$target_end_date, y = .data$scaled_value)) +
    facet_wrap(~ .data$geo_value, scales = "free_y", ncol = ncol, drop = TRUE) +
    theme(legend.position = "top", legend.text = element_text(size = 7))

  return(g)
}

plot_nation_forecasters <- function(predictions_cards, exclude_geos = c(), start_day = NULL, ncol = 5) {
  if (nrow(predictions_cards) == 0) {
    return(NULL)
  }

  predictions_cards %<>% filter(
    !geo_value %in% exclude_geos
  )

  td1 <- epidatr::pub_covidcast(
    "hhs",
    "confirmed_admissions_influenza_1d_7dav",
    "state",
    "day",
    "*",
    epirange(start_day, 20300101)
  ) %>%
    mutate(
      value = 7L * .data$value,
      data_source = "hhs"
    ) %>%
    rename(target_end_date = time_value) %>%
    group_by(
      target_end_date
    ) %>%
    summarize(value = sum(value))
  td2 <- epidatr::pub_covidcast(
    "chng",
    "smoothed_adj_outpatient_flu",
    "nation",
    "day",
    "*",
    epirange(start_day, 20300101)
  ) %>%
    select(
      target_end_date = time_value,
      value
    )
  td1.max <- td1 %>%
    summarize(max_value = max(value)) %>%
    pull(max_value)
  td2.max <- td2 %>%
    summarize(max_value = max(value)) %>%
    pull(max_value)
  td2 <- td2 %>%
    mutate(scaled_value = value * td1.max / td2.max)

  # Setup plot
  g <- ggplot(td1, mapping = aes(x = .data$target_end_date))

  quantiles_df <- get_quantiles_df(predictions_cards)
  g <- plot_quantiles(g, quantiles_df)

  points_df <- get_points_df(predictions_cards)
  g <- plot_points(g, points_df)

  # Plot truth data by geo
  g <- g +
    geom_line(mapping = aes(y = .data$value, color = "confirmed admissions")) +
    geom_line(data = td2, mapping = aes(x = .data$target_end_date, y = .data$scaled_value, color = "chng smoothed_adj_outpatient_flu, scaled")) +
    labs(fill = "Reported Signal") +
    theme(legend.position = "top", legend.text = element_text(size = 7))

  return(g)
}

#### END copied/adapted content

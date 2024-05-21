# Version: 2024-04-23

plot_forecasts <- function(predictions_cards, forecast_date, exclude_geos, geo_type, data_7dav_override = NULL) {
  assert_that(nrow(predictions_cards) > 0)
  assert_that(geo_type %in% c("state", "nation"))

  start_day <- as.Date("2022-10-01")

  if (is.null(data_7dav_override)) {
    signal_data <- epidatr::pub_covidcast(
      "hhs",
      "confirmed_admissions_influenza_1d_7dav",
      geo_type,
      "day",
      "*",
      epirange(start_day, forecast_date)
    )
  } else {
    if (geo_type == "state") {
      signal_data <- data_7dav_override %>%
        filter(geo_value != "us", start_day <= time_value)
    } else if (geo_type == "nation") {
      signal_data <- data_7dav_override %>%
        filter(geo_value == "us", start_day <= time_value)
    }
  }
  signal_data <- signal_data %>%
    filter(!.data$geo_value %in% exclude_geos) %>%
    select(.data$geo_value, .data$time_value, .data$value) %>%
    mutate(
      data_source = "hhs",
      forecaster = "hhs hosp truth",
      value = 7L * .data$value
    ) %>%
    rename(target_end_date = .data$time_value)

  # Setup plot
  g <- ggplot(signal_data, mapping = aes(
    x = .data$target_end_date,
    color = .data$forecaster,
    fill = .data$forecaster
  )) +
    geom_line(mapping = aes(y = .data$value))

  # Plot (symmetric) quantiles
  quantiles <- c(0.75, 0.95)
  alphas <- c(0.4, 0.2)
  for (i in seq_along(quantiles)) {
    q <- quantiles[i]
    a <- alphas[i]
    g <- g +
      geom_ribbon(
        data = predictions_cards %>%
          filter(near(.data$quantile, q) | near(.data$quantile, 1 - q)) %>%
          mutate(
            quantile = ifelse(near(.data$quantile, q), "upper", "lower") %>%
              as.factor()
          ) %>%
          pivot_wider(names_from = "quantile", values_from = "value"),
        mapping = aes(
          ymin = .data$lower,
          ymax = .data$upper,
          group = interaction(.data$forecast_date, .data$forecaster),
          color = NULL
        ),
        alpha = a
      )
  }

  # Plot median
  g <- g +
    geom_point(
      data = predictions_cards %>%
        filter(near(.data$quantile, 0.5)),
      mapping = aes(
        y = .data$value,
        group = interaction(.data$forecast_date, .data$forecaster)
      ),
      size = 0.125
    )

  if (geo_type == "state") {
    # Add lines, facet, and theme
    g <- g +
      facet_wrap(~ .data$geo_value, scales = "free_y", ncol = 2, drop = TRUE) +
      theme(legend.position = "top", legend.text = element_text(size = 7))
  } else if (geo_type == "nation") {
    # Add lines  and theme
    g +
      labs(fill = "Reported Signal") +
      theme(legend.position = "top", legend.text = element_text(size = 7))
  }

  return(g)
}

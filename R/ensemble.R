source(here::here("R", "approx-cdf.R"))

make_ensemble_forecaster <- function(component_alternatives, offline_signal_dir) {
  assert_that(is_string(offline_signal_dir))
  return(
    function(df_list, forecast_date) {
      component_predictions_cards <- component_alternatives %>%
        map_dfr(function(alternative) {
          get_predictions(
            alternative$forecaster,
            forecaster_name(alternative$forecaster),
            alternative$signals,
            forecast_date,
            incidence_period = "day",
            forecaster_args = list(),
            offline_signal_dir = offline_signal_dir
          )
        })
      component_predictions_cards %>%
        ## standardize quantile levels between components (some issues with
        ## floating-point values being slightly different):
        mutate(quantile = round(quantile, 5L)) %>>%
        group_by(
          data_source,
          signal,
          incidence_period,
          forecast_date,
          ahead,
          target_end_date,
          geo_value,
          forecaster
        ) %>>%
        ## validate (via filters and errors) and fix up component forecasts
        ##
        ## TODO this probably belongs somewhere else
        filter(!anyNA(value)) %>>%
        (~output_taus <- unique(.[["quantile"]])) %>>%
        mutate(value = {
          stopifnot(identical(quantile, output_taus))
          pmax(0, sort(value))
        }) %>>%
        ungroup() %>>%
        group_by(
          data_source, signal, incidence_period, forecast_date, ahead, target_end_date, geo_value,
          ## `forecaster` must be last here for drop_last to work
          forecaster
        ) %>>%
        arrange(quantile, .by_group = TRUE) %>>%
        summarize(forecast = list(approx_cdf_from_quantiles(value, quantile)), .groups = "drop_last") %>>%
        summarize(
          quantile = output_taus,
          value = quantile(weighted_mean_approx_cdfs(forecast, rep(1 / length(forecast), length(forecast))), output_taus),
          .groups = "drop"
        ) %>>%
        ## XXX select out the columns that evalcast expects and let it augment;
        #  assume without checking we didn't need the extra info
        select(geo_value, value, ahead, quantile)
    }
  )
}


library(pipeR)
library(shiny)
library(plotly)

if (!exists("common_set_evaluations")) {
  source("explore_2022_alternatives.R")
}

population_df =
  covidcast::state_census %>>%
  transmute(geo_value = tolower(ABBR), population = POPESTIMATE2019)

processed_common_set_evaluations =
  common_set_evaluations %>%
  left_join(population_df, by="geo_value") %>%
  mutate(across(c(wis, ae), list(
    "count_scale" = function(x) x/100e3*population,
    "per_100k" = identity
  ))) %>%
  select(-wis, -ae) %>%
  rename(wis = wis_count_scale, ae = ae_count_scale) %>%
  {.}

#### Adapted from shiny-eval.R from cmu-delphi/hospitalization-forecaster

shinyApp(
  onStart = function() {
    enableBookmarking(store="url")
  },
  ui=function(request) {
    fluidPage(
      titlePanel('Test Eval Summary Dashboard (stats on 7dav rather than 7dsum, no national)'),
      bookmarkButton(),
      sidebarLayout(
        sidebarPanel(
          selectInput("selected_forecasters",
                      'Forecasters:',
                      choices=unique(processed_common_set_evaluations$forecaster),
                      multiple=TRUE),
          radioButtons("selected_metric",
                       'Metric:',
                       c('Mean WIS'="wis_mean",
                         'Mean WIS per 100k'="wis_per_100k_mean",
                         'Mean AE'="ae_mean",
                         'Mean AE per 100k'="ae_per_100k_mean",
                         '80%PI Coverage'="coverage_80_mean")),
          selectInput("x_var",
                      'x var:',
                      choices=c("forecaster","ahead","forecast_date","target_end_date","geo_value"),
                      multiple=FALSE),
          selectInput("facet_vars",
                      'facet vars:',
                      choices=c("forecaster","ahead","geo_value"),
                      multiple=TRUE),
          sliderInput("selected_forecast_date_range",
                      'Forecast date range:',
                      min=min(processed_common_set_evaluations$forecast_date),
                      max=max(processed_common_set_evaluations$forecast_date),
                      value=range(processed_common_set_evaluations$forecast_date)),
          sliderInput("selected_target_end_date_range",
                      'Target end date range:',
                      min=min(processed_common_set_evaluations$target_end_date),
                      max=max(processed_common_set_evaluations$target_end_date),
                      value=range(processed_common_set_evaluations$target_end_date)),
          selectInput("excluded_geo_values",
                      'Exclude geo values:',
                      choices=unique(processed_common_set_evaluations$geo_value),
                      multiple=TRUE,
                      selected=c("vi")),
          ),
        mainPanel(
          plotlyOutput("main_plot", height="90em")
        )
      )
    )
  },
  server=function(input, output, session) {
    filtered_scorecards_reactive = reactive({
      ## XXX this is just an experiment with `reactive`; it may or may not be a
      ## good idea. Might speed up computations with the same set of data to
      ## summarize when the summary is changed, but might also eat a bunch of
      ## extra memory.
      processed_common_set_evaluations %>>%
        filter(.data$forecaster %in% .env$input$selected_forecasters,
               .data$forecast_date %>>% between(.env$input$selected_forecast_date_range[[1L]], .env$input$selected_forecast_date_range[[2L]]),
               .data$target_end_date %>>% between(.env$input$selected_target_end_date_range[[1L]], .env$input$selected_target_end_date_range[[2L]]),
               ! .data$geo_value %in% .env$input$excluded_geo_values)
    })
    output$main_plot = renderPlotly({
      filtered_scorecards_reactive() %>>%
        group_by(across(all_of(input$x_var)), across(all_of(input$facet_vars)), forecaster) %>>%
        ## TODO don't need to summarize across all the metrics. just the
        ## requested one. Could make the metric a faceting option with free_y
        summarize(across(c(wis, wis_per_100k, ae, ae_per_100k, coverage_80), list(mean=mean)), n=n(), .groups="drop") %>>%
        (~ plot.df) %>>%
        ggplot(aes_string(input$x_var, input$selected_metric, colour="forecaster")) %>>%
        `+`(expand_limits(y=if (grepl("cov_", input$selected_metric)) c(0,1) else 0)) %>>%
        `+`(geom_hline(linetype="dashed",
                       ## It's natural here to have the default
                       yintercept=switch(input$selected_metric,
                                         coverage_80_mean=0.80,
                                         ## (Avoid
                                         ## https://github.com/plotly/plotly.R/issues/1947
                                         ## by using NA default and na.rm=TRUE
                                         ## rather than numeric(0L) default)
                                         NA_real_),
                       na.rm=TRUE)) %>>%
        {
          if (input$x_var %in% c(input$facet_vars, "geo_value", "forecaster") || range(plot.df[["n"]])%>>%{.[[2L]]>1.2*.[[1L]]}) {
            . + geom_point(aes(size=n)) + expand_limits(size=0)
          }else {
            . + geom_line()
          }
        } %>>%
        `+`(if (length(input$facet_vars) == 0L) {
              theme()
            } else if (length(input$facet_vars) == 1L) {
              facet_wrap(input$facet_vars)
            } else {
              facet_grid(as.formula(paste0(input$facet_vars[[1L]]," ~ ",paste(collapse=" + ",input$facet_vars[-1L]))))
            })%>>%
        ggplotly()
    })
  }
)

# NOTE: While we make predictions on Tuesday, we want to label the file with a Monday, hence the -1.
forecast_date <- as.Date(Sys.getenv("FORECAST_DATE", unset = Sys.Date())) - 1
rmarkdown::render(
  "plot_prediction_cards.Rmd",
  output_file = sprintf("%s-flu-forecast.html", forecast_date)
)

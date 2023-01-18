# NOTE: While we make predictions on Tuesday, we want to label the file with a Monday, hence the -1.
rmarkdown::render(
    "plot_prediction_cards.Rmd",
    output_file = sprintf("%s-flu-forecast.html", Sys.Date() - 1)
)
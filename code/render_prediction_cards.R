rmarkdown::render(
    "plot_prediction_cards.Rmd",
    output_file = sprintf("%s-flu-forecast.html", Sys.Date())
)
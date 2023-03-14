"Forecasting R Dependencies Installer Script"

CRAN_MIRROR = "https://cloud.r-project.org" 
CRAN_PACKAGES = c(
    "assertthat",
    "aws.s3",
    "bettermc",
    "covidcast",
    "data.table",
    "devtools",
    "forecast",
    "fs",
    "gitcreds",
    "glmnet",
    "here",
    "MMWRweek",
    "pipeR",
    "plotly",
    "quantreg",
    "remotes",
    "Rglpk",
    "tidyverse",
    "xts"
)
GITHUB_PACKAGES = c(
    "cmu-delphi/covidcast/R-packages/evalcast@evalcast",
    "cmu-delphi/covid-19-forecast/forecasters/animalia@develop",
    "cmu-delphi/covid-19-forecast/utilities/zookeeper@develop",
    "cmu-delphi/epidatr@dev",
    "cmu-delphi/epiprocess@main",
)

# Cribbed from: https://github.com/eddelbuettel/littler/blob/master/inst/examples/install2.r
install_packages2 <- function(pkgs, ..., error = FALSE, skipinstalled = FALSE) {
    e <- NULL
    capture <- function(e) {
        if (error) {
            catch <-
                grepl("download of package .* failed", e$message) ||
                grepl("(dependenc|package).*(is|are) not available", e$message) ||
                grepl("installation of package.*had non-zero exit status", e$message) ||
                grepl("installation of .+ packages failed", e$message)
            if (catch) {
                e <<- e
            }
        }
    }
    if (skipinstalled) {
        pkgs <- setdiff(pkgs, installed.packages()[,1])
    }
    if (length(pkgs) > 0) {
        withCallingHandlers(install.packages(pkgs, ...), warning = capture)
        if (!is.null(e)) {
            stop(e$message, call. = FALSE)
        }
    }
}

install_packages2(
    CRAN_PACKAGES,
    repos = CRAN_MIRROR,
    error = TRUE,
    skipinstalled = TRUE
)
devtools::install_github(
    GITHUB_PACKAGES,
    repos = CRAN_MIRROR
)


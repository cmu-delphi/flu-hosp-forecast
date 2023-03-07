from os import environ, system

import typer

app = typer.Typer(name="flu-hosp-forecaster", chain=True)


@app.command("forecast")
def make_forecasts(production: bool = False):
    """Make forecasts for our main model."""
    if production:
        environ["FLU_CACHE"] = "production"

    system("Rscript train_model.R")


@app.command("postprocess")
def postprocess_forecasts(production: bool = False):
    """Postprocess forecasts for our main model."""
    if production:
        environ["FLU_CACHE"] = "production"

    system("Rscript postprocess_forecasts.R")


@app.command("notebook")
def make_notebook(production: bool = False):
    """Make a notebook for our main model."""
    if production:
        environ["FLU_CACHE"] = "production"

    system("Rscript render_prediction_cards.R")


if __name__ == "__main__":
    app()

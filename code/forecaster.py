import os
import subprocess

import typer

app = typer.Typer(name="flu-hosp-forecaster", chain=True)


@app.command("forecast")
def make_forecasts(production: bool = False):
    """Make forecasts for our main model."""
    if production:
        os.environ["FLU_CACHE"] = "production"

    subprocess.run(["Rscript", "train_model.R"], check=True)


@app.command("postprocess")
def postprocess_forecasts(production: bool = False):
    """Postprocess forecasts for our main model."""
    if production:
        os.environ["FLU_CACHE"] = "production"

    subprocess.run(["Rscript", "postprocess_forecasts.R"], check=True)


@app.command("notebook")
def make_notebook(production: bool = False):
    """Make a notebook for our main model."""
    if production:
        os.environ["FLU_CACHE"] = "production"

    subprocess.run(["Rscript", "render_prediction_cards.R"], check=True)


if __name__ == "__main__":
    app()

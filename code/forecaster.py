import os

import rpy2.robjects as robjects
import typer

app = typer.Typer(name="flu-hosp-forecaster", chain=True)
CLEAR_R_STATE = "rm(list=ls())"


def run_r_script(script_name: str, clear_r_session: bool = True):
    """Run an R script."""
    try:
        robjects.r.source(script_name)
    except Exception as e:
        raise typer.Abort(f"Error running {script_name}: {e}")
    finally:
        if clear_r_session:
            robjects.r(CLEAR_R_STATE)


def run_r_command(command: str, clear_r_session: bool = True):
    """Run an R command."""
    try:
        robjects.r(command)
    except Exception as e:
        raise typer.Abort(f"Error running {command}: {e}")
    finally:
        if clear_r_session:
            robjects.r(CLEAR_R_STATE)


@app.command("forecast")
def make_forecasts(production: bool = False):
    """Make forecasts for our main model."""
    if production:
        os.environ["FLU_CACHE"] = "production"

    run_r_script("train_model.R")


@app.command("postprocess")
def postprocess_forecasts(production: bool = False):
    """Postprocess forecasts for our main model."""
    if production:
        os.environ["FLU_CACHE"] = "production"

    run_r_script("postprocess_forecasts.R")


@app.command("notebook")
def make_notebook(production: bool = False):
    """Make a notebook for our main model."""
    if production:
        os.environ["FLU_CACHE"] = "production"

    run_r_script("render_prediction_cards.R")


if __name__ == "__main__":
    app()

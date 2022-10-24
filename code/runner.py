from os import system

import typer

app = typer.Typer()

@app.command()
def make_forecasts():
    """Make forecasts for our main model."""
    system("Rscript train_model.R")

@app.command()
def make_notebook():
    """Make a notebook for our main model."""
    system("Rscript render_prediction_cards.R")

if __name__ == "__main__":
    app()

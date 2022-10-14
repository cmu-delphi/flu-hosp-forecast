from os import system

import hug


@hug.cli()
@hug.get()
def make_forecasts():
    """Make forecasts for our main model."""
    system("Rscript train_model.R")

@hug.cli()
@hug.get()
def make_notebook():
    """Make a notebook for our main model."""
    system("Rscript render_prediction_cards.R")

if __name__ == "__main__":
    make_forecasts.interface.cli()
    make_notebook.interface.cli()

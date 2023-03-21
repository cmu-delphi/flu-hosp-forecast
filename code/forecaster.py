"""
Forecaster Runner Utility

See python forecaster.py --help for usage.

This runner acts as a friendly interface to a set of R forecasting scripts:

  train_model.R
  postprocess_forecasts.R
  render_prediction_cards.R

See their docstrings for info.
"""

import os
import re
import shutil
import subprocess
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path

import git
import typer
from dotenv import load_dotenv, set_key
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError

app = typer.Typer(name="flu-hosp-forecaster", chain=True)
load_dotenv()


def get_next_weekday(cur_date: datetime, weekday: int) -> datetime:
    """Get date of next given weekday.

    If cur_date is the weekday, return the same cur_date. Weekday 0 is Monday.

    weekday == cur_date.weekday()     => timedelta(0)
    weekday == cur_date.weekday() - 1 => timedelta(6)
    ...
    weekday == cur_date.weekday() - 6 => timedelta(1)"""
    return cur_date + timedelta((weekday - cur_date.weekday()) % 7)


def get_previous_weekday(cur_date: datetime, weekday: int) -> datetime:
    """Get date of previous weekday.

    If cur_date is the weekday, return the same cur_date. Weekday 0 is Monday.
    """
    return cur_date - timedelta((7 - weekday + cur_date.weekday()) % 7)


DATE_FORMAT = "%Y-%m-%d"
NEAREST_TUESDAY = get_previous_weekday(datetime.today(), 1)
os.environ["FORECAST_DATE"] = os.environ.get("FORECAST_DATE", NEAREST_TUESDAY.strftime(DATE_FORMAT))
NOMINAL_FORECAST_DATE = NEAREST_TUESDAY - timedelta(days=1)
NOMINAL_FORECAST_DATE_STR = NOMINAL_FORECAST_DATE.strftime(DATE_FORMAT)
TODAY = datetime.today()
TODAY_STR = TODAY.strftime(DATE_FORMAT)


@dataclass
class ForecasterPaths:
    FLU_SUBMISSION_DIR: Path
    FLU_DIRECTION_SUBMISSION_DIR: Path
    FLU_PREDICTIONS_FILE: Path
    FLU_DIRECTION_PREDICTIONS_FILE: Path
    FLU_PREDICTIONS_NOTEBOOK: Path


def get_forecaster_paths():
    """Get paths to the COVID and flu data and notebooks."""
    return ForecasterPaths(
        Path(os.environ.get("FLU_SUBMISSIONS_PATH", "")) / "data-forecasts" / "CMU-TimeSeries",
        Path(os.environ.get("FLU_SUBMISSIONS_PATH", "")) / "data-experimental" / "CMU-TimeSeries",
        Path(os.getcwd()) / "data-forecasts" / "CMU-TimeSeries" / f"{NOMINAL_FORECAST_DATE_STR}-CMU-TimeSeries.csv",
        Path(os.getcwd())
        / "data-forecasts"
        / "direction-predictions"
        / f"generated-{TODAY_STR}"
        / f"{NOMINAL_FORECAST_DATE_STR}-CMU-TimeSeries.csv",
        Path(os.getcwd()) / f"{NOMINAL_FORECAST_DATE_STR}-flu-forecast.html",
    )


FORECASTER_PATHS = get_forecaster_paths()


@app.command("install")
def install():
    """Install the R dependencies for the COVID hospitalization forecaster."""
    subprocess.run(["Rscript", "production-scripts/install.R"], check=True)


def check_exists(path: Path):
    if not path.exists():
        raise FileNotFoundError(f"{path} not found.")


def check_and_set_var(key: str, msg: str, force: bool = False):
    if not os.environ.get(key) or force:
        print(f"Your {key} env var is not set.")
        path = input(f"{msg}")
        check_exists(Path(path))
        set_key(".env", key, path, export=True)


@app.command("set-vars")
def setup_vars(force: bool = False):
    """Set the environment variables for the forecaster utility."""
    print("Checking and setting environment variables... (press Enter to skip any variable)")
    check_and_set_var(
        "FLU_SUBMISSIONS_PATH",
        "Please enter the path to 'Flusight-forecast-data' (e.g. /Users/username/Documents/Flusight-forecast-data): ",
        force=force,
    )
    check_and_set_var("SLACK_BOT_TOKEN", "Please enter your Slack bot token: ", force=force)


def copy_to_repo(directions: bool = False):
    """Copy predictions to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    """
    if not directions:
        shutil.copy(FORECASTER_PATHS.FLU_PREDICTIONS_FILE, FORECASTER_PATHS.FLU_SUBMISSION_DIR)
    else:
        shutil.copy(
            FORECASTER_PATHS.FLU_DIRECTION_PREDICTIONS_FILE,
            FORECASTER_PATHS.FLU_DIRECTION_SUBMISSION_DIR,
        )


def get_latest_commit_date(repo: git.Repo) -> datetime:
    """Search for the last submission by the CMU-TimeSeries group and extract the date."""
    pattern = r"\[CMU-TimeSeries\] Add (\d+-\d+-\d+) predictions"

    for commit in repo.iter_commits():
        if date_match := re.search(pattern, commit.message):
            return datetime.strptime(date_match.groups()[0], "%Y-%m-%d")

    raise ValueError("No previous commits by CMU-TimeSeries found.")


def make_new_branch(repo: git.Repo, branch_name: str):
    """Create a new branch in the repo."""
    new_branch = repo.create_head(branch_name)
    return new_branch


def switch_to_branch(repo: git.Repo, branch_name: str):
    """Switch to a branch in the repo."""
    repo.heads[branch_name].checkout()
    repo.head.reset(index=True, working_tree=True)


def commit_to_repo(directions: bool = False):
    """Commit to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_PATH"])
    assert flu_submissions_repo.remote(name="origin").exists()

    latest_commit_date = get_latest_commit_date(flu_submissions_repo)
    if latest_commit_date < NOMINAL_FORECAST_DATE:
        if not directions:
            # Can't use .index because we're using a sparse index. So we use the CLI wrapper .git instead.
            flu_submissions_repo.git.add(str(FORECASTER_PATHS.FLU_SUBMISSION_DIR / f"{NOMINAL_FORECAST_DATE_STR}-CMU-TimeSeries.csv"))
            flu_submissions_repo.git.commit("-m", f"[CMU-TimeSeries] Add {NOMINAL_FORECAST_DATE_STR} predictions")
        else:
        # Can't use .index because we're using a sparse index. So we use the CLI wrapper .git instead.
            flu_submissions_repo.git.add(str(FORECASTER_PATHS.FLU_DIRECTION_SUBMISSION_DIR / f"{NOMINAL_FORECAST_DATE_STR}-CMU-TimeSeries.csv"))
            flu_submissions_repo.git.commit("-m", f"[CMU-TimeSeries] Add {NOMINAL_FORECAST_DATE_STR} direction predictions")
    else:
        print(f"Latest commit is for {latest_commit_date.strftime(DATE_FORMAT)}; skipping commit.")


def push_to_repo():
    """Push to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_PATH"])
    assert flu_submissions_repo.remote(name="origin").exists()

    flu_submissions_repo.remote(name="origin").push()


@app.command("submit")
def submit(directions: bool = False):
    """Submit forecasts to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    Directions are submitted if the --directions flag is set.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_PATH"])
    assert flu_submissions_repo.remote(name="origin").exists()
    assert flu_submissions_repo.active_branch.name == "master"

    flu_submissions_repo.remote(name="origin").pull()

    if not directions:
        make_new_branch(flu_submissions_repo, f"forecast-{NOMINAL_FORECAST_DATE_STR}")
        switch_to_branch(flu_submissions_repo, f"forecast-{NOMINAL_FORECAST_DATE_STR}")
    else:
        make_new_branch(flu_submissions_repo, f"forecast-directions-{NOMINAL_FORECAST_DATE_STR}")
        switch_to_branch(flu_submissions_repo, f"forecast-directions-{NOMINAL_FORECAST_DATE_STR}")

    copy_to_repo(directions=directions)
    commit_to_repo(directions=directions)
    push_to_repo()

    switch_to_branch(flu_submissions_repo, "master")


def upload_file_to_slack(client: WebClient, file_path: str, slack_file_name: str) -> str:
    result = client.files_upload(file=file_path, title=slack_file_name)
    if not result.get("ok"):
        raise SlackApiError("Couldn't upload file.", result)
    else:
        file_link = result.get("file").get("permalink")
        return file_link


def post_upload_message(client: WebClient, text: str, channel: str):
    result = client.chat_postMessage(channel=channel, text=text)
    if not result.get("ok"):
        raise SlackApiError("Couldn't post message.", result)


def get_hyperlink_text(file_link: str, hyperlink_title: str) -> str:
    """Get the text for a hyperlink in Slack."""
    return f"<{file_link}|{hyperlink_title}>"


@app.command("post-slack")
def post_notebook_to_slack(test_mode: bool = False):
    """Post the Flu notebook and prediction csv to Slack."""
    if token := os.environ.get("SLACK_BOT_TOKEN"):
        client = WebClient(token=token)
    else:
        raise ValueError("SLACK_BOT_TOKEN not found.")

    SLACK_CHANNEL = "C03SD5K905D" if test_mode else "C03A46R6LBW"
    LOGAN_ID = "UFABH1A4V"
    DMITRY_ID = "U018LKL6W91"

    notebook_link = upload_file_to_slack(
        client,
        str(FORECASTER_PATHS.FLU_PREDICTIONS_NOTEBOOK),
        FORECASTER_PATHS.FLU_PREDICTIONS_NOTEBOOK.name,
    )
    csv_link = upload_file_to_slack(
        client,
        str(FORECASTER_PATHS.FLU_PREDICTIONS_FILE),
        FORECASTER_PATHS.FLU_PREDICTIONS_FILE.name,
    )
    csv_link2 = upload_file_to_slack(
        client,
        str(FORECASTER_PATHS.FLU_DIRECTION_PREDICTIONS_FILE),
        FORECASTER_PATHS.FLU_DIRECTION_PREDICTIONS_FILE.name,
    )

    hyperlinks = get_hyperlink_text(notebook_link, "Notebook") + " " + get_hyperlink_text(csv_link, "CSV") + " " + get_hyperlink_text(csv_link2, "Direction CSV")
    text = f"Flu: {hyperlinks} for {NOMINAL_FORECAST_DATE_STR} <@{DMITRY_ID}> <@{LOGAN_ID}>."

    post_upload_message(client, text, channel=SLACK_CHANNEL)


@app.command("forecast")
def make_forecasts():
    """Make flu forecasts for the previous Monday (possibly today).

    Output is placed in data-forecasts/CMU-TimSeries/.
    """
    # Set this to "production", to use the production cache
    os.environ["FLU_CACHE"] = os.environ.get("FLU_CACHE", "exploration")
    subprocess.run(["Rscript", "train_model.R"], check=True)


@app.command("forecast-direction")
def make_forecast_directions():
    """Make flu direction forecasts for the previous Monday (possibly today).

    Output is placed in data-forecasts/generated-YYYY-MM-DD-as-of-YYYY-MM-DD/.
    """
    os.environ["FLU_CACHE"] = os.environ.get("FLU_CACHE", "exploration")
    subprocess.run(["Rscript", "naive_direction_forecaster.R"], check=True)


@app.command("postprocess")
def postprocess_forecasts():
    """Postprocess forecasts for our main model."""
    os.environ["FLU_CACHE"] = os.environ.get("FLU_CACHE", "exploration")
    subprocess.run(["Rscript", "postprocess_forecasts.R"], check=True)


@app.command("notebook")
def make_notebook():
    """Make a markdown notebook for our main model."""
    os.environ["FLU_CACHE"] = os.environ.get("FLU_CACHE", "exploration")
    subprocess.run(["Rscript", "render_prediction_cards.R"], check=True)


if __name__ == "__main__":
    app()

"""
Forecaster Runner Utility

This utility mostly handles the process of copying the submission files,
committing to the repo, and posting to Slack. It can also generate the
forecasts.

See python forecaster.py --help for usage.
"""

import os
import re
import shutil
import subprocess
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
# The generation date is usually today.
FORECAST_GENERATION_DATE = datetime.today()
# The due date is the date of the forecast, which should be a Wednesday. It is
# also the effective as of date for requesting API data.
FORECAST_DUE_DATE = get_previous_weekday(FORECAST_GENERATION_DATE, 2)
# The reference date is the Saturday after the due date.
REFERENCE_DATE = get_next_weekday(FORECAST_DUE_DATE, 5)

FLU_SUBMISSION_DIR = (
    Path(os.environ.get("FLU_SUBMISSIONS_PATH", ""))
    / "data-forecasts"
    / "CMU-TimeSeries"
)
FLU_DIRECTION_SUBMISSION_DIR = (
    Path(os.environ.get("FLU_SUBMISSIONS_PATH", ""))
    / "data-experimental"
    / "CMU-TimeSeries"
)
FLU_PREDICTIONS_FILE = (
    Path(os.getcwd())
    / "data-forecasts"
    / f"{REFERENCE_DATE:%Y-%m-%d}-CMU-TimeSeries.csv"
)
FLU_PREDICTIONS_NOTEBOOK = (
    Path(os.getcwd()) / f"{REFERENCE_DATE:%Y-%m-%d}-flu-forecast.html"
)


@app.command("forecast")
def make_forecasts():
    """Make flu forecasts for the previous Monday (possibly today).

    Output is placed in data-forecasts/CMU-TimSeries/.
    """
    # Set this to "production", to use the production cache
    os.environ["FLU_CACHE"] = os.environ.get("FLU_CACHE", "exploration")
    subprocess.run(["Rscript", "run.R"], check=True)


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
def set_vars(force: bool = False):
    """Set the environment variables for the forecaster utility."""
    print(
        "Checking and setting environment variables... (press Enter to skip any variable)"
    )
    check_and_set_var(
        "FLU_SUBMISSIONS_PATH",
        "Please enter the path to 'Flusight-forecast-data' (e.g. /Users/username/Documents/Flusight-forecast-data): ",
        force=force,
    )
    check_and_set_var(
        "SLACK_BOT_TOKEN", "Please enter your Slack bot token: ", force=force
    )


def copy_to_repo():
    """Copy predictions to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    """
    shutil.copy(FLU_PREDICTIONS_FILE, FLU_SUBMISSION_DIR)


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


def commit_to_repo():
    """Commit to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.

    Can't use git.index because we're using a sparse index. So we use the CLI
    wrapper in git instead.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_PATH"])
    assert flu_submissions_repo.remote(name="origin").exists()

    latest_commit_date = get_latest_commit_date(flu_submissions_repo)
    if latest_commit_date < REFERENCE_DATE:
        flu_submissions_repo.git.add(
            str(FLU_SUBMISSION_DIR / f"{REFERENCE_DATE:%Y-%m-%d}-CMU-TimeSeries.csv")
        )
        flu_submissions_repo.git.commit(
            "-m", f"[CMU-TimeSeries] Add {REFERENCE_DATE:%Y-%m-%d} predictions"
        )
    else:
        print(
            f"Latest commit is for {latest_commit_date.strftime(DATE_FORMAT)}; not committing."
        )


def push_to_repo():
    """Push to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_PATH"])
    assert flu_submissions_repo.remote(name="origin").exists()
    branch_name = flu_submissions_repo.active_branch.name

    flu_submissions_repo.remote(name="origin").push(
        f"refs/heads/{branch_name}:refs/heads/{branch_name}"
    )


@app.command("submit")
def submit():
    """Submit forecasts to the Flusight-forecast-data repo.

    The repo path is specified in the env var FLU_FORECASTER_PATH.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_PATH"])
    assert flu_submissions_repo.remote(name="origin").exists()
    assert flu_submissions_repo.active_branch.name == "master"

    flu_submissions_repo.remote(name="origin").pull()

    make_new_branch(flu_submissions_repo, f"forecast-{REFERENCE_DATE:%Y-%m-%d}")
    switch_to_branch(flu_submissions_repo, f"forecast-{REFERENCE_DATE:%Y-%m-%d}")

    copy_to_repo()
    commit_to_repo()
    push_to_repo()

    switch_to_branch(flu_submissions_repo, "master")


def upload_file_to_slack(
    client: WebClient, file_path: str, slack_file_name: str
) -> str:
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
    ROLE_TAG_STRING = "<!subteam^S05AN3WH91V|covid-forecast-submission>"

    notebook_link = upload_file_to_slack(
        client,
        str(FLU_PREDICTIONS_NOTEBOOK),
        FLU_PREDICTIONS_NOTEBOOK.name,
    )
    csv_link = upload_file_to_slack(
        client,
        str(FLU_PREDICTIONS_FILE),
        FLU_PREDICTIONS_FILE.name,
    )

    hyperlinks = " ".join(
        get_hyperlink_text(notebook_link, "Notebook"),
        get_hyperlink_text(csv_link, "CSV"),
    )
    text = f"Flu: {hyperlinks} for {FORECAST_DUE_DATE:%Y-%m-%d} {ROLE_TAG_STRING}"

    post_upload_message(client, text, channel=SLACK_CHANNEL)


if __name__ == "__main__":
    app()

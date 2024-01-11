"""
Forecaster Runner Utility

See python run.py --help for usage.

This utility mostly handles the process of copying the submission files,
committing to the repo, and posting to Slack. It can also generate the
forecasts.

The generation date is usually today. The due date is the date of the forecast,
which should be the most recent next Wednesday (it is also the effective as of
date for requesting API data). The reference date is the Saturday after the due
date.

To generate forecasts simply run:

    python run.py forecast

After setting geo exclusions in run.R, you can run the same command again to
generate excluded forecasts.

Necessary environment variables (set in .env):

    * FLU_SUBMISSIONS_DIR is the submission repo for the Flu Forecast Hub,
      which you can clone from https://github.com/cdcepi/FluSight-forecast-hub/
    * SLACK_BOT_TOKEN is the token for the Slack bot that will post the
      forecasts to Slack.
"""

import os
import re
import shutil
import subprocess
from datetime import datetime, timedelta
from pathlib import Path

import git
import typer
from dotenv import load_dotenv
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError

app = typer.Typer(
    name="flu-hosp-forecaster", chain=True, help="Forecaster runner utility."
)
load_dotenv()


def get_next_weekday(cur_date: datetime, weekday: int) -> datetime:
    """Get date of next given weekday.

    If cur_date is the weekday, return the same cur_date. Weekday 0 is Monday.

    weekday == cur_date.weekday()     => timedelta(0)
    weekday == cur_date.weekday() - 1 => timedelta(6)
    ...
    weekday == cur_date.weekday() - 6 => timedelta(1)
    """
    return cur_date + timedelta((weekday - cur_date.weekday()) % 7)


def get_previous_weekday(cur_date: datetime, weekday: int) -> datetime:
    """Get date of previous weekday.

    If cur_date is the weekday, return the same cur_date. Weekday 0 is Monday.
    """
    return cur_date - timedelta((7 - weekday + cur_date.weekday()) % 7)


FORECAST_GENERATION_DATE = datetime.today()
FORECAST_DUE_DATE = get_next_weekday(FORECAST_GENERATION_DATE, 2)
REFERENCE_DATE = get_next_weekday(FORECAST_DUE_DATE, 5)
FLU_PREDICTIONS_FILE = (
    Path(os.getcwd())
    / "data-forecasts"
    / f"{REFERENCE_DATE:%Y-%m-%d}-CMU-TimeSeries.csv"
)
FLU_PREDICTIONS_NOTEBOOK = (
    Path(os.getcwd())
    / "data-forecasts"
    / f"{REFERENCE_DATE:%Y-%m-%d}-flu-forecast.html"
)
if not os.environ.get("FLU_SUBMISSIONS_DIR"):
    raise ValueError("FLU_SUBMISSIONS_DIR not found. See script docstring for info.")
FLU_SUBMISSION_DIR = (
    Path(os.environ.get("FLU_SUBMISSIONS_DIR", "")) / "model-output" / "CMU-TimeSeries"
)
if not FLU_PREDICTIONS_FILE.exists():
    raise FileNotFoundError(f"Flu predictions file {FLU_PREDICTIONS_FILE} not found.")
if not FLU_SUBMISSION_DIR.exists():
    os.makedirs(FLU_SUBMISSION_DIR)


@app.command("forecast")
def make_forecasts(
    clear_cache: bool = typer.Option(
        False, "--clear-cache", "-c", help="Clear the caches before making forecasts."
    )
):
    """Make flu hospitalization forecasts."""
    if clear_cache:
        os.environ["FLU_HOSP_CLEAR_CACHE"] = "TRUE"
    subprocess.run(["Rscript", "run.R"], check=True)


def copy_to_repo():
    """Copy predictions to the submission repo."""
    shutil.copy(FLU_PREDICTIONS_FILE, FLU_SUBMISSION_DIR)


def get_latest_commit_date(repo: git.Repo) -> datetime:
    """Search for the last submission by the CMU-TimeSeries group and extract the date."""
    pattern = r"\[CMU-TimeSeries\] Add (\d+-\d+-\d+) predictions"
    for commit in repo.iter_commits():
        if date_match := re.search(pattern, commit.message):
            return datetime.strptime(date_match.groups()[0], "%Y-%m-%d")

    return datetime(1970, 1, 1)


def make_new_branch(repo: git.Repo, branch_name: str):
    """Create a new branch in the repo."""
    new_branch = repo.create_head(branch_name)
    return new_branch


def switch_to_branch(repo: git.Repo, branch_name: str):
    """Switch to a branch in the repo."""
    repo.heads[branch_name].checkout()
    repo.head.reset(index=True, working_tree=True)


def commit_to_repo():
    """Commit to the submission repo.

    Can't use git.index because we're using a sparse index. So we use the CLI
    wrapper in git instead.
    """
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_DIR"])
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
            f"Latest commit is for {latest_commit_date.strftime('%Y-%m-%d')}; not committing."
        )


def push_to_repo():
    """Push to the submission repo."""
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_DIR"])
    assert flu_submissions_repo.remote(name="origin").exists()
    branch_name = flu_submissions_repo.active_branch.name

    flu_submissions_repo.remote(name="origin").push(
        f"refs/heads/{branch_name}:refs/heads/{branch_name}"
    )


@app.command("submit")
def submit_forecasts():
    """Copy, commit, and push to the submission repo."""
    flu_submissions_repo = git.Repo(os.environ["FLU_SUBMISSIONS_DIR"])
    assert flu_submissions_repo.remote(name="origin").exists()
    assert flu_submissions_repo.active_branch.name == "main"
    flu_submissions_repo.remote(name="origin").pull()

    # make_new_branch(flu_submissions_repo, f"forecast-{REFERENCE_DATE:%Y-%m-%d}")
    # switch_to_branch(flu_submissions_repo, f"forecast-{REFERENCE_DATE:%Y-%m-%d}")

    copy_to_repo()
    commit_to_repo()
    push_to_repo()

    # switch_to_branch(flu_submissions_repo, "main")


@app.command("post-slack")
def post_notebook_to_slack(test_mode: bool = False):
    """Post the notebook and prediction csv to Slack."""
    if token := os.environ.get("SLACK_BOT_TOKEN"):
        client = WebClient(token=token)
    else:
        raise ValueError("SLACK_BOT_TOKEN not found. See script docstring for info.")

    # Upload notebook
    result = client.files_upload_v2(
        file=str(FLU_PREDICTIONS_NOTEBOOK), title=FLU_PREDICTIONS_NOTEBOOK.name
    )
    if not result.get("ok"):
        raise SlackApiError("Couldn't upload file.", result)
    else:
        notebook_link = result.get("file").get("permalink")

    # Upload csv
    result = client.files_upload_v2(
        file=str(FLU_PREDICTIONS_FILE), title=FLU_PREDICTIONS_FILE.name
    )
    if not result.get("ok"):
        raise SlackApiError("Couldn't upload file.", result)
    else:
        csv_link = result.get("file").get("permalink")

    # Form the message
    hyperlinks = f"<{notebook_link}|Notebook> <{csv_link}|CSV>"
    role_tag_string = "<!subteam^S05AN3WH91V|covid-forecast-submission>"
    text = f"Flu: {hyperlinks} for {FORECAST_DUE_DATE:%Y-%m-%d} {role_tag_string}"

    # Post the message
    result = client.chat_postMessage(
        channel="C03SD5K905D" if test_mode else "C03A46R6LBW", text=text
    )
    if not result.get("ok"):
        raise SlackApiError("Couldn't post message.", result)


if __name__ == "__main__":
    app()

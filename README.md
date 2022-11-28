# flu-hosp-forecast

## Setup

Setup a virtual environment and install the requirements:

```sh
cd code
python3 -m venv venv
pip install -r requirements.txt
source venv/bin/activate
```

## Runnning The Code

You can use the CLI utility in the `code/` directory.
Enter that dir and

```sh
# Run the forecasts and make an evaluation notebook (using exploration cache)
python forecaster.py forecast notebook
# Use the production cache
python forecaster.py forecast --production notebook --production
# See the CLI help message
python forecaster.py --help
```

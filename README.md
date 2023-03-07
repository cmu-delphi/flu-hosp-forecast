# flu-hosp-forecast

Flu hospitalization forecaster for the Delphi team.
Submitted to the [Flusight repo](https://github.com/cmu-delphi/Flusight-forecast-data).

## Usage

You can use the CLI utility in the `code/` directory.

```sh
# Setup Python.
cd code
python3 -m venv venv
pip install -r requirements.txt
source venv/bin/activate

# Run the forecasts and make an evaluation notebook (using exploration cache)
python forecaster.py forecast postprocess notebook

# Run the forecasts on a given date (make sure it's a Tuesday)
FORECAST_DATE=2023-02-28 python forecaster.py forecast postprocess notebook

# Use the production cache
python forecaster.py forecast --production postprocess --production notebook --production

# See the CLI help message
python forecaster.py --help
```

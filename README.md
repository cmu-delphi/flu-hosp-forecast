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
python forecaster.py forecast notebook

# Use the production cache
python forecaster.py forecast --production notebook --production

# See the CLI help message
python forecaster.py --help
```

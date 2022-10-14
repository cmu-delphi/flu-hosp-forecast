# flu-hosp-forecast

## Setup

Install the Python dependencies

```sh
pip install -r requirements.txt
```

## Runner

To produce forecasts, from the `code/` directory run

```sh
hug -f runner.py -c make_forecasts
```

To make a notebook with forecast plots, from the `code/` directory run

```sh
hug -f runner.py -c make_notebook
```

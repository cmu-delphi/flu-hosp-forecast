SHELL=/bin/bash
TODAY := $(shell date '+%Y-%m-%d')

forecasts:
	Rscript ./train_model.R >> forecast_$(TODAY).log 2>&1 &

plots: 
	Rscript ./render_prediction_cards.R >> forecast_$(TODAY).log 2>&1 &

all: forecasts plots
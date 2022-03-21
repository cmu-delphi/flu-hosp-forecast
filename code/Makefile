SHELL=/bin/bash
TODAY := $(shell date '+%Y-%m-%d')

forecasts:
	(Rscript ./train_model.R && Rscript ./render_prediction_cards.R) >> forecast_$(TODAY).log 2>&1 &
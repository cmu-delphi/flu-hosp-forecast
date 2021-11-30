library(tibble)
library(dplyr)
library(evalcast)
library(covidcast)

actuals = readRDS('actuals.RDS')

tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

# Read baseline
baseline_preds = readRDS('predictions/preds_Baseline.RDS')

# Subset baseline quantiles (properly!)
baseline_preds_subset = baseline_preds %>% filter (
    as.character(quantile*1000) %in% as.character(tau*1000),
  )

# Sanity check

baseline_preds_subset %>% summarize (
    p_na = mean(is.na(value)),
    count = n(),
  )

baseline_preds_subset %>% filter (
    is.na(value),
  ) 

baseline_preds_subset %>% group_by (
    quantile,
  ) %>% summarize (
    count = n(),
  )

# Ingest AR predictions

ar_preds = readRDS('predictions/preds_AR.RDS')

ar_preds %>% summarize (
    p_na = mean(is.na(value)),
    count = n(),
  )

preds = bind_rows(baseline_preds_subset, ar_preds)

preds %>% summarize (p_na = mean(is.na(value)))

preds %>% group_by (
        forecaster,
      ) %>% summarize (
        count = n(),
        n_preds = n() / 7,
      )

preds %>% group_by (
        quantile,
      ) %>% summarize (
        count = n(),
      )

preds$quantile = signif(preds$quantile, 4)

preds %>% group_by (
        quantile,
      ) %>% summarize (
        count = n(),
      )

saveRDS(preds, 'predictions/predictions.RDS')

preds = readRDS('predictions/predictions.RDS')

preds = preds %>% mutate (
    target_end_date = forecast_date + ahead,
)

grp_vars = intersect(colnames(preds), colnames(actuals))

preds_card <- preds %>% select(
     -data_source,
    -signal,
    -incidence_period,
  )

preds_card  = preds

preds_card <- left_join(preds_card, actuals)

results <- preds_card %>% group_by (
    forecaster,
    geo_value,
    ahead,
    forecast_date,
    target_end_date,
  )

sc_keys <- results %>% group_keys()

results <- results %>% 
  group_split() %>% 
  lapply(evalcast:::erm, 
         list(wis=weighted_interval_score, ae = absolute_error)) %>%
  bind_rows()

results = bind_cols(sc_keys, results)

saveRDS(results, 'results/results.RDS')

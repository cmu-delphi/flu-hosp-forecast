library(tibble)
library(dplyr)
library(evalcast)
library(covidcast)

actuals = readRDS('actuals.RDS')

# Read baseline
baseline_preds = readRDS('predictions/preds_Baseline.RDS')

preds_files = list.files('predictions/')

preds_list = vector('list', length(preds_files))

for (idx in 1:length(preds_list)) {
  fi = preds_files[idx]
  preds = readRDS(sprintf('predictions/%s', fi))
  preds %>% summarize (
      p_na = mean(is.na(value)),
      count = n(),
    )
  preds %>% filter (
      is.na(value),
    ) 
  preds %>% group_by (
      quantile,
    ) %>% summarize (
      count = n(),
    )
  preds_list[[idx]] = preds
}


preds = bind_rows(preds_list)

preds %>% summarize (p_na = mean(is.na(value)))

preds %>% group_by (
        forecaster,
      ) %>% summarize (
        count = n(),
        n_preds = n() / 7,
      )

preds %>% filter (
        !is.na(quantile),
      ) %>% group_by (
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
      ) %>% print (
        n = 24
      )

saveRDS(preds, 'predictions/predictions.RDS')

preds = readRDS('predictions/predictions.RDS')

# Only using 23 quantile predictions from here forth

preds %>% group_by (
        forecaster,
    ) %>% summarize (
        count = n(),
    )

preds = preds %>% filter (
        (forecaster == 'Baseline')|(stringr::str_ends(forecaster, '_23')),
    )

preds %>% group_by (
        forecaster,
    ) %>% summarize (
        count = n(),
    )


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
  parallel::mclapply(evalcast:::erm, 
         list(wis=weighted_interval_score,
              cov80 = interval_coverage(0.8),
              ae = absolute_error),
         mc.cores=parallel::detectCores()) %>%
  bind_rows()

results = bind_cols(sc_keys, results)

saveRDS(results, 'results/results_23.RDS')

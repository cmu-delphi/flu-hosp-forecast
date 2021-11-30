library(covidcast)
library(dplyr)
library(tidyr)
library(tibble)

states_dc_pr_vi = c('al', 'ak', 'az', 'ar', 'ca', 'co', 'ct', 'dc', 'de', 'fl',
                    'ga', 'hi', 'id', 'il', 'in', 'ia', 'ks', 'ky', 'la', 'me',
                    'md', 'ma', 'mi', 'mn', 'ms', 'mo', 'mt', 'ne', 'nv', 'nh',
                    'nj', 'nm', 'ny', 'nc', 'nd', 'oh', 'ok', 'or', 'pa', 'ri',
                    'sc', 'sd', 'tn', 'tx', 'ut', 'vt', 'va', 'wa', 'wv', 'wi',
                    'wy', 'pr', 'vi')
start_day =  '2020-05-01'
end_day =    '2021-12-01'
geo_type = 'state'

hhs_source = 'hhs'
flu_hosp_prop = 'confirmed_admissions_influenza_1d_prop_7dav'

actuals  = covidcast_signal(data_source = hhs_source,
                            signal = flu_hosp_prop,
                            start_day = start_day,
                            end_day = end_day,
                            geo_type = geo_type
    ) %>% tibble (
    ) %>% select (
      geo_value,
      target_end_date=time_value,
      actual=value,
    )

saveRDS(actuals, 'actuals.RDS')

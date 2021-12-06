library(covidcast)
library(dplyr)
library(tidyr)
library(tibble)

source('common_params.R')

start_day =  '2020-05-01'
end_day =    last_eval_date

hhs_source = response_data_source
flu_hosp_prop = response_signal

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

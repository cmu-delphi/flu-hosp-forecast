library(covidcast)
library(dplyr)
library(tidyr)
library(ggplot2)

results = readRDS('results/results.RDS')

results

plt = results %>% group_by (
      forecaster,
      ahead,
    ) %>% summarize (
      mean_wis = mean(wis),
    ) %>% ggplot (
      aes(x=ahead, y=mean_wis, color=forecaster)
    ) + geom_point (
    ) + geom_line (
    )

plt

state_pop = covidcast::state_census %>% tibble (
    ) %>% transmute (
      geo_value = stringr::str_to_lower(ABBR),
      pop = POPESTIMATE2019,
    )

results = results %>% left_join (
      state_pop,
      by='geo_value',
    ) %>% mutate (
      pop_wis = wis * pop,
      pop_ae = ae * pop,
    )


plt = results %>% group_by (
      forecaster,
      ahead,
    ) %>% summarize (
      mean_pop_wis = mean(pop_wis) / 1e6,
    ) %>% ggplot (
      aes(x=ahead, y=mean_pop_wis, color=forecaster)
    ) + geom_point (
    ) + geom_line (
    )

plt

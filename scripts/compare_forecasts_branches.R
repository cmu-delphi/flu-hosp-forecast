source(here::here("R", "load_all.R"))

a <- read_csv("2024-01-13-CMU-TimeSeries-unfiltered.csv")
b <- read_csv("data-forecasts/2024-01-13-CMU-TimeSeries-unfiltered.csv")

a %<>% select(reference_date, target_end_date, geo_value, output_type_id, value)
b %<>% select(reference_date, target_end_date, geo_value, output_type_id, value)

# Join the two
diff <- a %>%
  full_join(b, by = c("reference_date", "target_end_date", "geo_value", "output_type_id")) %>%
  mutate(diff = abs(value.x - value.y)) %>%
  filter(diff != 0) %>%
  arrange(desc(diff))
diff %>% print(n = 100)

from datetime import date, timedelta

today = date.today()
max_ahead = 28

today.strftime("%Y-%m-%d")

last_eval_date = today - timedelta(days=1)
last_forecast_date = last_eval_date - timedelta(days=max_ahead + 1)

output_str = f"""
forecast_dates <- seq(as.Date('2021-10-01'),
                      as.Date('{last_forecast_date.strftime('%Y-%m-%d')}'),
                      by = "day")
last_eval_date <- '{last_eval_date.strftime('%Y-%m-%d')}'
geo_type <- 'state'
ahead = 5:{max_ahead}
response_data_source = 'hhs'
response_signal = 'confirmed_admissions_influenza_1d_prop_7dav'
states_dc_pr_vi = c('al', 'ak', 'az', 'ar', 'ca', 'co', 'ct', 'dc', 'de', 'fl',
                    'ga', 'hi', 'id', 'il', 'in', 'ia', 'ks', 'ky', 'la', 'me',
                    'md', 'ma', 'mi', 'mn', 'ms', 'mo', 'mt', 'ne', 'nv', 'nh',
                    'nj', 'nm', 'ny', 'nc', 'nd', 'oh', 'ok', 'or', 'pa', 'ri',
                    'sc', 'sd', 'tn', 'tx', 'ut', 'vt', 'va', 'wa', 'wv', 'wi',
                    'wy', 'pr', 'vi')
"""

with open("common_params.R", "w") as f:
    f.write(output_str)

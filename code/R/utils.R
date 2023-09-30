# Returns the next instance of a given weekday. If that is today, return today.
get_next_weekday <- function(date, wday) {
  return(as.Date(date) + (wday - lubridate::wday(date)) %% 7)
}

# Returns the previous instance of a given weekday. If that is today, return today.
get_previous_weekday <- function(date, wday) {
  return(as.Date(date) - (7 - (wday - lubridate::wday(date)) %% 7))
}

# Uploads or downloads a folder to the s3bucket; direction can be c("upload", "download")
manage_forecast_cache <- function(rel_cache_dir, bucket_name = "forecasting-team-data", direction = "download", verbose = FALSE) {
  cache_path <- here::here(rel_cache_dir)
  if (!dir.exists(cache_path)) dir.create(cache_path)

  s3b <- aws.s3::get_bucket(bucket_name)
  if (verbose) {
    aws.s3::s3sync(cache_path, s3b, paste0("covid-hosp-forecast/", rel_cache_dir), direction = direction)
  } else {
    sink("/dev/null")
    aws.s3::s3sync(cache_path, s3b, paste0("covid-hosp-forecast/", rel_cache_dir), direction = direction, verbose = FALSE)
    sink()
  }
  return(TRUE)
}

# helpers.R
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(hms)
})

bucket_start_local <- function(hms_time, buckets) {
  # hms_time: hms object
  # buckets: cfg$buckets list with start/end strings
  # Returns a single string bucket name per element of hms_time.
  res <- rep(NA_character_, length(hms_time))
  for (i in seq_along(hms_time)) {
    h <- hms_time[i]
    if (is.na(h)) next

    # Compare using seconds since midnight
    sec <- as.numeric(h)
    for (nm in names(buckets)) {
      start <- as.numeric(hms::as_hms(buckets[[nm]][1]))
      end   <- as.numeric(hms::as_hms(buckets[[nm]][2]))

      if (nm != "Night") {
        if (sec >= start && sec < end) { res[i] <- nm; break }
      } else {
        # Night wraps midnight: [22:00, 24:00) U [00:00, 06:00)
        if (sec >= start || sec < end) { res[i] <- nm; break }
      }
    }
    if (is.na(res[i])) res[i] <- "Night"
  }
  res
}

coerce_num <- function(x) suppressWarnings(as.numeric(x))

safe_ymd_hms_utc <- function(x) {
  # Parse timestamps defensively as UTC. Returns POSIXct (UTC) with NAs on failures.
  suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
}

dist_km_haversine <- function(lon1, lat1, lon2, lat2) {
  geosphere::distHaversine(cbind(lon1, lat1), cbind(lon2, lat2)) / 1000
}

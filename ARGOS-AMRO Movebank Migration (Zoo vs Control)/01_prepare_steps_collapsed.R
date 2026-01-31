#!/usr/bin/env Rscript
# 01_prepare_steps_collapsed.R
# Same as 01_prepare_steps_no_collapse.R, but collapses multiple steps within
# bird × local date × time-of-day bucket into one row.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(geosphere); library(lubridate); library(hms)
})

source("R/config.R")
source("R/helpers.R")

in_csv  <- cfg$in_csv
out_dir <- cfg$out_dir
cutoff <- ymd_hms(cfg$cutoff_utc, tz = "UTC")

amro <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
  dplyr::select(
    "timestamp", "individual-local-identifier", `tag-local-identifier`,
    "location-lat", "location-long"
  ) %>%
  mutate(`individual-local-identifier` = as.character(`individual-local-identifier`)) %>%
  rename(
    id.full = `individual-local-identifier`,
    lat     = `location-lat`,
    long    = `location-long`
  ) %>%
  tidyr::separate(id.full, into = c("prefix","ID","type","experiment.no"),
                  remove = FALSE, fill = "right") %>%
  distinct()

amro <- amro %>%
  mutate(timestamp = safe_ymd_hms_utc(timestamp)) %>%
  filter(!is.na(timestamp), timestamp <= cutoff)

amro <- amro %>%
  filter(
    dplyr::between(lat,  cfg$lat_min, cfg$lat_max),
    dplyr::between(long, cfg$lon_min, cfg$lon_max)
  ) %>%
  distinct(id.full, `tag-local-identifier`, timestamp, lat, long, .keep_all = TRUE)

amro <- amro %>%
  arrange(id.full, timestamp) %>%
  group_by(id.full) %>%
  mutate(
    startlat     = dplyr::lag(lat),
    startlong    = dplyr::lag(long),
    ts_start_utc = dplyr::lag(timestamp),
    ts_end_utc   = timestamp
  ) %>%
  ungroup()

steps <- amro %>%
  filter(!is.na(ts_start_utc)) %>%
  mutate(
    dist_km     = dist_km_haversine(startlong, startlat, long, lat),
    dist_m      = dist_km * 1000,
    timegap_sec = as.numeric(difftime(ts_end_utc, ts_start_utc, units = "secs")),
    long_gap    = timegap_sec >= cfg$long_gap_sec,
    short_gap   = timegap_sec >= cfg$short_gap_sec & timegap_sec < cfg$long_gap_sec,
    speed_kmh   = dist_km / pmax(timegap_sec/3600, 1e-6)
  ) %>%
  filter(
    timegap_sec >= cfg$min_step_seconds,
    is.finite(speed_kmh),
    speed_kmh <= cfg$max_speed_kmh
  ) %>%
  mutate(
    ts_start_cst   = with_tz(ts_start_utc, cfg$tz_loc),
    ts_end_cst     = with_tz(ts_end_utc,   cfg$tz_loc),
    time_start_cst = hms::as_hms(format(ts_start_cst, "%H:%M:%S")),
    time_end_cst   = hms::as_hms(format(ts_end_cst,   "%H:%M:%S")),
    time_of_day_cst = bucket_start_local(time_start_cst, cfg$buckets),
    date_start_cst  = as.Date(ts_start_cst)
  ) %>%
  arrange(id.full, ts_start_utc)

# Collapse by bird × local day × bucket
collapsed <- steps %>%
  group_by(id.full, ID, type, experiment.no, date_start_cst, time_of_day_cst) %>%
  summarise(
    ts_start_utc = min(ts_start_utc),
    ts_end_utc   = max(ts_end_utc),
    ts_start_cst = min(ts_start_cst),
    ts_end_cst   = max(ts_end_cst),
    startlat     = startlat[which.min(ts_start_utc)],
    startlong    = startlong[which.min(ts_start_utc)],
    lat          = lat[which.max(ts_end_utc)],
    long         = long[which.max(ts_end_utc)],
    dist_m       = sum(dist_m, na.rm = TRUE),
    dist_km      = sum(dist_km, na.rm = TRUE),
    timegap_sec  = as.numeric(difftime(max(ts_end_utc), min(ts_start_utc), units = "secs")),
    long_gap     = any(long_gap,  na.rm = TRUE),
    short_gap    = any(short_gap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    time_start_cst = hms::as_hms(format(ts_start_cst, "%H:%M:%S")),
    time_end_cst   = hms::as_hms(format(ts_end_cst,   "%H:%M:%S")),
    timegap_hms    = hms::as_hms(timegap_sec)
  ) %>%
  arrange(id.full, ts_start_utc) %>%
  group_by(id.full) %>%
  mutate(step_idx = dplyr::row_number(), timestamp = ts_end_utc) %>%
  ungroup()

final <- collapsed %>%
  dplyr::select(
    id.full, ID, type, experiment.no, step_idx,
    ts_start_utc, ts_end_utc, ts_start_cst, ts_end_cst,
    time_start_cst, time_end_cst,
    startlat, startlong, lat, long,
    dist_m, dist_km, timegap_sec, timegap_hms, long_gap, short_gap,
    time_of_day_cst, timestamp
  )

# Write
intensive     <- final %>% filter(type == "Intensive")
non_intensive <- final %>% filter(type != "Intensive" | is.na(type))

readr::write_csv(intensive,     file.path(out_dir, "AMRO_Intensive_collapsed.csv"))
readr::write_csv(non_intensive, file.path(out_dir, "AMRO_NonIntensive_collapsed.csv"))

message("# rows intensive(collapsed): ",    nrow(intensive))
message("# rows non-intensive(collapsed): ", nrow(non_intensive))

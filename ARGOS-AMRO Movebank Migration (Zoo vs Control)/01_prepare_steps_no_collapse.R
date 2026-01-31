#!/usr/bin/env Rscript
# 01_prepare_steps_no_collapse.R
# Convert Movebank fixes → step-level table (one row per consecutive fix pair).
# Outputs:
#   data_processed/AMRO_Intensive_steps.csv
#   data_processed/AMRO_NonIntensive_steps.csv

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
  library(geosphere); library(lubridate); library(hms)
})

source("R/config.R")
source("R/helpers.R")

in_csv  <- cfg$in_csv
out_dir <- cfg$out_dir

cutoff <- ymd_hms(cfg$cutoff_utc, tz = "UTC")

# ---- Read + standardize ----
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
  tidyr::separate(
    id.full,
    into = c("prefix", "ID", "type", "experiment.no"),
    remove = FALSE,
    fill = "right"
  )

# ---- Parse time (UTC) + cutoff ----
amro <- amro %>%
  mutate(timestamp = safe_ymd_hms_utc(timestamp)) %>%
  filter(!is.na(timestamp), timestamp <= cutoff)

# ---- Fix-level coord QC + dedup ----
amro <- amro %>%
  filter(
    dplyr::between(lat,  cfg$lat_min, cfg$lat_max),
    dplyr::between(long, cfg$lon_min, cfg$lon_max)
  ) %>%
  distinct(id.full, `tag-local-identifier`, timestamp, lat, long, .keep_all = TRUE)

# ---- Compute step starts/ends ----
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

amro_steps <- amro %>% filter(!is.na(ts_start_utc)) %>%
  # Step-level coord QC (both ends plausible)
  filter(
    dplyr::between(startlat,  cfg$lat_min, cfg$lat_max),
    dplyr::between(startlong, cfg$lon_min, cfg$lon_max),
    dplyr::between(lat,       cfg$lat_min, cfg$lat_max),
    dplyr::between(long,      cfg$lon_min, cfg$lon_max)
  ) %>%
  distinct(id.full, ts_start_utc, ts_end_utc, startlat, startlong, lat, long, .keep_all = TRUE)

# ---- Distances + time gaps ----
amro_steps <- amro_steps %>%
  mutate(
    dist_km     = dist_km_haversine(startlong, startlat, long, lat),
    dist_m      = dist_km * 1000,
    timegap_sec = as.numeric(difftime(ts_end_utc, ts_start_utc, units = "secs")),
    timegap_hms = hms::as_hms(timegap_sec),
    long_gap    = timegap_sec >= cfg$long_gap_sec,
    short_gap   = timegap_sec >= cfg$short_gap_sec & timegap_sec < cfg$long_gap_sec,
    speed_kmh   = dist_km / pmax(timegap_sec/3600, 1e-6)
  ) %>%
  filter(
    timegap_sec >= cfg$min_step_seconds,
    is.finite(speed_kmh),
    speed_kmh <= cfg$max_speed_kmh
  )

# ---- Local time-of-day bucket by START time (reference) ----
amro_steps <- amro_steps %>%
  mutate(
    ts_start_cst   = with_tz(ts_start_utc, cfg$tz_loc),
    ts_end_cst     = with_tz(ts_end_utc,   cfg$tz_loc),
    time_start_cst = hms::as_hms(format(ts_start_cst, "%H:%M:%S")),
    time_end_cst   = hms::as_hms(format(ts_end_cst,   "%H:%M:%S")),
    time_of_day_cst = bucket_start_local(time_start_cst, cfg$buckets)
  ) %>%
  arrange(id.full, ts_start_utc) %>%
  group_by(id.full) %>%
  mutate(step_idx = dplyr::row_number()) %>%
  ungroup()

# ---- Final columns ----
amro_final <- amro_steps %>%
  dplyr::select(
    id.full, ID, type, experiment.no, step_idx,
    ts_start_utc, ts_end_utc, ts_start_cst, ts_end_cst,
    time_start_cst, time_end_cst,
    startlat, startlong, lat, long,
    dist_m, dist_km, timegap_sec, timegap_hms, long_gap, short_gap,
    time_of_day_cst
  ) %>%
  mutate(timestamp = ts_end_utc)

# ---- Split + write ----
amro_intensive     <- amro_final %>% filter(type == "Intensive")
amro_non_intensive <- amro_final %>% filter(is.na(type) | type != "Intensive")

readr::write_csv(amro_intensive,     file.path(out_dir, "AMRO_Intensive_steps.csv"))
readr::write_csv(amro_non_intensive, file.path(out_dir, "AMRO_NonIntensive_steps.csv"))

message("# rows intensive: ",    nrow(amro_intensive))
message("# rows non-intensive: ", nrow(amro_non_intensive))

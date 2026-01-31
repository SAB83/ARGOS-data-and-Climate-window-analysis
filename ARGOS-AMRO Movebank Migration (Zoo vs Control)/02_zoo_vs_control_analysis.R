#!/usr/bin/env Rscript
# 02_zoo_vs_control_analysis.R
# Zoo vs Control comparisons using step-level table created in script 01.
# Outputs: CSV summaries in outputs/

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate)
  library(tidyr); library(purrr); library(stringr); library(geosphere)
})
options(dplyr.summarise.inform = FALSE)

source("R/config.R")

# ---- Choose which step file to analyze ----
# Use the collapsed file if you prefer bird × day × bucket resolution:
csv_path <- file.path(cfg$out_dir, "AMRO_NonIntensive_steps.csv")
if (!file.exists(csv_path)) {
  # fallback to collapsed if steps not found
  csv_path <- file.path(cfg$out_dir, "AMRO_NonIntensive_collapsed.csv")
}

onset_km <- cfg$onset_km
idle_days_window <- cfg$idle_days_window
daily_km_threshold <- cfg$daily_km_threshold
bearing_range <- cfg$bearing_range
south_method <- cfg$south_method

`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_min_date <- function(x) {
  x <- as.Date(x)
  if (all(is.na(x))) as.Date(NA) else suppressWarnings(min(x, na.rm = TRUE))
}

winter_arrival_date <- function(d_sub, south_idx, idle_days = 14L, daily_km_thresh = 20) {
  if (length(south_idx) == 0) return(as.Date(NA))
  last_i <- max(south_idx, na.rm = TRUE)
  tail <- d_sub %>% dplyr::slice(last_i:n())
  if (nrow(tail) == 0) return(as.Date(NA))

  daily <- tail %>%
    group_by(local_date) %>%
    summarise(day_km = sum(dist_km, na.rm = TRUE), .groups = "drop") %>%
    arrange(local_date)

  if (nrow(daily) == 0) return(as.Date(NA))

  for (i in seq_len(nrow(daily))) {
    start <- daily$local_date[i]
    win <- daily %>% filter(local_date >= start, local_date < start + idle_days)
    if (nrow(win) > 0 && all(win$day_km < daily_km_thresh)) return(start)
  }
  as.Date(NA)
}

compute_stopovers_all_pairs <- function(d_sub, south_idx, thresh = 20) {
  if (length(south_idx) < 2) return(list(n_stop_days_total = NA_real_, avg_bout_len_all = NA_real_))
  total_stop_days <- 0
  all_bout_lengths <- c()

  for (k in seq_len(length(south_idx) - 1)) {
    i <- south_idx[k]; j <- south_idx[k + 1]
    if (is.na(i) || is.na(j) || j <= i) next

    window <- d_sub %>% dplyr::slice(i:j)
    byday <- window %>%
      group_by(local_date) %>%
      summarise(km = sum(dist_km, na.rm = TRUE), .groups = "drop") %>%
      arrange(local_date)

    if (nrow(byday) == 0) next
    flags <- byday$km < thresh
    total_stop_days <- total_stop_days + sum(flags, na.rm = TRUE)

    runs <- rle(flags)
    if (any(runs$values)) all_bout_lengths <- c(all_bout_lengths, runs$lengths[runs$values])
  }

  list(
    n_stop_days_total = total_stop_days,
    avg_bout_len_all  = if (length(all_bout_lengths)) mean(all_bout_lengths) else NA_real_
  )
}

mw_print <- function(x, g, title, g1="Zoo", g2="Control") {
  a <- x[g==g1]; a <- a[!is.na(a)]
  b <- x[g==g2]; b <- b[!is.na(b)]
  cat("\n====", title, "====\n")
  cat("N(", g1, ")=", length(a), "  N(", g2, ")=", length(b), "\n", sep="")
  if (length(a)>=3 && length(b)>=3) {
    wt <- suppressWarnings(wilcox.test(a, b, exact = FALSE))
    cat("Median(", g1, ")=", median(a), "  Median(", g2, ")=", median(b), "\n", sep="")
    cat("Wilcoxon W=", unname(wt$statistic), "  p=", signif(unname(wt$p.value), 4), "\n", sep="")
  } else {
    cat("Not enough non-missing values (need >=3 per group).\n")
  }
}

raw <- readr::read_csv(csv_path, show_col_types = FALSE)

df <- raw %>%
  mutate(
    ts_start_utc = suppressWarnings(lubridate::ymd_hms(ts_start_utc, quiet = TRUE)),
    ts_end_utc   = suppressWarnings(lubridate::ymd_hms(ts_end_utc, quiet = TRUE)),
    ts_start_cst = suppressWarnings(lubridate::ymd_hms(ts_start_cst, quiet = TRUE, tz = cfg$tz_loc)),
    ts_end_cst   = suppressWarnings(lubridate::ymd_hms(ts_end_cst,   quiet = TRUE, tz = cfg$tz_loc)),
    local_date   = as.Date(dplyr::coalesce(ts_start_cst, ts_start_utc)),
    startlat  = suppressWarnings(as.numeric(startlat)),
    startlong = suppressWarnings(as.numeric(startlong)),
    lat       = suppressWarnings(as.numeric(lat)),
    long      = suppressWarnings(as.numeric(long)),
    dist_km   = suppressWarnings(as.numeric(dist_km))
  )

# If dist_km missing, reconstruct from dist_m or coords
if (!"dist_km" %in% names(df) || all(is.na(df$dist_km))) {
  if ("dist_m" %in% names(df)) df$dist_km <- suppressWarnings(as.numeric(df$dist_m))/1000
  else df$dist_km <- geosphere::distHaversine(cbind(df$startlong, df$startlat), cbind(df$long, df$lat))/1000
}

# Group labeling: keep ONLY Zoo and Control; exclude Experimental
df <- df %>%
  mutate(
    type_chr = if ("type" %in% names(.)) tolower(as.character(type)) else NA_character_,
    group2 = dplyr::case_when(
      !is.na(type_chr) & grepl("zoo",       type_chr) ~ "Zoo",
      !is.na(type_chr) & grepl("control",   type_chr) ~ "Control",
      !is.na(type_chr) & grepl("experimental", type_chr) ~ "Experimental",
      grepl("zoo",       tolower(id.full %||% "")) ~ "Zoo",
      grepl("control",   tolower(id.full %||% "")) ~ "Control",
      grepl("experimental", tolower(id.full %||% "")) ~ "Experimental",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(group2 %in% c("Zoo","Control")) %>%
  arrange(id.full, ts_end_utc)

# Bearing + southward definition
df <- df %>%
  mutate(
    bearing_deg = suppressWarnings((geosphere::bearing(cbind(startlong, startlat), cbind(long, lat)) + 360) %% 360),
    delta_lat   = lat - startlat,
    south_lat     = (!is.na(dist_km)) & dist_km >= onset_km & (!is.na(delta_lat))   & (delta_lat < 0),
    south_bearing = (!is.na(dist_km)) & dist_km >= onset_km & (!is.na(bearing_deg)) &
      (bearing_deg >= bearing_range[1] & bearing_deg <= bearing_range[2]),
    southward = dplyr::case_when(
      south_method == "lat"     ~ south_lat,
      south_method == "bearing" ~ south_bearing,
      south_method == "either"  ~ (south_lat | south_bearing),
      south_method == "both"    ~ (south_lat & south_bearing),
      TRUE ~ south_lat
    )
  )

# Per-bird summary
bird_summary <- df %>%
  group_by(id.full) %>%
  group_modify(function(d_sub, key){
    d_sub <- d_sub %>% arrange(ts_end_utc)
    grp <- d_sub$group2[which(!is.na(d_sub$group2))[1]] %||% NA_character_
    cap <- safe_min_date(d_sub$local_date)

    sidx <- which(d_sub$southward)
    first_idx <- if (length(sidx)) sidx[1] else NA_integer_

    first_date <- if (!is.na(first_idx)) d_sub$local_date[first_idx] else as.Date(NA)
    first_dist <- if (!is.na(first_idx)) d_sub$dist_km[first_idx]   else NA_real_

    first_three_idxs <- if (length(sidx) >= 3) sidx[1:3] else integer(0)
    cum3 <- if (length(first_three_idxs) == 3) sum(d_sub$dist_km[first_three_idxs], na.rm = TRUE) else NA_real_

    st_all <- compute_stopovers_all_pairs(d_sub, south_idx = sidx, thresh = daily_km_threshold)
    dur_pre <- as.numeric(first_date - cap)

    winter <- winter_arrival_date(d_sub, sidx, idle_days = idle_days_window, daily_km_thresh = onset_km)

    tibble(
      group2 = grp,
      capture_date = cap,
      first50_date = first_date,
      MigDistDay1 = first_dist,
      CumDistanceFirst3 = cum3,
      NumberStopoverDays = st_all$n_stop_days_total,
      AverageStopoverBoutLength_days = st_all$avg_bout_len_all,
      DurationBeforeMigration_days = dur_pre,
      WinterArrivalDate = winter,
      capture_date_doy = yday(cap),
      first50_date_doy = yday(first_date),
      WinterArrivalDate_doy = yday(winter)
    )
  }) %>% ungroup()

cat("Southward method used:", south_method, "\n")
cat("Per-bird summary rows:", nrow(bird_summary), "\n")

# Analyses
mw_print(bird_summary$DurationBeforeMigration_days, bird_summary$group2,
         "DurationBeforeMigration_days (first big southward date - capture date)")
mw_print(bird_summary$MigDistDay1, bird_summary$group2,
         "MigDistDay1 (km) — distance of first big southward flight")
mw_print(bird_summary$CumDistanceFirst3, bird_summary$group2,
         "Cumulative distance across first 3 big southward flights (km)")
mw_print(bird_summary$WinterArrivalDate_doy, bird_summary$group2,
         "WinterArrivalDate_doy (between groups)")
mw_print(bird_summary$NumberStopoverDays, bird_summary$group2,
         paste0("NumberStopoverDays (all intervals, <", daily_km_threshold, " km/day)"))
mw_print(bird_summary$AverageStopoverBoutLength_days, bird_summary$group2,
         "AverageStopoverBoutLength_days (all intervals)")

# Save outputs
out1 <- file.path("outputs", "migration_summary_by_bird_Zoo_vs_Control.csv")
out2 <- file.path("outputs", "winter_arrival_table_Zoo_vs_Control.csv")
out3 <- file.path("outputs", "winter_arrival_group_stats_Zoo_vs_Control.csv")
dir.create("outputs", showWarnings = FALSE)

wa_table <- bird_summary %>%
  select(id.full, group2, WinterArrivalDate, WinterArrivalDate_doy) %>%
  arrange(group2, WinterArrivalDate)

wa_group_stats <- bird_summary %>%
  group_by(group2) %>%
  summarise(
    n = sum(!is.na(WinterArrivalDate_doy)),
    mean_DOY = mean(WinterArrivalDate_doy, na.rm = TRUE),
    median_DOY = median(WinterArrivalDate_doy, na.rm = TRUE),
    sd_DOY = sd(WinterArrivalDate_doy, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(bird_summary, out1)
readr::write_csv(wa_table, out2)
readr::write_csv(wa_group_stats, out3)

message("Wrote: ", out1)
message("Wrote: ", out2)
message("Wrote: ", out3)

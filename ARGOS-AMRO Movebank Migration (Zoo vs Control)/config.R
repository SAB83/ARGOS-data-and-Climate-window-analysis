# config.R
# User-editable settings for the AMRO Movebank → step table → Zoo vs Control analysis.
# Keep your raw Movebank export OUT of the repo (put it in data_raw/).

cfg <- list(
  # --- Paths ---
  in_csv  = "data_raw/movebank.csv",         # raw Movebank export
  out_dir = "data_processed",                # where step tables will be written
  figures_dir = "outputs/figures",           # maps/plots

  # --- Time handling ---
  tz_loc  = "America/Chicago",               # local timezone for time-of-day bucketing
  cutoff_utc = "2022-12-31 23:59:59",         # inclusive cutoff in UTC

  # --- Coordinate QC (broad North America bounding box) ---
  lat_min  = 10,
  lat_max  = 70,
  lon_min  = -170,
  lon_max  = -50,

  # --- Step QC ---
  min_step_seconds = 60,                      # drop steps shorter than this
  max_speed_kmh    = 150,                     # drop implausible teleports
  long_gap_sec  = 27 * 3600,                  # >= 27 hours
  short_gap_sec = 8  * 3600,                  # >= 8 hours

  # --- Time-of-day buckets (local start time) ---
  buckets = list(
    Morning   = c("06:00:00", "12:00:00"),
    Afternoon = c("12:00:00", "18:00:00"),
    Evening   = c("18:00:00", "22:00:00"),
    Night     = c("22:00:00", "06:00:00")     # wraps midnight
  ),

  # --- Collapse settings (for the 'collapsed' step table) ---
  collapse_by = c("bird_id", "date_start", "time_of_day"),
  # When collapsing: sum distances; start is first start point; end is last end point.

  # --- Analysis settings (Zoo vs Control) ---
  onset_km = 50,                              # “big flight” threshold (km)
  idle_days_window = 14,                      # consecutive idle days to call “arrived”
  daily_km_threshold = 20,                    # day is “idle” if daily km < this
  bearing_range = c(110, 250),                # SE–SW window
  south_method = "lat"                        # lat | bearing | either | both
)

# Create output dirs if needed
if (!dir.exists(cfg$out_dir)) dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(cfg$figures_dir)) dir.create(cfg$figures_dir, recursive = TRUE, showWarnings = FALSE)

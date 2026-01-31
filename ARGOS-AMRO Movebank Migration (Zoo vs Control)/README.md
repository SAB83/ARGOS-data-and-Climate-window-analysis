# AMRO Movebank: Step Tables, Zoo vs Control Migration Metrics, and Segment Maps

This repo turns a **Movebank export** (fix-level locations) into **step-level movement data**, then runs
a **Zoo vs Control** comparison and produces **segment maps** (leaflet + ggplot).

## What’s included

### 1) Step-table creation (two options)
- `R/01_prepare_steps_no_collapse.R`  
  One row per consecutive fix-pair (step). Best if you want the most detailed movement.

- `R/01_prepare_steps_collapsed.R`  
  Collapses multiple steps into **bird × local day × time-of-day bucket** rows (Morning/Afternoon/Evening/Night).  
  Best if you want to reduce autocorrelation and stabilize per-day summaries.

Outputs are written to `data_processed/`:
- `AMRO_Intensive_steps.csv` / `AMRO_NonIntensive_steps.csv`
- or collapsed: `AMRO_Intensive_collapsed.csv` / `AMRO_NonIntensive_collapsed.csv`

### 2) Zoo vs Control analysis
- `R/02_zoo_vs_control_analysis.R`

Produces per-bird summary metrics and saves:
- `outputs/migration_summary_by_bird_Zoo_vs_Control.csv`
- `outputs/winter_arrival_table_Zoo_vs_Control.csv`
- `outputs/winter_arrival_group_stats_Zoo_vs_Control.csv`

### 3) Segment maps
- `R/03_segment_maps.R`

Creates:
- `outputs/figures/<bird_id>_leaflet.html`
- `outputs/figures/<bird_id>_ggplot.png`

## Data privacy
No data are included. Put your Movebank export at:

```
data_raw/movebank.csv
```

These folders are git-ignored by default:
- `data_raw/`
- `data_processed/`
- `outputs/`

## Required columns in Movebank export
Your `movebank.csv` should include at least:

- `timestamp`
- `individual-local-identifier`
- `tag-local-identifier`
- `location-lat`
- `location-long`

(Names match your code; if your export differs, rename columns in the script.)

## Setup

1. Open `R/config.R` and edit:
   - `cfg$in_csv`  (path to your raw export)
   - `cfg$cutoff_utc`
   - QC bounding box, speed cap, time buckets, etc.

2. Install packages:

```r
install.packages(c("readr","dplyr","tidyr","lubridate","hms","geosphere",
                   "sf","leaflet","ggplot2","ggrepel","rnaturalearth","htmlwidgets"))
```

## Run

From the project root:

```bash
Rscript R/01_prepare_steps_no_collapse.R
# or
Rscript R/01_prepare_steps_collapsed.R

Rscript R/02_zoo_vs_control_analysis.R

Rscript R/03_segment_maps.R
```

## Notes on key choices

- **Coordinate QC:** default broad North America bounds in `config.R`.
- **Teleport filter:** drops steps with `speed_kmh > max_speed_kmh`.
- **Winter arrival:** first date after last “big southward flight” followed by N idle days.
- **Stopover days:** days with total movement < `daily_km_threshold` between consecutive big flights.

If you want, I can add:
- unit tests for parsing/step creation,
- a `renv` lockfile for reproducible package versions,
- or a script to automatically detect Zoo/Control labels from your ID naming convention.

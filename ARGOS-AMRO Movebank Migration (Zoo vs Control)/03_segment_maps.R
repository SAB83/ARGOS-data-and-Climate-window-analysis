#!/usr/bin/env Rscript
# 03_segment_maps.R
# Create interactive leaflet and static ggplot maps for one bird.
# Notes:
# - This script assumes your step table has end-point lat/long and local times.
# - Labels are segment numbers and local start/end times.
#
# Output:
#   outputs/figures/<bird_id>_leaflet.html
#   outputs/figures/<bird_id>_ggplot.png

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(lubridate); library(stringr)
  library(sf); library(leaflet); library(ggplot2); library(ggrepel); library(rnaturalearth)
  library(htmlwidgets)
})

source("R/config.R")

# ---- Choose input ----
csv_path <- file.path(cfg$out_dir, "AMRO_NonIntensive_steps.csv")
if (!file.exists(csv_path)) csv_path <- file.path(cfg$out_dir, "AMRO_NonIntensive_collapsed.csv")
df <- readr::read_csv(csv_path, show_col_types = FALSE)

# ---- Standardize types ----
df <- df %>%
  mutate(
    bird_id = as.character(id.full),
    lat  = suppressWarnings(as.numeric(lat)),
    lon  = suppressWarnings(as.numeric(long)),
    ts_local = suppressWarnings(ymd_hms(ts_end_cst, tz = cfg$tz_loc, quiet = TRUE))
  ) %>%
  filter(!is.na(bird_id), !is.na(lat), !is.na(lon), !is.na(ts_local)) %>%
  arrange(bird_id, ts_local)

# ---- pick bird_id with most points if not set ----
bird_counts <- df %>% count(bird_id, name = "n_points") %>% arrange(desc(n_points))
print(head(bird_counts, 20))

my_bird_id <- bird_counts$bird_id[1]

# ---- Build segments for one bird ----
build_segments <- function(df_one) {
  df_one <- df_one %>% arrange(ts_local)
  if (nrow(df_one) < 2) return(NULL)

  segs <- df_one %>%
    mutate(
      lon2 = lead(lon),
      lat2 = lead(lat),
      end_time = lead(ts_local)
    ) %>%
    filter(!is.na(lon2), !is.na(lat2)) %>%
    filter(!(lon == lon2 & lat == lat2)) %>%
    mutate(
      segment_id = row_number(),
      mid_lon = (lon + lon2)/2,
      mid_lat = (lat + lat2)/2,
      label = paste0(
        "#", segment_id, "  ",
        format(ts_local, "%Y-%m-%d %H:%M"),
        " → ",
        format(end_time, "%Y-%m-%d %H:%M")
      )
    ) %>%
    rowwise() %>%
    mutate(
      geometry = list(
        sf::st_linestring(matrix(c(lon, lat, lon2, lat2), ncol = 2, byrow = TRUE))
      )
    ) %>%
    ungroup()

  sf::st_as_sf(segs, crs = 4326)
}

pts <- df %>% filter(bird_id == my_bird_id)
segs <- build_segments(pts)
if (is.null(segs) || nrow(segs) == 0) stop("Not enough points to build segments for selected bird.")

# ---- Interactive leaflet ----
m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles() %>%
  addPolylines(data = segs, weight = 3, opacity = 0.9, popup = ~label) %>%
  addCircleMarkers(data = st_as_sf(pts, coords = c("lon","lat"), crs = 4326, remove = FALSE),
                   radius = 4, stroke = TRUE, weight = 1, fillOpacity = 0.9,
                   popup = ~paste0("<b>", bird_id, "</b><br/>",
                                   "Local time: ", format(ts_local, "%Y-%m-%d %H:%M"), "<br/>",
                                   "(", round(lat, 4), ", ", round(lon, 4), ")")) %>%
  addScaleBar(position = "bottomleft")

html_out <- file.path(cfg$figures_dir, paste0(gsub("[^A-Za-z0-9_-]+","_", my_bird_id), "_leaflet.html"))
htmlwidgets::saveWidget(m, html_out, selfcontained = TRUE)
message("Wrote leaflet map: ", html_out)

# ---- Static ggplot ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
seg_mid <- segs %>% st_drop_geometry() %>% select(mid_lon, mid_lat, label)

p <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_sf(data = segs, linewidth = 0.9) +
  geom_point(data = pts, aes(x = lon, y = lat), size = 1.8, alpha = 0.9) +
  ggrepel::geom_label_repel(data = seg_mid, aes(x = mid_lon, y = mid_lat, label = label),
                            size = 3, min.segment.length = 0, max.overlaps = 100) +
  labs(title = paste0("Movement segments: ", my_bird_id),
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12)

png_out <- file.path(cfg$figures_dir, paste0(gsub("[^A-Za-z0-9_-]+","_", my_bird_id), "_ggplot.png"))
ggsave(png_out, p, width = 10, height = 6, dpi = 200)
message("Wrote ggplot map: ", png_out)

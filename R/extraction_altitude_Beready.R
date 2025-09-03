# ────────────────────────────────────────────────────────────────────────────────
#  Stand‑alone script: classify Swiss municipalities as “lowland” vs “mountain”
# ────────────────────────────────────────────────────────────────────────────────
library(sf)       # vector data
library(terra)    # raster data + extraction
library(dplyr)    # data wrangling
library(leaflet)  # web map
library(exactextractr)

# ── user parameters ─────────────────────────────────────────────────────────────
# Elevation cut‑off (metres a.s.l.) separating lowland from mountain
thr <- 800                     # CH Federal Office of Topography convention

# Path to SwissBOUNDARIES3D shapefile (municipalities, LV95 / EPSG 2056)
shp_path <- "data/swissboundaries3d_2025-04_2056_5728.shp/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp"

# Path to DHM25 grid (one large ASCII grid or a virtual raster) — LV95
dem_path <- "data/DHM25_MM_ASCII_GRID/ASCII_GRID_1part/dhm25_grid_raster.asc"

# ── load data ──────────────────────────────────────────────────────────────────
g        <- read_sf(shp_path)         # municipality polygons (≈2200)
dhm_25   <- rast(dem_path)            # 25‑m DEM (≈340 million cells)

g=st_zm(g,drop = TRUE)
g=st_transform(g,"EPSG:21781")
crs(dhm_25) <- "EPSG:21781"
dat_in=read.csv("data/1951BEreadyHauptstud-VectorborneDiseases_DATA_2025-06-12_1515.csv")
dat_in=dat_in %>% filter(!is.na(bl_commune))

target_ids <- unique(dat_in$bl_commune)                       # BFS codes present
g      <- g |> filter(BFS_NUMMER %in% target_ids)

dhm_25 <- crop(dhm_25, vect(g))

# ── compute mean elevation per municipality ────────────────────────────────────
# terra::extract returns one row per polygon

g$mean_elev <- exact_extract(dhm_25,g,fun="mean")
g$zone      <- ifelse(g$mean_elev < 800, "lowland", "mountain")

# 4. join back to the respondents --------------------------------------------
dat_in <- left_join(dat_in,
                    g |> st_drop_geometry() |> select(BFS_NUMMER, mean_elev, zone),
                    by = c("bl_commune" = "BFS_NUMMER"))

# ── interactive map coloured by zone ───────────────────────────────────────────
g_wgs84 <- st_transform(g, 4326)       # leaflet needs WGS‑84
pal     <- colorFactor(c("seagreen3", "firebrick"), domain = g_wgs84$zone)

leaflet(g_wgs84) |>
  addTiles() |>
  addPolygons(
    fillColor   = ~pal(zone),
    fillOpacity = 0.65,
    color       = "grey40",
    weight      = 0.8,
    label       = ~paste0(NAME, ": ", zone, " (", round(mean_elev), "m)")
  ) |>
  addLegend(
    "topright",
    pal    = pal,
    values = ~zone,
    title  = paste0("Lowland<", thr, "m Mountain ≥ ", thr, "m")
  )

write.csv(dat_in,"data/BeReady_extracted_alt.csv",row.names = F)

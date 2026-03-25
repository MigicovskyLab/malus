# This is a script for extracting the PA names that are stable for
# Malus angustifolia under SSP585 in 2070
# They use the origional shape files from the WDPAR to get the names
library(terra)
library(dplyr)

# Paths
future_path <- "./sdm_output/ang/subs/habitat_pred/ssp585/ang_pred_high_ssp585_70_subs.Rdata"
hist_path   <- "./sdm_output/ang/subs/habitat_pred/hist/ang_pred_high_hist_subs.Rdata"
pa_mask_path <- "./gap_analysis/pa_raster_us_can.tif"
wdpa_dir    <- "./gap_analysis/wdpar_us_can/WDPA_May2025_USA-shapefile/"
out_csv     <- "./gap_analysis/ang_ssp585_2070_stable_PA_overlap_WDPA_appalachians.csv"

# To speed up analysis going to crop the area to the southern Appilachias
app_ext <- ext(-86, -79, 33.5, 38.0)

# Load suitability SpatRaster 
future_mask <- readRDS(future_path)
hist_mask   <- readRDS(hist_path)

# Only want the true suitable areas
future_mask <- future_mask == 1
hist_mask   <- hist_mask == 1

# Rasterize the PAs
pa_mask <- rast(pa_mask_path) == 1
pa_mask <- resample(pa_mask, future_mask, method = "near")

# Crop them all
future_crop <- crop(future_mask, app_ext)
hist_crop   <- crop(hist_mask, app_ext)
pa_crop     <- crop(pa_mask, app_ext)

# Find the areas that are refugia by comparing the three rasters
refugia_r <- future_crop & hist_crop & pa_crop
refugia_r <- ifel(refugia_r, 1, NA)

refugia_poly <- as.polygons(refugia_r, dissolve = TRUE, na.rm = TRUE)
val_col <- names(refugia_poly)[1]
refugia_poly <- refugia_poly[refugia_poly[[val_col]] == 1, ]
if (nrow(refugia_poly) == 0) stop("No refugia found in crop window.")

# Now load olygon from WDPA to get the names of PAs
shp_files <- list.files(wdpa_dir, pattern = "shp-polygons.*\\.shp$", full.names = TRUE, recursive = TRUE)
stopifnot(length(shp_files) > 0)
pa_list <- lapply(shp_files, vect)
pa_us <- do.call(rbind, pa_list)

# Crop PAs
pa_app <- crop(pa_us, app_ext)

# Make sure the CRS is the same
if (!same.crs(pa_app, refugia_poly)) pa_app <- project(pa_app, crs(refugia_poly))

# Now intersect the PA poly with the refugia raster
hits <- intersect(pa_app, refugia_poly)
hits <- do.call(rbind, hits)


# Interested to know the actual area of refugia
hits$overlap_km2 <- expanse(hits, unit = "km")
hits_df <- as.data.frame(hits)

valid_types <- c(
  "National Park",
  "National Forest",
  "National Wildlife Refuge",
  "Wilderness",
  "Wilderness Area",
  "State Forest",
  "State Park",
  "State Natural Area",
  "State Natural Area Preserve",
  "State Nature Preserve",
  "Research Natural Area",
  "World Heritage Site (natural or mixed)"
)

hits_df <- hits_df %>%
  filter(DESIG_ENG %in% valid_types)

# Output what refugia are
out <- hits_df %>%
  group_by(DESIG_ENG) %>%
  summarise(
    overlap_km2 = sum(overlap_km2),
    n_units = n_distinct(NAME),
    .groups = "drop"
  ) %>%
  arrange(desc(overlap_km2))

unique_pa_names <- unique(hits_df$NAME)
unique_pa_names

# save CSV output
write.csv(out, out_csv, row.names = FALSE)
print(out)

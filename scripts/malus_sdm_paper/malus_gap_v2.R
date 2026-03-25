# Malus Gap Analysis — Restricted by Ecoregions Containing Occurrences Masked by Historical Suitability 
# Version 2, much faster
# Terrell Roulston
# Started May 20th 2025
# ---- Malus Gap Analysis for All Species ----

# The following gap analysis is referencing Carver et al. (2021)
# https://nsojournals.onlinelibrary.wiley.com/doi/full/10.1111/ecog.05430

library(terra)        # For raster operations
library(tidyverse)    # For dplyr, tibble, etc.
library(geodata)      # For loading GADM and other data

# Species list
species_list <- c("cor", "fus", "ion", "ang", "chl")
species_names <- c("Malus coronaria", "Malus fusca", "Malus ioensis", "Malus angustifolia", "Sect. Chloromeles")

# ---- No Migration Functions ----

calculate_srsin <- function(pred, pa, occ, threshold) {
  r_masked <- pred > threshold
  names(r_masked) <- "binary"
  r_pa <- terra::mask(pred, pa) > threshold
  names(r_pa) <- "binary"
  
  occ_pred <- terra::extract(r_masked, occ) %>% as.data.frame() %>% dplyr::filter(binary == 1)
  occ_pa   <- terra::extract(r_pa, occ)     %>% as.data.frame() %>% dplyr::filter(binary == 1)
  
  if (nrow(occ_pred) == 0) return(0)
  return(nrow(occ_pa) / nrow(occ_pred) * 100)
}

calculate_grsin <- function(pred, threshold, hist_masked, pa_mask) {
  r_pred <- pred > threshold
  r_masked <- r_pred & (hist_masked == 1)
  area_all <- terra::expanse(r_masked, byValue = TRUE, unit = "km") %>%
    as.data.frame() %>% filter(value == 1) %>% pull(area)
  
  r_pa_masked <- r_pred & (hist_masked == 1) & (pa_mask == 1)
  area_pa <- terra::expanse(r_pa_masked, byValue = TRUE, unit = "km") %>%
    as.data.frame() %>% filter(value == 1) %>% pull(area)
  
  if (length(area_all) == 0 || sum(area_all) == 0) return(0)
  return(sum(area_pa, na.rm = TRUE) / sum(area_all, na.rm = TRUE) * 100)
}

calculate_ersin <- function(pred, threshold, eco_vec, hist_masked, pa_mask_resampled) {
  vals <- as.data.frame(terra::values(c(pred, hist_masked, pa_mask_resampled, eco_vec)))
  colnames(vals) <- c("pred", "hist", "pa", "eco")

  # Remove rows with NA or 0 eco values (assumes 0 = outside eco region)
  vals <- vals[!is.na(vals$eco) & vals$eco != 0, ]

  # Historical suitable ecoregions (regardless of PA)
  hist_filter <- vals$hist == 1
  eco_hist_codes <- sort(unique(vals$eco[hist_filter]))

  # Future suitable + protected *within historical range*
  future_filter <- vals$pred > threshold & vals$hist == 1 & vals$pa == 1
  eco_future_codes <- sort(unique(vals$eco[future_filter]))

  # Coerce to numeric to avoid factor/character mismatch issues
  eco_hist_codes <- as.numeric(as.character(eco_hist_codes))
  eco_future_codes <- as.numeric(as.character(eco_future_codes))

  # Filter out invalid or NA codes
  eco_hist_codes <- eco_hist_codes[!is.na(eco_hist_codes)]
  eco_future_codes <- eco_future_codes[!is.na(eco_future_codes)]

  # Compute overlap
  overlap_codes <- intersect(eco_hist_codes, eco_future_codes)
  overlap_codes <- overlap_codes[!is.na(overlap_codes)]

  # Debug output
  message("[ERSin] Unique ecoregions (historical suitable): ", paste(eco_hist_codes, collapse = " "))
  message("[ERSin] Unique ecoregions (future suitable *in hist range* + protected): ", paste(eco_future_codes, collapse = " "))
  message("[ERSin] Overlap: ", paste(overlap_codes, collapse = " "))

  # Return 0 if no overlap
  if (length(overlap_codes) == 0) {
    message("[ERSin] No overlapping ecoregions — returning 0.")
    return(0)
  }

  # Final ERSin: proportion of historical suitable ecoregions still suitable+protected in future
  return((length(overlap_codes) / length(eco_hist_codes)) * 100)
}


# Perfect Migration Functions (No Historical Masking) -------------------------------

calculate_srsin_migration <- function(pred, pa, occ, threshold) {
  r_masked <- pred > threshold
  names(r_masked) <- "binary"
  r_pa <- terra::mask(pred, pa) > threshold
  names(r_pa) <- "binary"
  
  occ_pred <- terra::extract(r_masked, occ) %>%
    as.data.frame() %>%
    dplyr::filter(binary == 1)
  
  occ_pa <- terra::extract(r_pa, occ) %>%
    as.data.frame() %>%
    dplyr::filter(binary == 1)
  
  if (nrow(occ_pred) == 0) return(0)
  return(nrow(occ_pa) / nrow(occ_pred) * 100)
}

calculate_grsin_migration <- function(pred, pa, threshold) {
  r_masked <- pred > threshold
  names(r_masked) <- "binary"
  
  area_all <- terra::expanse(r_masked, byValue = TRUE, unit = "km") %>%
    as.data.frame() %>%
    dplyr::filter(value == 1) %>%
    dplyr::summarise(area = sum(area, na.rm = TRUE)) %>%
    dplyr::pull(area)
  
  r_pa <- (pred > threshold) & (pa == 1)
  names(r_pa) <- "binary"
  
  area_pa <- terra::expanse(r_pa, byValue = TRUE, unit = "km") %>%
    as.data.frame() %>%
    dplyr::filter(value == 1) %>%
    dplyr::summarise(area = sum(area, na.rm = TRUE)) %>%
    dplyr::pull(area)
  
  if (is.na(area_all) || area_all == 0) return(0)
  return(area_pa / area_all * 100)
}

calculate_ersin_migration <- function(pred, pa, eco_all, threshold, eco_hist = NULL) {
  # Future suitable area and protected subset
  r_masked <- pred > threshold
  names(r_masked) <- "binary"
  r_pa <- (pred > threshold) & (pa == 1)
  names(r_pa) <- "binary"
  
  # Reference ecoregions = all FUTURE suitable
  ref_codes <- terra::mask(eco_all, r_masked) %>%
    terra::values() %>%
    as.vector() %>%
    unique() %>%
    stats::na.omit() %>%
    sort()
  
  # Protected ecoregions = FUTURE suitable + protected
  pa_codes <- terra::mask(eco_all, r_pa) %>%
    terra::values() %>%
    as.vector() %>%
    unique() %>%
    stats::na.omit() %>%
    sort()
  
  if (length(ref_codes) == 0) {
    return(list(value = 0, new_ecoregions = NULL))
  }
  
  ersin <- length(intersect(ref_codes, pa_codes)) / length(ref_codes) * 100
  
  # Optional: identify new ecoregions relative to historical-suitable set
  new_codes <- NULL
  if (!is.null(eco_hist)) {
    hist_codes <- terra::mask(eco_hist, eco_hist != 0) %>%
      terra::values() %>%
      as.vector() %>%
      unique() %>%
      stats::na.omit() %>%
      sort()
    new_codes <- setdiff(ref_codes, hist_codes)
  }
  
  return(list(value = ersin, new_ecoregions = new_codes))
}

calculate_fcsin_migration <- function(SRSin, GRSin, ERSin) {
  return(mean(c(SRSin, GRSin, ERSin)))
}

# ---- Run Gap Analysis ----
# This gap analysis does not allow for migration of species to new suitable areas
# All analyses here are masked to the historical extent of suitability

full_result <- list()
Start_all <- Sys.time()

for (index in seq_along(species_list)) {
  sp_code <- species_list[index]
  sp_name <- species_names[index]
  
  cat("\n==== Running NO MIGRATION gapanalysis for:", sp_code, "====\n")
  Start <- Sys.time()
  
  base_path <- file.path("./sdm_output", sp_code, "subs")
  thresh_path <- file.path(base_path, "threshold")
  occ_path <- file.path("./occ_data", sp_code)
  
  pa_raster <- terra::rast("./gap_analysis/pa_raster_us_can.tif")
  eco_vec <- readRDS(file.path("./maps/eco_regions", paste0("ecoNA_", sp_code, ".Rdata")))
  
  occ <- readRDS(file.path(occ_path, paste0("occThin_", sp_code, ".Rdata")))
  if (!inherits(occ, "SpatVector")) occ <- terra::vect(occ)
  
  eco_mask_pts <- terra::intersect(eco_vec, occ)
  eco_occ <- unique(eco_mask_pts$NA_L2CODE)
  eco_vec_crop <- eco_vec[eco_vec$NA_L2CODE %in% eco_occ, ]
  
  preds <- list(
    hist = readRDS(file.path(base_path, paste0(sp_code, "_pred_hist_subs.Rdata"))),
    ssp245_30 = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_30_subs.Rdata"))),
    ssp245_50 = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_50_subs.Rdata"))),
    ssp245_70 = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_70_subs.Rdata"))),
    ssp585_30 = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_30_subs.Rdata"))),
    ssp585_50 = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_50_subs.Rdata"))),
    ssp585_70 = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_70_subs.Rdata")))
  )
  
  thresholds <- list(
    low = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_1_subs.Rdata"))),
    mod = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_10_subs.Rdata"))),
    high = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_50_subs.Rdata")))
  )
  
  preds <- lapply(preds, function(x) terra::crop(x, eco_vec_crop, mask = TRUE))
  template <- preds[[1]]
  pa_mask_resampled <- terra::resample(pa_raster == 1, template, method = "near")
  hist_masked <- preds[["hist"]] > thresholds[["high"]]
  hist_masked <- terra::resample(hist_masked, template, method = "near")
  
  eco_rast <- terra::rasterize(eco_vec_crop, template, field = "NA_L2CODE")
  
  out <- list()
  for (pname in names(preds)) {
    for (thresh_name in "high") { # Only analysis high suitability areas
      th <- thresholds[[thresh_name]]
      pr <- preds[[pname]]
      
      cat("[INFO] Processing:", sp_code, pname, thresh_name, "\n")
      
      SRSin <- calculate_srsin(pr, pa_mask_resampled, occ, th)
      GRSin <- calculate_grsin(pr, th, hist_masked, pa_mask_resampled)
      ERSin <- calculate_ersin(pr, th, eco_rast, hist_masked, pa_mask_resampled)
      FCSin <- mean(c(SRSin, GRSin, ERSin))
      
      out[[paste(pname, thresh_name, sep = "_")]] <- tibble(
        species = sp_name,
        sp_code = sp_code,
        ssp = ifelse(pname == 'hist', 'historical', ifelse(grepl('245', pname), '245', '585')),
        period = case_when(
          pname == 'hist' ~ 2000,
          pname == 'ssp245_30' ~ 2030,
          pname == 'ssp245_50' ~ 2050,
          pname == 'ssp245_70' ~ 2070,
          pname == 'ssp585_30' ~ 2030,
          pname == 'ssp585_50' ~ 2050,
          pname == 'ssp585_70' ~ 2070
        ),
        suitability = thresh_name,
        SRSin = SRSin,
        GRSin = GRSin,
        ERSin = ERSin,
        FCSin = FCSin
      )
    }
  }
  
  full_result[[sp_code]] <- bind_rows(out)
}


all_species_results <- bind_rows(full_result)
readr::write_csv(all_species_results, "./gap_analysis/malus_gap_analysis_NO_migration.csv")


# Run Migration Gap Analysis ----------------------------------------------
# Requires the migration helpers
# calculate_srsin_migration(), calculate_grsin_migration(),
# calculate_ersin_migration(), calculate_fcsin_migration()

full_result_migration <- list()
Start_all <- Sys.time()

for (index in seq_along(species_list)) {
  sp_code <- species_list[index]
  sp_name <- species_names[index]
  
  cat("\n==== Running MIGRATION gap analysis for:", sp_code, "====\n")
  Start <- Sys.time()
  
  base_path   <- file.path("./sdm_output", sp_code, "subs")
  thresh_path <- file.path(base_path, "threshold")
  occ_path    <- file.path("./occ_data", sp_code)
  
  pa_raster <- terra::rast("./gap_analysis/pa_raster_us_can.tif")
  eco_vec   <- readRDS(file.path("./maps/eco_regions", paste0("ecoNA_", sp_code, ".Rdata")))
  
  occ <- readRDS(file.path(occ_path, paste0("occThin_", sp_code, ".Rdata")))
  if (!inherits(occ, "SpatVector")) occ <- terra::vect(occ)
  
  eco_mask_pts <- terra::intersect(eco_vec, occ)
  eco_occ      <- unique(eco_mask_pts$NA_L2CODE)
  eco_vec_crop <- eco_vec[eco_vec$NA_L2CODE %in% eco_occ, ]
  
  preds <- list(
    hist       = readRDS(file.path(base_path, paste0(sp_code, "_pred_hist_subs.Rdata"))),
    ssp245_30  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_30_subs.Rdata"))),
    ssp245_50  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_50_subs.Rdata"))),
    ssp245_70  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_70_subs.Rdata"))),
    ssp585_30  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_30_subs.Rdata"))),
    ssp585_50  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_50_subs.Rdata"))),
    ssp585_70  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_70_subs.Rdata")))
  )
  
  thresholds <- list(
    low  = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_1_subs.Rdata"))),
    mod  = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_10_subs.Rdata"))),
    high = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_50_subs.Rdata")))
  )
  
  # Crop predictions to species eco extent
  preds <- lapply(preds, function(x) terra::crop(x, eco_vec_crop, mask = TRUE))
  template <- preds[[1]]
  
  # PA mask and historical binary mask (for reporting "new ecoregions")
  pa_mask_resampled <- terra::resample(pa_raster == 1, template, method = "near")
  # Create a binary raster of historical high suitability
  hist_masked <- preds[["hist"]] > thresholds[["high"]]
  # Resample to match the template resolution/extent
  hist_masked <- terra::resample(hist_masked, template, method = "near")
  
  # Load Level II ecoregions vector that spans NA
  eco_dir <- "./maps/eco_regions/na_cec_eco_l2"
  
  # Read full Level II ecoregions as a vector layer
  eco_vec_full <- terra::vect(file.path(eco_dir, "NA_CEC_Eco_Level2.shp"))
  # Attributes should include NA_L2CODE and NA_L2NAME)
  
  # Project to the raster template CRS
  if (!terra::same.crs(eco_vec_full, template)) {
    eco_vec_full <- terra::project(eco_vec_full, terra::crs(template))
  }
  
  # (Optional) fix geometries if needed
  # eco_vec_full <- terra::makeValid(eco_vec_full)
  
  # Rasterize full ecoregions on the same resolution/extent as 'template'
  eco_rast_full <- terra::rasterize(eco_vec_full, template, field = "NA_L2CODE")
  
  # Historical-suitable ecoregions (to identify “new” ones under migration)
  eco_hist <- terra::mask(eco_rast_full, hist_masked)
  
  out <- list()
  for (pname in names(preds)) {
    for (thresh_name in "high") {  # only analyze high suitability
      th <- thresholds[[thresh_name]]
      pr <- preds[[pname]]
      
      cat("[MIGRATION] Processing:", sp_code, pname, thresh_name, "\n")
      
      SRSin <- calculate_srsin_migration(pr, pa_mask_resampled, occ, th)
      GRSin <- calculate_grsin_migration(pr, pa_mask_resampled, th)
      ERobj <- calculate_ersin_migration(pr, pa_mask_resampled, eco_rast_full, th, eco_hist = eco_hist)
      
      ERSin <- ERobj$value
      FCSin <- calculate_fcsin_migration(SRSin, GRSin, ERSin)
      
      out[[paste(pname, thresh_name, sep = "_")]] <- tibble(
        mode        = "migration",
        species     = sp_name,
        sp_code     = sp_code,
        ssp         = ifelse(pname == 'hist', 'historical', ifelse(grepl('245', pname), '245', '585')),
        period      = dplyr::case_when(
          pname == 'hist'       ~ 2000,
          pname == 'ssp245_30'  ~ 2030,
          pname == 'ssp245_50'  ~ 2050,
          pname == 'ssp245_70'  ~ 2070,
          pname == 'ssp585_30'  ~ 2030,
          pname == 'ssp585_50'  ~ 2050,
          pname == 'ssp585_70'  ~ 2070
        ),
        suitability = thresh_name,
        SRSin = SRSin,
        GRSin = GRSin,
        ERSin = ERSin,
        FCSin = FCSin,
        # Keep the codes for auditing
        new_ecoregions = list(ERobj$new_ecoregions)
      )
    }
  }
  
  full_result_migration[[sp_code]] <- dplyr::bind_rows(out)
  cat("Finished:", sp_code, " in", round(as.numeric(difftime(Sys.time(), Start, units = "mins")), 2), "min\n")
}

migration_results <- dplyr::bind_rows(full_result_migration)

# Write a flat CSV (drop list-column for convenience)
readr::write_csv(
  migration_results %>% dplyr::select(-dplyr::any_of("new_ecoregions")),
  "./gap_analysis/malus_gap_analysis_PERFECT_migration.csv"
)

cat("\nAll done (MIGRATION). Total minutes:",
    round(as.numeric(difftime(Sys.time(), Start_all, units = "mins")), 2), "\n")

# ---- Combine NO-MIGRATION and MIGRATION results ----

cat("\n==== Combining no-migration and migration results ====\n")

# Read the two outputs
no_mig <- readr::read_csv("./gap_analysis/malus_gap_analysis_NO_migration.csv") %>%
  mutate(mode = "no_migration")

mig <- readr::read_csv("./gap_analysis/malus_gap_analysis_PERFECT_migration.csv") %>%
  mutate(mode = "migration")

# Ensure consistent columns before binding
common_cols <- union(names(no_mig), names(mig))
no_mig <- no_mig %>% dplyr::select(all_of(common_cols))
mig <- mig %>% dplyr::select(all_of(common_cols))

# Combine and save
combined_results <- bind_rows(no_mig, mig)

readr::write_csv(combined_results, "./gap_analysis/malus_gap_analysis_combined.csv")

cat("\nCombined results written to: ./gap_analysis/malus_gap_analysis_combined.csv\n")
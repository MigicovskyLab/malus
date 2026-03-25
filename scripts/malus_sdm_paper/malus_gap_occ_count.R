# Terrell Roulston
# Started Nov 17th 2025
# Malus occurrence coverage counts
# Number of occurrences in suitable habitat and in suitable + PA
# For BOTH no-migration (historical mask) and migration (no mask)
# ------------------------------

library(terra)
library(tidyverse)

# Species list (same as main script)
species_list  <- c("cor", "fus", "ion", "ang", "chl")
species_names <- c("Malus coronaria", "Malus fusca", "Malus ioensis",
                   "Malus angustifolia", "Sect. Chloromeles")

# Only using "high" threshold to match main gap analysis
thresh_to_use <- "high"

# Helper: compute counts from a *binary suitability raster* and PA mask
# suit_bin: SpatRaster with 0/1 (or logical) for suitability
# pa_mask : SpatRaster with 1 inside PA, 0 outside
get_occ_counts_bin <- function(suit_bin, pa_mask, occ) {
  # Suitability raster
  r_suit <- suit_bin
  names(r_suit) <- "binary"
  
  # Suitability + PA
  r_pa <- (suit_bin == 1) & (pa_mask == 1)
  names(r_pa) <- "binary"
  
  # Extract at occurrence points
  occ_suit <- terra::extract(r_suit, occ) %>%
    as.data.frame() %>%
    dplyr::filter(binary == 1)
  
  occ_pa <- terra::extract(r_pa, occ) %>%
    as.data.frame() %>%
    dplyr::filter(binary == 1)
  
  tibble(
    n_occ_total        = length(occ),      # total occurrences
    n_occ_suitable     = nrow(occ_suit),   # in suitable habitat
    n_occ_suitable_pa  = nrow(occ_pa)      # suitable + inside PA
  )
}

# ------------------------------
# Main loop
# ------------------------------

occ_results <- list()
Start_all <- Sys.time()

for (index in seq_along(species_list)) {
  sp_code <- species_list[index]
  sp_name <- species_names[index]
  
  cat("\n==== Computing occurrence counts for:", sp_code, "====\n")
  Start <- Sys.time()
  
  base_path   <- file.path("./sdm_output", sp_code, "subs")
  thresh_path <- file.path(base_path, "threshold")
  occ_path    <- file.path("./occ_data", sp_code)
  
  # PA raster
  pa_raster <- terra::rast("./gap_analysis/pa_raster_us_can.tif")
  
  # Ecoregions for cropping (same as no-migration block in main script)
  eco_vec <- readRDS(file.path("./maps/eco_regions", paste0("ecoNA_", sp_code, ".Rdata")))
  
  # Occurrences
  occ <- readRDS(file.path(occ_path, paste0("occThin_", sp_code, ".Rdata")))
  if (!inherits(occ, "SpatVector")) occ <- terra::vect(occ)
  
  # Crop extent to ecoregions containing occurrences
  eco_mask_pts <- terra::intersect(eco_vec, occ)
  eco_occ      <- unique(eco_mask_pts$NA_L2CODE)
  eco_vec_crop <- eco_vec[eco_vec$NA_L2CODE %in% eco_occ, ]
  
  # Load predictions
  preds <- list(
    hist       = readRDS(file.path(base_path, paste0(sp_code, "_pred_hist_subs.Rdata"))),
    ssp245_30  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_30_subs.Rdata"))),
    ssp245_50  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_50_subs.Rdata"))),
    ssp245_70  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp245_70_subs.Rdata"))),
    ssp585_30  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_30_subs.Rdata"))),
    ssp585_50  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_50_subs.Rdata"))),
    ssp585_70  = readRDS(file.path(base_path, paste0(sp_code, "_pred_ssp585_70_subs.Rdata")))
  )
  
  # Thresholds
  thresholds <- list(
    low  = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_1_subs.Rdata"))),
    mod  = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_10_subs.Rdata"))),
    high = readRDS(file.path(thresh_path, paste0(sp_code, "Pred_threshold_50_subs.Rdata")))
  )
  
  # Crop predictions to eco extent (same behaviour as main gap script)
  preds <- lapply(preds, function(x) terra::crop(x, eco_vec_crop, mask = TRUE))
  template <- preds[[1]]
  
  # Resample PA to template (binary)
  pa_mask_resampled <- terra::resample(pa_raster == 1, template, method = "near")
  
  # Historical high-suitability mask (for NO-MIGRATION constraint)
  hist_mask_high <- preds[["hist"]] > thresholds[["high"]]
  hist_mask_high <- terra::resample(hist_mask_high, template, method = "near")
  
  out <- list()
  for (pname in names(preds)) {
    for (thresh_name in thresh_to_use) {
      th <- thresholds[[thresh_name]]
      pr <- preds[[pname]]
      
      cat("[OCC] Processing:", sp_code, pname, thresh_name, "\n")
      
      # ------------------
      # MIGRATION MODE
      # ------------------
      suit_migration <- pr > th
      counts_mig <- get_occ_counts_bin(suit_migration, pa_mask_resampled, occ)
      
      out[[paste(pname, thresh_name, "migration", sep = "_")]] <-
        tibble(
          mode        = "migration",
          species     = sp_name,
          sp_code     = sp_code,
          ssp         = ifelse(pname == 'hist', 'historical',
                               ifelse(grepl('245', pname), '245', '585')),
          period      = dplyr::case_when(
            pname == 'hist'       ~ 2000,
            pname == 'ssp245_30'  ~ 2030,
            pname == 'ssp245_50'  ~ 2050,
            pname == 'ssp245_70'  ~ 2070,
            pname == 'ssp585_30'  ~ 2030,
            pname == 'ssp585_50'  ~ 2050,
            pname == 'ssp585_70'  ~ 2070
          ),
          suitability        = thresh_name,
          n_occ_total        = counts_mig$n_occ_total,
          n_occ_suitable     = counts_mig$n_occ_suitable,
          n_occ_suitable_pa  = counts_mig$n_occ_suitable_pa
        )
      
      # ------------------
      # NO-MIGRATION MODE
      # Suitability constrained to historical high-suitability extent
      # ------------------
      suit_nomig <- (pr > th) & (hist_mask_high == 1)
      counts_nomig <- get_occ_counts_bin(suit_nomig, pa_mask_resampled, occ)
      
      out[[paste(pname, thresh_name, "no_migration", sep = "_")]] <-
        tibble(
          mode        = "no_migration",
          species     = sp_name,
          sp_code     = sp_code,
          ssp         = ifelse(pname == 'hist', 'historical',
                               ifelse(grepl('245', pname), '245', '585')),
          period      = dplyr::case_when(
            pname == 'hist'       ~ 2000,
            pname == 'ssp245_30'  ~ 2030,
            pname == 'ssp245_50'  ~ 2050,
            pname == 'ssp245_70'  ~ 2070,
            pname == 'ssp585_30'  ~ 2030,
            pname == 'ssp585_50'  ~ 2050,
            pname == 'ssp585_70'  ~ 2070
          ),
          suitability        = thresh_name,
          n_occ_total        = counts_nomig$n_occ_total,
          n_occ_suitable     = counts_nomig$n_occ_suitable,
          n_occ_suitable_pa  = counts_nomig$n_occ_suitable_pa
        )
    }
  }
  
  occ_results[[sp_code]] <- dplyr::bind_rows(out)
  
  cat("Finished:", sp_code, " in",
      round(as.numeric(difftime(Sys.time(), Start, units = "mins")), 2), "min\n")
}

# Bind and write
occ_results_df <- dplyr::bind_rows(occ_results)

readr::write_csv(
  occ_results_df,
  "./gap_analysis/malus_occ_coverage_counts_migration_and_no_migration.csv"
)

cat("\nAll done (occurrence counts for migration + no-migration). Total minutes:",
    round(as.numeric(difftime(Sys.time(), Start_all, units = "mins")), 2), "\n")

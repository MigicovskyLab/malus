# Top ---------------------------------------------------------------------
# Qauntifying "Taxanomic sensitivty" of SDMs
# Calculate Warren's I between each taxon*time_slice*ssp*k_fold
# Convert to T_sub_s = 1 - Warren's I
# Plot using NMDS, from distance matrix pairs
# Started October 8th 2025
# Terrell Roulston

library(tidyverse)
library(terra)
library(dismo)
library(purrr)
library(stringr)
library(vegan)

# Load SDM predictions ----------------------------------------------------
# Load only the taxa that are being considererd in the taxanomic analysis
# Malus coronaria
cor_pred_hist <- readRDS(file = './sdm_output/cor/subs/cor_pred_hist_subs.Rdata')
cor_pred_ssp245_30 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_30_subs.Rdata')
cor_pred_ssp245_50 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_50_subs.Rdata')
cor_pred_ssp245_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_70_subs.Rdata')
cor_pred_ssp585_30 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_30_subs.Rdata')
cor_pred_ssp585_50 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_50_subs.Rdata')
cor_pred_ssp585_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_70_subs.Rdata')
# Malus ioensis
ion_pred_hist <- readRDS(file = './sdm_output/ion/subs/ion_pred_hist_subs.Rdata')
ion_pred_ssp245_30 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_30_subs.Rdata')
ion_pred_ssp245_50 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_50_subs.Rdata')
ion_pred_ssp245_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_70_subs.Rdata')
ion_pred_ssp585_30 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_30_subs.Rdata')
ion_pred_ssp585_50 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_50_subs.Rdata')
ion_pred_ssp585_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_70_subs.Rdata')
# Malus angustifolia
ang_pred_hist <- readRDS(file = './sdm_output/ang/subs/ang_pred_hist_subs.Rdata')
ang_pred_ssp245_30 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_30_subs.Rdata')
ang_pred_ssp245_50 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_50_subs.Rdata')
ang_pred_ssp245_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_70_subs.Rdata')
ang_pred_ssp585_30 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_30_subs.Rdata')
ang_pred_ssp585_50 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_50_subs.Rdata')
ang_pred_ssp585_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_70_subs.Rdata')
# Sect. Chloromeles
chl_pred_hist <- readRDS(file = './sdm_output/chl/subs/chl_pred_hist_subs.Rdata')
chl_pred_ssp245_30 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_30_subs.Rdata')
chl_pred_ssp245_50 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_50_subs.Rdata')
chl_pred_ssp245_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_70_subs.Rdata')
chl_pred_ssp585_30 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_30_subs.Rdata')
chl_pred_ssp585_50 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_50_subs.Rdata')
chl_pred_ssp585_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_70_subs.Rdata')


# Load mask (background area of Chloromeles) ------------------------------
# Load ecoregion shape file
ecoNA <- vect(x = "maps/eco_regions/na_cec_eco_l2/NA_CEC_Eco_Level2.shp")
ecoNA <- project(ecoNA, 'WGS84') # project ecoregion vector to same coords ref as basemap

# Sect. Chloromeles
# Using the background area that includes the historical area and additional ecoregions where suitability expands adjacent to historical ecoregions
# Historic: "5.3" "8.1" "8.2" "8.3" "8.4" "8.5" "9.5"
# Addition: "5.2" "9.2" "9.4" "5.1" "4.1" "5.4" "3.4" "1.1"
eco_chl_code_add <- c("5.2", "8.1", "8.2", "8.3", "8.4", "8.5", "9.2", "9.4", "5.1", "4.1", "5.4", "3.4", "5.3", "9.5", "1.1")
ecoNA_chl_add <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_chl_code_add) 

# rename background mask
bgM <- ecoNA_chl_add

# Helper functions for calculation ----------------------------------------
# Indexs
taxa_codes <- c("chl","cor","ion","ang")
slices <- c("hist", "ssp245_30", "ssp245_50", "ssp245_70", "ssp585_30", "ssp585_50", "ssp585_70")

# Fetch raster object by constructed name, return NULL if not found
get_slice_r <- function(code, slice) {
  nm <- paste0(code, "_pred_", slice)  # "cor_pred_ssp245_30"
  if (exists(nm, inherits = TRUE)) get(nm, inherits = TRUE) else NULL
}

# Build a dataframe to make it easier to access each raster and associate a value for each variable (taxon, time slice and ssp)
items <- expand.grid(taxon = taxa_codes, slice = slices, stringsAsFactors = FALSE) %>%
  mutate(
    r = map2(taxon, slice, get_slice_r)
  ) %>%
  # drop combinations not loaded yet
  filter(!map_lgl(r, is.null)) %>%
  mutate(
    time = case_when(
      slice == "hist" ~ "historical",
      str_ends(slice, "_30") ~ "2030",
      str_ends(slice, "_50") ~ "2050",
      str_ends(slice, "_70") ~ "2070",
      TRUE ~ NA_character_
    ),
    ssp = if_else(slice == "hist", "hist", str_extract(slice, "ssp\\d+"))
  )

# This function inputs the suitability rasters for taxon pair i,j and the background mask from above
# it projects them onto one another and aligns using resampling (they should already by aligned but this is added saftey)
# Output is a list of masked rasters for each species pair
# r_ref = reference raster (r1)
# ref_cmp = comparison raster (r2)
# For the Chloromeles taxanomic sensitivty calculation specificailly, the Chloromeles raster = r_ref

align_mask <- function(r_ref, r_cmp, bgM) {
  r_cmp2 <- project(r_cmp, r_ref)
  r_cmp2 <- resample(r_cmp2, r_ref, method = "bilinear")
  m <- !is.na(bgM) & !is.na(r_ref) & !is.na(r_cmp2)
  list(ref = mask(r_ref, m), cmp = mask(r_cmp2, m))
}


# Wrapper function to caluclate Warren's I from the suitability rasters 
warrens_I <- function(r1, r2) {
  dismo::nicheOverlap(raster::raster(r1), raster::raster(r2), stat = "I")
}

# Pairwise Ts plot --------------------------------------------------------
pairwise_ts <- items %>%
  group_by(time, ssp) %>% # Compute Ts for each pairwise species comparison grouped by time and ssp (only compare like time and ssp)
  group_modify(~{ 
    df <- .x # take all rows
    idx <- combn(seq_len(nrow(df)), 2) # 2-dimensional combinations of all indices
    map_dfr(seq_len(ncol(idx)), function(k){ # loop over each species pair of rasters and compute I and Ts
      i <- idx[1,k]; j <- idx[2,k] # select the two-dimensional indices e.g., (chl, cor), (chl, ion), (cor, ion), etc.
      # Align the rasters and apply mask so both have the same extent/res
      al <- align_mask(df$r[[i]], df$r[[j]], bgM)
      
      # 4Compute niche overlap (Warren’s I) between the two rasters
      #   and convert it to sensitivity/dissimilarity (Tₛ = 1 - I)
      tibble(
        taxon_i = df$taxon[i],   # first taxon in the pair
        taxon_j = df$taxon[j],   # second taxon in the pair
        Ts = 1 - warrens_I(al$ref, al$cmp)  # the 1−I result
      )
    })
  }) %>% 
  #  Combine all groups back together into one long dataframe
  ungroup()

View(pairwise_ts)

# Now subset for Chloromeles-speciesI only
ts_lines <- pairwise_ts %>%
  filter(taxon_i=="chl" | taxon_j=="chl") %>%
  mutate(species = if_else(taxon_i=="chl", taxon_j, taxon_i)) %>%
  dplyr::select(time, ssp, species, Ts)

# order the time factor for plotting
ts_lines$time <- factor(ts_lines$time, levels = c("historical","2030","2050","2070"))

# pick one SSP to plot, e.g. ssp585
ssp_to_plot <- "ssp585"
df <- subset(ts_lines, ssp %in% c("hist", ssp_to_plot))  # <-- include historical

# assign colors per species
species_cols <- c(cor = "steelblue", ion = "darkorange", ang = "seagreen")

# make an empty plot frame
plot(NA, NA,
     xlim = c(1, length(levels(df$time))),
     ylim = c(0,1),
     xaxt = "n", xlab = "Time slice",
     ylab = expression(T[s]~~"(1 - Warren's I)"),
     main = paste0("Taxonomic sensitivity vs time (", ssp_to_plot, ")"))

axis(1, at = 1:length(levels(df$time)), labels = levels(df$time))

# add lines for each species
for (sp in unique(df$species)) {
  sub <- df[df$species == sp, ]
  sub <- sub[order(sub$time), ]
  lines(as.numeric(sub$time), sub$Ts,
        type = "b", pch = 18, lwd = 2, col = species_cols[sp])
}

legend("topleft",
       legend = c("cor","ion","ang"),
       col = species_cols[c("cor","ion","ang")],
       lwd = 2, pch = 19, bty = "n",
       title = "species (codes)")




# Extras? -----------------------------------------------------------------
# --- A) All-pairs Ts within an SSP panel (includes historical) ---
pairwise_ts_panel <- function(items, ssp_id){
  sel <- items %>%
    dplyr::filter(ssp %in% c("hist", ssp_id)) %>%
    dplyr::mutate(name = paste(taxon, time, sep = "_"))
  
  idx <- combn(seq_len(nrow(sel)), 2)
  purrr::map_dfr(seq_len(ncol(idx)), function(k){
    i <- idx[1, k]; j <- idx[2, k]
    al <- align_mask(sel$r[[i]], sel$r[[j]], bgM)
    tibble::tibble(
      taxon_i = sel$name[i],
      taxon_j = sel$name[j],
      Ts = 1 - warrens_I(al$ref, al$cmp)
    )
  })
}

# Now subset for Chloromeles-speciesI only
ts_lines <- pairwise_ts %>%
  filter(taxon_i=="chl" | taxon_j=="chl") %>%
  mutate(species = if_else(taxon_i=="chl", taxon_j, taxon_i)) %>%
  dplyr::select(time, ssp, species, Ts)


# --- B) Build a full distance matrix from the long table ---
build_matrix_from_pairs <- function(pairs){
  labs <- sort(unique(c(pairs$taxon_i, pairs$taxon_j)))
  M <- matrix(0, length(labs), length(labs), dimnames = list(labs, labs))
  for (k in seq_len(nrow(pairs))) {
    i <- pairs$taxon_i[k]; j <- pairs$taxon_j[k]
    M[i, j] <- M[j, i] <- pairs$Ts[k]
  }
  M
}

# --- C) Compute dense matrices for each SSP ---
pairs_245   <- pairwise_ts_panel(items, "ssp245")
pairs_585   <- pairwise_ts_panel(items, "ssp585")
dist_245_all <- build_matrix_from_pairs(pairs_245)
dist_585_all <- build_matrix_from_pairs(pairs_585)

# quick sanity checks (should have almost no zeros off-diagonal)
range(dist_245_all); mean(dist_245_all[upper.tri(dist_245_all)] == 0)

# --- D) NMDS as before ---
library(vegan)
set.seed(1)
nmds_245 <- metaMDS(as.dist(dist_245_all), k=2, trymax=100)
nmds_585 <- metaMDS(as.dist(dist_585_all), k=2, trymax=100)

extract_scores <- function(nmds, ssp_label){
  df <- as.data.frame(scores(nmds)); df$label <- rownames(df); df$ssp <- ssp_label
  pr <- strsplit(df$label, "_"); df$taxon <- sapply(pr, `[`, 1); df$time <- sapply(pr, `[`, 2); df
}
ord_all <- rbind(extract_scores(nmds_245,"ssp245"), extract_scores(nmds_585,"ssp585"))

# --- E) Clean ellipse-only plots (no connecting lines) ---
plot_ssp_NMDS_ellipses <- function(nmds_obj, scores_df, ssp_label){
  df <- subset(scores_df, ssp == ssp_label)
  df$time <- factor(df$time, levels=c("historical","2030","2050","2070"))
  taxon_cols  <- c(chl="gray40", cor="steelblue", ion="darkorange", ang="seagreen")
  time_shapes <- c(historical=21, `2030`=22, `2050`=23, `2070`=24)
  
  plot(df$NMDS1, df$NMDS2, type="n",
       main=paste("NMDS (", ssp_label, "): taxon ellipses", sep=""),
       xlab="NMDS1", ylab="NMDS2"); abline(h=0,v=0,col="grey90",lty=3)
  
  vegan::ordiellipse(nmds_obj,
                     groups=factor(df$taxon, levels=names(taxon_cols)),
                     kind="sd", conf=0.95, draw="lines",
                     col=taxon_cols, lwd=2, lty=1
  )
  
  for (tx in names(taxon_cols)) {
    pts <- df[df$taxon==tx, ]
    points(pts$NMDS1, pts$NMDS2,
           pch = unname(time_shapes[as.character(pts$time)]),
           bg  = taxon_cols[tx], col="black", cex=1.5)
    text(pts$NMDS1, pts$NMDS2, labels=pts$time, pos=3, cex=0.7)
  }
  legend("topright", legend=names(taxon_cols), col=taxon_cols, lwd=2, bty="n", title="Taxon")
  #legend("bottomright", legend=names(time_shapes), pch=time_shapes, bty="n", title="Time")
}

par(mfrow=c(1,2))
plot_ssp_NMDS_ellipses(nmds_245, ord_all, "ssp245")
plot_ssp_NMDS_ellipses(nmds_585, ord_all, "ssp585")
par(mfrow=c(1,1))


# PERMANOVA
# Convert your distance matrix to a dist object
D <- as.dist(dist_245_all)

# Build a data frame of sample metadata
meta <- data.frame(
  taxon = sapply(strsplit(rownames(dist_245_all), "_"), `[`, 1),
  time  = sapply(strsplit(rownames(dist_245_all), "_"), `[`, 2)
)

# 3Run PERMANOVA
# Ensure the distance matrix is symetrical
dist_585_all <- as.matrix(dist_585_all)
dist_585_all[lower.tri(dist_585_all)] <- t(dist_585_all)[lower.tri(dist_585_all)]
diag(dist_585_all) <- 0
# Double-check
isSymmetric(dist_585_all)  # should be TRUE

# Convert to a proper dist object 
D <- as.dist(dist_585_all)

# Build the meta again (matching order) 
meta <- data.frame(
  taxon = sapply(strsplit(rownames(dist_585_all), "_"), `[`, 1),
  time  = sapply(strsplit(rownames(dist_585_all), "_"), `[`, 2)
)

# matrix is still non-euclian
library(ade4)
D_fix <- cailliez(D)
adonis2(D_fix ~ taxon + time, data = meta)    

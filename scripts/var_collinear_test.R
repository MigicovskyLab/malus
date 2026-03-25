# Top ---------------------------------------------------------------------
# Terrell Roulston
# March 19 2026
# Quick test of variable colinearity
# Maxent does not have colinearity assumptions and handles it via regularization
# But I am testing using Pearson's r and reporting in supplement
# Still selecting using a priori

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(terra)
library(geodata)

# Occurrence points -------------------------------------------------------
## # Occurrence Points in SpatVectors
occThin_cor <- readRDS(file = './occ_data/cor/occThin_cor.Rdata') # M. coronaria
occThin_fus <- readRDS(file = './occ_data/fus/occThin_fus.Rdata') # M. fusca
occThin_ion <- readRDS(file = './occ_data/ion/occThin_ion.Rdata') # M. fusca
occThin_ang <- readRDS(file = './occ_data/ang/occThin_ang.Rdata') # M. fusca
occThin_chl <- readRDS(file = './occ_data/chl/occThin_chl.Rdata') # M. fusca

# Map prep ----------------------------------------------------------------
great_lakes <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/great lakes/combined great lakes/')
NA_ext <- ext(-180, -30, 18, 85) # Set spatial extent of analyis to NA in Western Hemisphere

# Load climate predictors -------------------------------------------------
wclim <- geodata::worldclim_global(var = 'bio',
                                   res = 2.5, 
                                   version = '2.1', 
                                   path = "./wclim_data/") %>% 
  terra::crop(NA_ext)  %>% #crop raster to NA 
  terra::mask(great_lakes, inverse = T) # cut out the great lakes

# SSP (Shared social-economic pathway) 2.45 
# middle of the road projection, high climate adaptation, low climate mitigation
ssp245_2030 <- cmip6_world(model = "CanESM5",
                           ssp = "245",
                           time = "2021-2040",
                           var = "bioc",
                           res = 2.5,
                           path = "./wclim_data/") %>% 
  crop(NA_ext) %>% #crop raster to NA 
  mask(great_lakes, inverse = T) # cut out the great lakes

ssp245_2050 <- cmip6_world(model = "CanESM5",
                           ssp = "245",
                           time = "2041-2060",
                           var = "bioc",
                           res = 2.5,
                           path = "./wclim_data/") %>% 
  crop(NA_ext) %>% #crop raster to NA 
  mask(great_lakes, inverse = T) # cut out the great lakes

ssp245_2070 <- cmip6_world(model = "CanESM5",
                           ssp = "245",
                           time = "2061-2080",
                           var = "bioc",
                           res = 2.5,
                           path = "./wclim_data/") %>% 
  crop(NA_ext) %>% #crop raster to NA 
  mask(great_lakes, inverse = T) # cut out the great lakes

# SPP 5.85 
# low regard for enviromental sustainability, increased fossil fuel reliance, this is the current tracking projection
ssp585_2030 <- cmip6_world(model = "CanESM5",
                           ssp = "585",
                           time = "2021-2040",
                           var = "bioc",
                           res = 2.5,
                           path = "./wclim_data/") %>% 
  crop(NA_ext) %>% #crop raster to NA 
  mask(great_lakes, inverse = T) # cut out the great lakes

ssp585_2050 <- cmip6_world(model = "CanESM5",
                           ssp = "585",
                           time = "2041-2060",
                           var = "bioc",
                           res = 2.5,
                           path = "./wclim_data/") %>% 
  crop(NA_ext) %>% #crop raster to NA 
  mask(great_lakes, inverse = T) # cut out the great lakes

ssp585_2070 <- cmip6_world(model = "CanESM5",
                           ssp = "585",
                           time = "2061-2080",
                           var = "bioc",
                           res = 2.5,
                           path = "./wclim_data/")%>% 
  crop(NA_ext) %>% #crop raster to NA 
  mask(great_lakes, inverse = T) # cut out the great lakes

wclim_cor <- readRDS(file = './wclim_data/wclim_cor.Rdata')
wclim_fus <- readRDS(file = './wclim_data/wclim_fus.Rdata')
wclim_ion <- readRDS(file = './wclim_data/wclim_ion.Rdata')
wclim_ang <- readRDS(file = './wclim_data/wclim_ang.Rdata')
wclim_chl <- readRDS(file = './wclim_data/wclim_chl.Rdata')

# SSP (Shared social-economic pathway) 2.45 
# middle of the road projection, high climate adaptation, low climate mitigation
climate_predictors <- names(wclim_cor) # extract climate predictor names, to ren
# Future SSPs
# Do not need to create RasterStacks
# SSP 245
names(wclim) <- climate_predictors
names(ssp245_2030) <- climate_predictors #rename raster layers for downsteam analysis
names(ssp245_2050) <- climate_predictors 
names(ssp245_2070) <- climate_predictors 
names(ssp585_2030) <- climate_predictors
names(ssp585_2050) <- climate_predictors
names(ssp585_2070) <- climate_predictors 


# Subset climate variables for SDM analysis -------------------------------
wclim_subs <- wclim %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp245_2030_subs <- ssp245_2030 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp245_2050_subs <- ssp245_2050 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp245_2070_subs <- ssp245_2070 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

ssp585_2030_subs <- ssp585_2030 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp585_2050_subs <- ssp585_2050 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp585_2070_subs <- ssp585_2070 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

wclim_cor_subs <- wclim_cor %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
wclim_fus_subs <- wclim_fus %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
wclim_ion_subs <- wclim_ion %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
wclim_ang_subs <- wclim_ang %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
wclim_chl_subs <- wclim_chl %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))


# Extract values from points ----------------------------------------------
wclim_df_cor <- terra::extract(wclim_cor_subs, occThin_cor) %>% select(!ID) %>% rename('Bio_1' = 'wc2.1_2.5m_bio_1', 'Bio_4' = 'wc2.1_2.5m_bio_4', 'Bio_10' = 'wc2.1_2.5m_bio_10', 'Bio_11' = 'wc2.1_2.5m_bio_11', 'Bio_15' = 'wc2.1_2.5m_bio_15', 'Bio_16' = 'wc2.1_2.5m_bio_16')
wclim_df_fus <- terra::extract(wclim_fus_subs, occThin_fus) %>% select(!ID) %>% rename('Bio_1' = 'wc2.1_2.5m_bio_1', 'Bio_4' = 'wc2.1_2.5m_bio_4', 'Bio_10' = 'wc2.1_2.5m_bio_10', 'Bio_11' = 'wc2.1_2.5m_bio_11', 'Bio_15' = 'wc2.1_2.5m_bio_15', 'Bio_16' = 'wc2.1_2.5m_bio_16')
wclim_df_ion <- terra::extract(wclim_ion_subs, occThin_ion) %>% select(!ID) %>% rename('Bio_1' = 'wc2.1_2.5m_bio_1', 'Bio_4' = 'wc2.1_2.5m_bio_4', 'Bio_10' = 'wc2.1_2.5m_bio_10', 'Bio_11' = 'wc2.1_2.5m_bio_11', 'Bio_15' = 'wc2.1_2.5m_bio_15', 'Bio_16' = 'wc2.1_2.5m_bio_16')
wclim_df_ang <- terra::extract(wclim_ang_subs, occThin_ang) %>% select(!ID) %>% rename('Bio_1' = 'wc2.1_2.5m_bio_1', 'Bio_4' = 'wc2.1_2.5m_bio_4', 'Bio_10' = 'wc2.1_2.5m_bio_10', 'Bio_11' = 'wc2.1_2.5m_bio_11', 'Bio_15' = 'wc2.1_2.5m_bio_15', 'Bio_16' = 'wc2.1_2.5m_bio_16')
wclim_df_chl <- terra::extract(wclim_chl_subs, occThin_chl) %>% select(!ID) %>% rename('Bio_1' = 'wc2.1_2.5m_bio_1', 'Bio_4' = 'wc2.1_2.5m_bio_4', 'Bio_10' = 'wc2.1_2.5m_bio_10', 'Bio_11' = 'wc2.1_2.5m_bio_11', 'Bio_15' = 'wc2.1_2.5m_bio_15', 'Bio_16' = 'wc2.1_2.5m_bio_16')

# Pearson's correlation test ----------------------------------------------
cor_mat_cor <- stats::cor(wclim_df_cor, use = "pairwise.complete.obs", method = "pearson") %>% round(., digits = 2)
cor_mat_fus <- stats::cor(wclim_df_fus, use = "pairwise.complete.obs", method = "pearson") %>% round(., digits = 2)
cor_mat_ion <- stats::cor(wclim_df_ion, use = "pairwise.complete.obs", method = "pearson") %>% round(., digits = 2)
cor_mat_ang <- stats::cor(wclim_df_ang, use = "pairwise.complete.obs", method = "pearson") %>% round(., digits = 2)
cor_mat_chl <- stats::cor(wclim_df_chl, use = "pairwise.complete.obs", method = "pearson") %>% round(., digits = 2)

list(cor_mat_cor, cor_mat_fus, cor_mat_ion, cor_mat_ang, cor_mat_chl)


# Convert cor matrices to table -------------------------------------------
# Function to convert correlation matrix to tidy format
cor_to_df <- function(cor_mat, species_name) {
  as.data.frame(as.table(cor_mat)) %>%
    rename(r = Freq) %>%
    mutate(Species = species_name) %>%
    filter(Var1 != Var2) %>%              # remove diagonal
    rowwise() %>%
    mutate(pair = paste(sort(c(Var1, Var2)), collapse = "_")) %>%
    ungroup() %>%
    distinct(Species, pair, .keep_all = TRUE) %>%  # remove duplicates
    select(Species, Var1, Var2, r) %>% 
    mutate('r > 0.7' = ifelse(abs(r) > 0.7, "Y", ""))
}

# Apply to all species
cor_df_all <- bind_rows(
  cor_to_df(cor_mat_cor, "Cor"),
  cor_to_df(cor_mat_fus, "Fus"),
  cor_to_df(cor_mat_ion, "Ion"),
  cor_to_df(cor_mat_ang, "Ang"),
  cor_to_df(cor_mat_chl, "Chl")
)

# Save cor table ----------------------------------------------------------

write.csv(cor_df_all, "./wclim_data/correlation_table_all_species.csv", row.names = FALSE)


# Format matrices and output as table -------------------------------------

format_cor_matrix <- function(cor_mat, species_name, threshold = 0.7, digits = 2) {
  
  mat <- round(cor_mat, digits)
  
  # convert to character for formatting
  mat_chr <- matrix(
    as.character(mat),
    nrow = nrow(mat),
    dimnames = dimnames(mat)
  )
  
  # blank upper triangle
  mat_chr[upper.tri(mat_chr)] <- ""
  
  # add * for |r| > threshold (lower triangle only)
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (i > j && abs(mat[i, j]) > threshold) {
        mat_chr[i, j] <- paste0(mat_chr[i, j], "*")
      }
    }
  }
  
  # return clean df
  data.frame(
    Species = species_name,
    Variable = rownames(mat_chr),
    mat_chr,
    row.names = NULL,
    check.names = FALSE
  )
}

# apply
cor_tables <- list(
  format_cor_matrix(cor_mat_cor, "Cor"),
  format_cor_matrix(cor_mat_fus, "Fus"),
  format_cor_matrix(cor_mat_ion, "Ion"),
  format_cor_matrix(cor_mat_ang, "Ang"),
  format_cor_matrix(cor_mat_chl, "Chl")
)

# combine into one CSV (with blank rows between species)
cor_combined <- bind_rows(
  cor_tables[[1]], tibble(),
  cor_tables[[2]], tibble(),
  cor_tables[[3]], tibble(),
  cor_tables[[4]], tibble(),
  cor_tables[[5]]
)

write.csv(cor_combined, "./wclim_data/correlation_matrices_all_species.csv",
          row.names = FALSE, na = "")

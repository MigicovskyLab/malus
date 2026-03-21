# Top ---------------------------------------------------------------------
# Terrell Roulston; Thomas Connor
# Started Feb 21, 2024

# New MESS analysis using <predicts> --------------------------------------
# This analysis is contributed by **Thomas Connor**
# March 21 2026

# Mask future predictions

# Climates is a list of raster files with given clim data for some number or all scenarios
# occCoords is x y pos used in training maxent
# croppedClimate is the cropped historical climate used to train maxent, a subset of climates where name == historical


# Libraries ---------------------------------------------------------------
library(tidyverse) # Data management
library(terra) # GIS
library(predicts) # MESS
library(geodata) # Wclim data

# Helper functions --------------------------------------------------------
apply_mess <- function(climates, occCoords, croppedClimate, progress) {
  trainingEnv <- terra::extract(croppedClimate, occCoords, ID = FALSE)
  
  messRasters <- pmap(list(climates), \(clim) {
    predicts::mess(clim, trainingEnv, progress = progress) # progress bar, takes numeric
  })
}

# Import climate predictions and occurrence data --------------------------
# Occurrence data
occThin_cor <- readRDS(file = './occ_data/cor/occThin_cor.Rdata') # M. coronaria
occThin_fus <- readRDS(file = './occ_data/fus/occThin_fus.Rdata') # M. fusca
occThin_ion <- readRDS(file = './occ_data/ion/occThin_ion.Rdata') # M. ioensis
occThin_ang <- readRDS(file = './occ_data/ang/occThin_ang.Rdata') # M. angustifolia
occThin_chl <- readRDS(file = './occ_data/chl/occThin_chl.Rdata') # Chloromeles

# Cropped climate data <trainingEnv>
wclim_cor <- readRDS('./wclim_data/wclim_cor.Rdata') %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

wclim_fus <- readRDS('./wclim_data/wclim_fus.Rdata') %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

wclim_ion <- readRDS('./wclim_data/wclim_ion.Rdata') %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

wclim_ang <- readRDS('./wclim_data/wclim_ang.Rdata') %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

wclim_chl <- readRDS('./wclim_data/wclim_chl.Rdata') %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

# Wclim historical and SSP245/585
# Climate Data
great_lakes <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/great lakes/combined great lakes/')

NA_ext <- ext(-180, -30, 18, 85) # Set spatial extent of analyis to NA in Western Hemisphere

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

wclim <- geodata::worldclim_global(var = 'bio',
                                   res = 2.5, 
                                   version = '2.1', 
                                   path = "./wclim_data/") %>% 
  terra::crop(NA_ext)  %>% #crop raster to NA 
  terra::mask(great_lakes, inverse = T) # cut out the great lakes

# SSP (Shared social-economic pathway) 2.45 
# middle of the road projection, high climate adaptation, low climate mitigation
climate_predictors <- names(wclim) # extract climate predictor names, to ren
# Future SSPs
# SSP 245
names(ssp245_2030) <- climate_predictors #rename raster layers for downsteam analysis
names(ssp245_2050) <- climate_predictors 
names(ssp245_2070) <- climate_predictors 
names(ssp585_2030) <- climate_predictors
names(ssp585_2050) <- climate_predictors
names(ssp585_2070) <- climate_predictors 


# Subset climate variables for SDM analysis 
wclim_subs <- wclim %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp245_2030_subs <- ssp245_2030 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp245_2050_subs <- ssp245_2050 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp245_2070_subs <- ssp245_2070 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))

ssp585_2030_subs <- ssp585_2030 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp585_2050_subs <- ssp585_2050 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))
ssp585_2070_subs <- ssp585_2070 %>% terra::subset(c('wc2.1_2.5m_bio_1', 'wc2.1_2.5m_bio_4', 'wc2.1_2.5m_bio_10', 'wc2.1_2.5m_bio_11', 'wc2.1_2.5m_bio_15', 'wc2.1_2.5m_bio_16'))


# Data prep ---------------------------------------------------------------
# Reshape data to fit MESS function

# List climate data and name the elements as slice names
climate_list <- list(wclim_subs, 
                     ssp245_2030_subs, ssp245_2050_subs, ssp245_2070_subs,
                     ssp585_2030_subs, ssp585_2050_subs, ssp585_2070_subs)

names(climate_list) <- c('wclim', 
                        "ssp245_2030", "ssp245_2050", "spp245_2070", 
                        "ssp585_2030", "ssp585_2050", "ssp585_2070")

# Extract coordinates from occurrences
occ_crd_cor <- terra::geom(occThin_cor) %>% as.data.frame() %>% select(x, y)
occ_crd_fus <- terra::geom(occThin_fus) %>% as.data.frame() %>% select(x, y)
occ_crd_ion <- terra::geom(occThin_ion) %>% as.data.frame() %>% select(x, y)
occ_crd_ang <- terra::geom(occThin_ang) %>% as.data.frame() %>% select(x, y)
occ_crd_chl <- terra::geom(occThin_chl) %>% as.data.frame() %>% select(x, y)

# Run MESS analysis -------------------------------------------------------
# What is the output of predicts:mess??
# Malus coronaria

mess_cor <- apply_mess(climates = climate_list, occCoords = occ_crd_cor, croppedClimate = wclim_cor)


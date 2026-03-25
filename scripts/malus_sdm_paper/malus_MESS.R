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
library(magick) # Arranging pngs
library(scales) # added plotting func

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
                        "ssp245_2030", "ssp245_2050", "ssp245_2070", 
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

# mess_cor <- apply_mess(climates = climate_list, occCoords = occ_crd_cor, croppedClimate = wclim_cor, progress = 1)
# mess_fus <- apply_mess(climates = climate_list, occCoords = occ_crd_fus, croppedClimate = wclim_fus, progress = 1)
# mess_ion <- apply_mess(climates = climate_list, occCoords = occ_crd_ion, croppedClimate = wclim_ion, progress = 1)
# mess_ang <- apply_mess(climates = climate_list, occCoords = occ_crd_ang, croppedClimate = wclim_ang, progress = 1)
# mess_chl <- apply_mess(climates = climate_list, occCoords = occ_crd_chl, croppedClimate = wclim_chl, progress = 1)

# Save MESS analysis ------------------------------------------------------
# # M coronaria
# dir.create("./mess_data/mess_cor", recursive = TRUE, showWarnings = FALSE)
# 
# for (nm in names(mess_cor)) {
#   writeRaster(
#     mess_cor[[nm]],
#     filename = file.path("./mess_data/mess_cor", paste0(nm, ".tif")),
#     overwrite = TRUE
#   )
# }
# 
# # M fusca
# dir.create("./mess_data/mess_fus", recursive = TRUE, showWarnings = FALSE)
# 
# for (nm in names(mess_fus)) {
#   writeRaster(
#     mess_fus[[nm]],
#     filename = file.path("./mess_data/mess_fus", paste0(nm, ".tif")),
#     overwrite = TRUE
#   )
# }
# 
# # M ioensis
# dir.create("./mess_data/mess_ion", recursive = TRUE, showWarnings = FALSE)
# 
# for (nm in names(mess_ion)) {
#   writeRaster(
#     mess_ion[[nm]],
#     filename = file.path("./mess_data/mess_ion", paste0(nm, ".tif")),
#     overwrite = TRUE
#   )
# }
# 
# # M angustifolia
# dir.create("./mess_data/mess_ang", recursive = TRUE, showWarnings = FALSE)
# 
# for (nm in names(mess_ang)) {
#   writeRaster(
#     mess_ang[[nm]],
#     filename = file.path("./mess_data/mess_ang", paste0(nm, ".tif")),
#     overwrite = TRUE
#   )
# }
# 
# # Sect. Chloromeles
# dir.create("./mess_data/mess_chl", recursive = TRUE, showWarnings = FALSE)
# 
# for (nm in names(mess_chl)) {
#   writeRaster(
#     mess_chl[[nm]],
#     filename = file.path("./mess_data/mess_chl", paste0(nm, ".tif")),
#     overwrite = TRUE
#   )
# }

# Load MESS analysis ------------------------------------------------------
# M coronaria
mess_cor <- list(
  wclim        = rast("./mess_data/mess_cor/wclim.tif"),
  ssp245_2030  = rast("./mess_data/mess_cor/ssp245_2030.tif"),
  ssp245_2050  = rast("./mess_data/mess_cor/ssp245_2050.tif"),
  ssp245_2070  = rast("./mess_data/mess_cor/ssp245_2070.tif"),
  ssp585_2030  = rast("./mess_data/mess_cor/ssp585_2030.tif"),
  ssp585_2050  = rast("./mess_data/mess_cor/ssp585_2050.tif"),
  ssp585_2070  = rast("./mess_data/mess_cor/ssp585_2070.tif")
)

# M fusca
mess_fus <- list(
  wclim        = rast("./mess_data/mess_fus/wclim.tif"),
  ssp245_2030  = rast("./mess_data/mess_fus/ssp245_2030.tif"),
  ssp245_2050  = rast("./mess_data/mess_fus/ssp245_2050.tif"),
  ssp245_2070  = rast("./mess_data/mess_fus/ssp245_2070.tif"),
  ssp585_2030  = rast("./mess_data/mess_fus/ssp585_2030.tif"),
  ssp585_2050  = rast("./mess_data/mess_fus/ssp585_2050.tif"),
  ssp585_2070  = rast("./mess_data/mess_fus/ssp585_2070.tif")
)

# M ioensis
mess_ion <- list(
  wclim        = rast("./mess_data/mess_ion/wclim.tif"),
  ssp245_2030  = rast("./mess_data/mess_ion/ssp245_2030.tif"),
  ssp245_2050  = rast("./mess_data/mess_ion/ssp245_2050.tif"),
  ssp245_2070  = rast("./mess_data/mess_ion/ssp245_2070.tif"),
  ssp585_2030  = rast("./mess_data/mess_ion/ssp585_2030.tif"),
  ssp585_2050  = rast("./mess_data/mess_ion/ssp585_2050.tif"),
  ssp585_2070  = rast("./mess_data/mess_ion/ssp585_2070.tif")
)

# M angustifolia
mess_ang <- list(
  wclim        = rast("./mess_data/mess_ang/wclim.tif"),
  ssp245_2030  = rast("./mess_data/mess_ang/ssp245_2030.tif"),
  ssp245_2050  = rast("./mess_data/mess_ang/ssp245_2050.tif"),
  ssp245_2070  = rast("./mess_data/mess_ang/ssp245_2070.tif"),
  ssp585_2030  = rast("./mess_data/mess_ang/ssp585_2030.tif"),
  ssp585_2050  = rast("./mess_data/mess_ang/ssp585_2050.tif"),
  ssp585_2070  = rast("./mess_data/mess_ang/ssp585_2070.tif")
)

# Sect. Chloromeles
mess_chl <- list(
  wclim        = rast("./mess_data/mess_chl/wclim.tif"),
  ssp245_2030  = rast("./mess_data/mess_chl/ssp245_2030.tif"),
  ssp245_2050  = rast("./mess_data/mess_chl/ssp245_2050.tif"),
  ssp245_2070  = rast("./mess_data/mess_chl/ssp245_2070.tif"),
  ssp585_2030  = rast("./mess_data/mess_chl/ssp585_2030.tif"),
  ssp585_2050  = rast("./mess_data/mess_chl/ssp585_2050.tif"),
  ssp585_2070  = rast("./mess_data/mess_chl/ssp585_2070.tif")
)

# Load SDM predictions ----------------------------------------------------
# Histotical 
cor_pred_hist <- readRDS(file = './sdm_output/cor/subs/cor_pred_hist_subs.Rdata')
fus_pred_hist <- readRDS(file = './sdm_output/fus/subs/fus_pred_hist_subs.Rdata')
ion_pred_hist <- readRDS(file = './sdm_output/ion/subs/ion_pred_hist_subs.Rdata')
ang_pred_hist <- readRDS(file = './sdm_output/ang/subs/ang_pred_hist_subs.Rdata')
chl_pred_hist <- readRDS(file = './sdm_output/chl/subs/chl_pred_hist_subs.Rdata')

# SSP245 2030
cor_pred_ssp245_30 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_30_subs.Rdata')
fus_pred_ssp245_30 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp245_30_subs.Rdata')
ion_pred_ssp245_30 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_30_subs.Rdata')
ang_pred_ssp245_30 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_30_subs.Rdata')
chl_pred_ssp245_30 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_30_subs.Rdata')

# SSP245 2050
cor_pred_ssp245_50 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_50_subs.Rdata')
fus_pred_ssp245_50 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp245_50_subs.Rdata')
ion_pred_ssp245_50 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_50_subs.Rdata')
ang_pred_ssp245_50 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_50_subs.Rdata')
chl_pred_ssp245_50 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_50_subs.Rdata')

# SSP245 2070
cor_pred_ssp245_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_70_subs.Rdata')
fus_pred_ssp245_70 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp245_70_subs.Rdata')
ion_pred_ssp245_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_70_subs.Rdata')
ang_pred_ssp245_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_70_subs.Rdata')
chl_pred_ssp245_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_70_subs.Rdata')

# SSP585 2030
cor_pred_ssp585_30 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_30_subs.Rdata')
fus_pred_ssp585_30 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_30_subs.Rdata')
ion_pred_ssp585_30 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_30_subs.Rdata')
ang_pred_ssp585_30 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_30_subs.Rdata')
chl_pred_ssp585_30 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_30_subs.Rdata')

# SSP585 2050
cor_pred_ssp585_50 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_50_subs.Rdata')
fus_pred_ssp585_50 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_50_subs.Rdata')
ion_pred_ssp585_50 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_50_subs.Rdata')
ang_pred_ssp585_50 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_50_subs.Rdata')
chl_pred_ssp585_50 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_50_subs.Rdata')

# SSP585 2070
cor_pred_ssp585_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_70_subs.Rdata')
fus_pred_ssp585_70 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_70_subs.Rdata')
ion_pred_ssp585_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_70_subs.Rdata')
ang_pred_ssp585_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_70_subs.Rdata')
chl_pred_ssp585_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_70_subs.Rdata')

# Extend
cor_pred_hist_extend <- terra::extend(cor_pred_hist, NA_ext)
fus_pred_hist_extend <- terra::extend(fus_pred_hist, NA_ext)
ion_pred_hist_extend <- terra::extend(ion_pred_hist, NA_ext)
ang_pred_hist_extend <- terra::extend(ang_pred_hist, NA_ext)
chl_pred_hist_extend <- terra::extend(chl_pred_hist, NA_ext)
cor_pred_ssp245_30_extend <- terra::extend(cor_pred_ssp245_30, NA_ext)
fus_pred_ssp245_30_extend <- terra::extend(fus_pred_ssp245_30, NA_ext)
ion_pred_ssp245_30_extend <- terra::extend(ion_pred_ssp245_30, NA_ext)
ang_pred_ssp245_30_extend <- terra::extend(ang_pred_ssp245_30, NA_ext)
chl_pred_ssp245_30_extend <- terra::extend(chl_pred_ssp245_30, NA_ext)
cor_pred_ssp245_50_extend <- terra::extend(cor_pred_ssp245_50, NA_ext)
fus_pred_ssp245_50_extend <- terra::extend(fus_pred_ssp245_50, NA_ext)
ion_pred_ssp245_50_extend <- terra::extend(ion_pred_ssp245_50, NA_ext)
ang_pred_ssp245_50_extend <- terra::extend(ang_pred_ssp245_50, NA_ext)
chl_pred_ssp245_50_extend <- terra::extend(chl_pred_ssp245_50, NA_ext)
cor_pred_ssp245_70_extend <- terra::extend(cor_pred_ssp245_70, NA_ext)
fus_pred_ssp245_70_extend <- terra::extend(fus_pred_ssp245_70, NA_ext)
ion_pred_ssp245_70_extend <- terra::extend(ion_pred_ssp245_70, NA_ext)
ang_pred_ssp245_70_extend <- terra::extend(ang_pred_ssp245_70, NA_ext)
chl_pred_ssp245_70_extend <- terra::extend(chl_pred_ssp245_70, NA_ext)
cor_pred_ssp585_30_extend <- terra::extend(cor_pred_ssp585_30, NA_ext)
fus_pred_ssp585_30_extend <- terra::extend(fus_pred_ssp585_30, NA_ext)
ion_pred_ssp585_30_extend <- terra::extend(ion_pred_ssp585_30, NA_ext)
ang_pred_ssp585_30_extend <- terra::extend(ang_pred_ssp585_30, NA_ext)
chl_pred_ssp585_30_extend <- terra::extend(chl_pred_ssp585_30, NA_ext)
cor_pred_ssp585_50_extend <- terra::extend(cor_pred_ssp585_50, NA_ext)
fus_pred_ssp585_50_extend <- terra::extend(fus_pred_ssp585_50, NA_ext)
ion_pred_ssp585_50_extend <- terra::extend(ion_pred_ssp585_50, NA_ext)
ang_pred_ssp585_50_extend <- terra::extend(ang_pred_ssp585_50, NA_ext)
chl_pred_ssp585_50_extend <- terra::extend(chl_pred_ssp585_50, NA_ext)
cor_pred_ssp585_70_extend <- terra::extend(cor_pred_ssp585_70, NA_ext)
fus_pred_ssp585_70_extend <- terra::extend(fus_pred_ssp585_70, NA_ext)
ion_pred_ssp585_70_extend <- terra::extend(ion_pred_ssp585_70, NA_ext)
ang_pred_ssp585_70_extend <- terra::extend(ang_pred_ssp585_70, NA_ext)
chl_pred_ssp585_70_extend <- terra::extend(chl_pred_ssp585_70, NA_ext)

# High suit threshold
corPred_threshold_50 <- readRDS(file = './sdm_output/cor/subs/threshold/corPred_threshold_50_subs.Rdata')
fusPred_threshold_50 <- readRDS(file = './sdm_output/fus/subs/threshold/fusPred_threshold_50_subs.Rdata')
ionPred_threshold_50 <- readRDS(file = './sdm_output/ion/subs/threshold/ionPred_threshold_50_subs.Rdata')
angPred_threshold_50 <- readRDS(file = './sdm_output/ang/subs/threshold/angPred_threshold_50_subs.Rdata')
chlPred_threshold_50 <- readRDS(file = './sdm_output/chl/subs/threshold/chlPred_threshold_50_subs.Rdata')


# Load ecoregions ---------------------------------------------------------
ecoNA <- vect(x = "maps/eco_regions/na_cec_eco_l2/NA_CEC_Eco_Level2.shp")
ecoNA <- project(ecoNA, 'WGS84') # project ecoregion vector to same coords ref as basemap


# M. fusca
# Add 6.1?? a small area of suitability in southern Alaska is missed under 2070
# Historic: "7.1""6.2"  "10.1" "11.1" "10.2"
# Addition: 6.1, 2.2
eco_fus_code_old <- c("7.1", "6.2", "10.1", "11.1", "10.2")
eco_fus_code_new <- c("6.1", "2.2")
ecoNA_fus_old <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_fus_code_old)
ecoNA_fus_new <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_fus_code_new)

# M. coronaria
# Historic: "8.1" "8.2" "5.3" "8.4" "8.3" "8.5" "9.2" "9.4" "5.2"
# Addition: 5.1 and 3.4
eco_cor_code_old <- c("8.1", "8.2", "5.3", "8.4", "8.3", "8.5", "9.2", "9.4", "5.2")
eco_cor_code_new <- c("5.1", '3.4')
ecoNA_cor_old <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_cor_code_old)
ecoNA_cor_new <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_cor_code_new)

# M. ioensis
# Historic: "5.2" "8.1" "8.2" "8.3" "8.4" "8.5" "9.2" "9.4"
# Addition: 5.1, 4.1, 5.4 and 3.4
eco_ion_code_old <- c("5.2", "8.1", "8.2", "8.3", "8.4", "8.5", "9.2", "9.4")
eco_ion_code_new <- c("5.1", "5.4", "3.4")
ecoNA_ion_old <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_ion_code_old)
ecoNA_ion_new <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_ion_code_new)

# M. angustifolia
# Historic: "5.3" "8.1" "8.2" "8.3" "8.4" "8.5" "9.5"
# Addition: "9.2", "9.4", "5.2", "5.1", '3.4', 1.1 (adding more than need i think but mirroring coronaria)
eco_ang_code_old <- c("5.3", "8.1", "8.2", "8.3", "8.4", "8.5", "9.5")
eco_ang_code_new <- c("9.2", "9.4", "5.2", "5.1", '3.4', "1.1")
ecoNA_ang_old <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_ang_code_old)
ecoNA_ang_new <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_ang_code_new)

# Sect. Chloromeles
# Historic: "5.3" "5.2" "8.1" "8.2" "8.3" "8.4" "8.5" "9.5" 
# Addition:  "9.2" "9.4" "5.1" "4.1" "5.4" "3.4" "1.1"
eco_chl_code_old <- c("5.3", "8.1", "8.2", "8.3", "8.4", "8.5", "9.5")
eco_chl_code_new <- c("9.2", "9.4", "5.2", "5.1", '3.4', "1.1", "4.1")
ecoNA_chl_old <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_chl_code_old) 
ecoNA_chl_new <- terra::subset(ecoNA, ecoNA$NA_L2CODE %in% eco_chl_code_new)

#  Plot MESS analysis **M coronaria** -------------------------------------


# Historical
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_historical.png", width = 2000, height = 2000, res = 300)


plot(mess_cor$wclim > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = "Historical"
)

terra::plot(cor_pred_hist_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

# legend(
#   x = -175, y = 47.75,
#   legend = c("Analogous", "Non-analogous", "High suitability"),
#   fill   = c("#FFC20A", "white", alpha('black', alpha = 0.3)),
#   border = c("black", "black", "black"),
#   bg     = "white",
#   bty    = "o",
#   xjust  = 0,
#   yjust  = 1,
#   cex = 2)
# 
# legend(x = -175, y = 47.25,
#        legend = c("Historical\necoregions", "Expansion\necoregions"),
#        col    = c("#D81B60", "#0C7BDC"),
#        lty    = c(1, 1),
#        lwd    = c(2, 2),
#        bg     = "white",
#        bty    = "o",
#        xjust  = 0,
#        yjust  = 2,
#        y.intersp = 1.5,
#        cex = 2)
#        
       
dev.off()

# SSP245 2030

jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_ssp245_2030.png", width = 2000, height = 2000, res = 300)


plot(mess_cor$ssp245_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     main = 'SSP245 2030',
     axes = F
)

terra::plot(cor_pred_ssp245_30_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2050

jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_ssp245_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_cor$ssp245_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2050'
)

terra::plot(cor_pred_ssp245_50_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_ssp245_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_cor$ssp245_2070 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     main = 'SSP245 2070',
     axes = F
)

terra::plot(cor_pred_ssp245_70_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_ssp585_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_cor$ssp585_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = "SSP585 2030"
)

terra::plot(cor_pred_ssp585_30_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP585 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_ssp585_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_cor$ssp585_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2050'
)

terra::plot(cor_pred_ssp585_50_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_ssp585_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_cor$ssp585_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP585 2070'
)

terra::plot(cor_pred_ssp585_70_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_cor_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_cor_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# Plot MESS analysis **M fusca**  -----------------------------------------
# Historical
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_historical.png", width = 2000, height = 2000, res = 300)

plot(mess_fus$wclim > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = "Historical"
)

terra::plot(fus_pred_hist_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_ssp245_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_fus$ssp245_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2030'
)

terra::plot(fus_pred_ssp245_30_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_ssp245_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_fus$ssp245_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2050'
)

terra::plot(fus_pred_ssp245_50_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_ssp245_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_fus$ssp245_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP245 2070'
)

terra::plot(fus_pred_ssp245_70_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_ssp585_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_fus$ssp585_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2030'
)

terra::plot(fus_pred_ssp585_30_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_ssp585_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_fus$ssp585_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2050'
)

terra::plot(fus_pred_ssp585_50_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_ssp585_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_fus$ssp585_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP585 2070'
)

terra::plot(fus_pred_ssp585_70_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_fus_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_fus_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# Plot MESS analysis **M ioensis**  -----------------------------------------
# Historical
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_historical.png", width = 2000, height = 2000, res = 300)

plot(mess_ion$wclim > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = "Historical"
)

terra::plot(ion_pred_hist_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_ssp245_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_ion$ssp245_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2030'
)

terra::plot(ion_pred_ssp245_30_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_ssp245_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_ion$ssp245_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2050'
)

terra::plot(ion_pred_ssp245_50_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_ssp245_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_ion$ssp245_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP245 2070'
)

terra::plot(ion_pred_ssp245_70_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_ssp585_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_ion$ssp585_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2030'
)

terra::plot(ion_pred_ssp585_30_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_ssp585_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_ion$ssp585_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2050'
)

terra::plot(ion_pred_ssp585_50_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_ssp585_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_ion$ssp585_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP585 2070'
)

terra::plot(ion_pred_ssp585_70_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ion_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ion_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# Plot MESS analysis **M angustifolia**  -----------------------------------------
# Historical
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_historical.png", width = 2000, height = 2000, res = 300)

plot(mess_ang$wclim > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = "Historical"
)

terra::plot(ang_pred_hist_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_ssp245_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_ang$ssp245_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2030'
)

terra::plot(ang_pred_ssp245_30_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_ssp245_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_ang$ssp245_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2050'
)

terra::plot(ang_pred_ssp245_50_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_ssp245_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_ang$ssp245_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP245 2070'
)

terra::plot(ang_pred_ssp245_70_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_ssp585_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_ang$ssp585_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2030'
)

terra::plot(ang_pred_ssp585_30_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_ssp585_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_ang$ssp585_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2050'
)

terra::plot(ang_pred_ssp585_50_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_ssp585_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_ang$ssp585_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP585 2070'
)

terra::plot(ang_pred_ssp585_70_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_ang_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_ang_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# Plot MESS analysis **Sect. Chloromeles**  -----------------------------------------
# Historical
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_historical.png", width = 2000, height = 2000, res = 300)

plot(mess_chl$wclim > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = "Historical"
)

terra::plot(chl_pred_hist_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_ssp245_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_chl$ssp245_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2030'
)

terra::plot(chl_pred_ssp245_30_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_ssp245_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_chl$ssp245_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP245 2050'
)

terra::plot(chl_pred_ssp245_50_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# SSP245 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_ssp245_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_chl$ssp245_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP245 2070'
)

terra::plot(chl_pred_ssp245_70_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_ssp585_2030.png", width = 2000, height = 2000, res = 300)

plot(mess_chl$ssp585_2030 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2030'
)

terra::plot(chl_pred_ssp585_30_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_ssp585_2050.png", width = 2000, height = 2000, res = 300)

plot(mess_chl$ssp585_2050 > 0,
     col = c('white', "#FFC20A"),
     background = 'lightblue',
     legend = F,
     axes = F,
     main = 'SSP585 2050'
)

terra::plot(chl_pred_ssp585_50_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()

# ssp585 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_ssp585_2070.png", width = 2000, height = 2000, res = 300)

terra::plot(mess_chl$ssp585_2070 > 0,
            col = c('white', "#FFC20A"),
            background = 'lightblue',
            legend = F,
            axes = F,
            main = 'SSP585 2070'
)

terra::plot(chl_pred_ssp585_70_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

terra::plot(ecoNA_chl_old,
            border = '#D81B60',
            col = NA,
            add = T,
            lwd = 0.5
)

terra::plot(ecoNA_chl_new,
            border = '#0C7BDC',
            add = T,
            lwd = 0.5
)

dev.off()


# Make supplement figure from pngs ----------------------------------------
# Helper function
make_mess_2x4 <- function(sp_dir, sp_name, out_file, file_prefix,
                          width = 2000, height = 2000,
                          title_height = 220,
                          title_size = 120) {
  
  img_paths <- c(
    file.path(sp_dir, paste0(file_prefix, "_historical.png")),
    file.path(sp_dir, paste0(file_prefix, "_ssp245_2030.png")),
    file.path(sp_dir, paste0(file_prefix, "_ssp245_2050.png")),
    file.path(sp_dir, paste0(file_prefix, "_ssp245_2070.png")),
    file.path(sp_dir, paste0(file_prefix, "_ssp585_2030.png")),
    file.path(sp_dir, paste0(file_prefix, "_ssp585_2050.png")),
    file.path(sp_dir, paste0(file_prefix, "_ssp585_2070.png"))
  )
  
  imgs <- lapply(img_paths, function(x) {
    if (!file.exists(x)) stop(paste("Missing file:", x))
    image_read(x) |>
      image_resize(paste0(width, "x", height, "!"))
  })
  
  # make legend panel
  tf <- tempfile(fileext = ".png")
  png(tf, width = width, height = height, res = 300)
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  
  legend(
    x = 0.08, y = 0.82,
    legend = c("Analogous", "Non-analogous", "High suitability"),
    fill   = c("#FFC20A", "white", alpha("black", alpha = 0.3)),
    border = c("black", "black", "black"),
    bg     = "white",
    bty    = "o",
    xjust  = 0,
    yjust  = 1,
    cex    = 2.2
  )
  
  legend(
    x = 0.08, y = 0.48,
    legend = c("Historical\necoregions", "Expansion\necoregions"),
    col    = c("#D81B60", "#0C7BDC"),
    lty    = c(1, 1),
    lwd    = c(3, 3),
    bg     = "white",
    bty    = "o",
    xjust  = 0,
    yjust  = 1,
    y.intersp = 1.5,
    cex    = 2.2
  )
  
  dev.off()
  
  legend_panel <- image_read(tf) |>
    image_resize(paste0(width, "x", height, "!"))
  
  # combine into 8 panels
  all_panels <- c(imgs, list(legend_panel))
  
  row1 <- image_append(do.call(c, all_panels[1:4]))
  row2 <- image_append(do.call(c, all_panels[5:8]))
  
  panel_grid <- image_append(c(row1, row2), stack = TRUE)
  
  title_strip <- image_blank(
    width = image_info(panel_grid)$width,
    height = title_height,
    color = "white"
  ) |>
    image_annotate(
      text = sp_name,
      size = title_size,
      gravity = "center",
      color = "black"
    )
  
  final <- image_append(c(title_strip, panel_grid), stack = TRUE)
  
  image_write(final, out_file)
  return(final)
}

make_mess_2x4(
  sp_dir = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus",
  sp_name = "M. fusca",
  out_file = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/fus/fusca_2x4_panel.png",
  file_prefix = "fusca"
)

make_mess_2x4(
  sp_dir = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor",
  sp_name = "M. coronaria",
  out_file = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/cor/coronaria_2x4_panel.png",
  file_prefix = "coronaria"
)

make_mess_2x4(
  sp_dir = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion",
  sp_name = "M. ioensis",
  out_file = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ion/ioensis_2x4_panel.png",
  file_prefix = "ioensis"
)

make_mess_2x4(
  sp_dir = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang",
  sp_name = "M. angustifolia",
  out_file = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/ang/angustifolia_2x4_panel.png",
  file_prefix = "angustifolia"
)

make_mess_2x4(
  sp_dir = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl",
  sp_name = "Sect. Chloromeles",
  out_file = "C:/Users/terre/Documents/Acadia/Malus Project/mess_plots/chl/chloromeles_2x4_panel.png",
  file_prefix = "chloromeles"
)

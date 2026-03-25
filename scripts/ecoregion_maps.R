# Top ---------------------------------------------------------------------
# Terrell Roulston
# March 19th 2026
# This is a script to show the ecoregion areas in the supplement where
# area calculations and gap analysis is masked to

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(terra)
library(geodata)

# Basemaps ----------------------------------------------------------------
na.ext <- terra::ext(-170, -50, 20, 75)


us_map_0 <- gadm(country = 'USA', level = 0, resolution = 2, path = "./maps/base_maps") #USA w.o States
ca_map_0 <- gadm(country = 'CA', level = 0, resolution = 2, path = './maps/base_maps') #Canada w.o Provinces
mex_map_0 <-gadm(country = 'MX', level = 0, resolution = 2, path = './maps/base_maps') # Mexico w.o States
gl_map_0  <-gadm(country = 'GL', level = 0, resolution = 2, path = './maps/base_maps') # Mexico w.o States
caribbean_codes <- c("BS", "CU", "JM", "HT", "DO", "PR", "BM", "TC", "KY") # Caribbean codes
gadm_list <- lapply(caribbean_codes, function(code) {
  tryCatch(
    gadm(country = code, level = 0, path = './maps/base_maps'),  # Save to specified path
    error = function(e) NULL
  )
})
gadm_list <- gadm_list[!sapply(gadm_list, is.null)]
# Combine all downloaded boundaries into a single spatial object
car_map_0 <- do.call(rbind, gadm_list) # Spatvertor of Caribbean Islands

ca_us_mx_map_0 <- rbind(ca_map_0, us_map_0, mex_map_0, gl_map_0, car_map_0)


# Shape file for Canada/US/Mexico borders
can_us_mex_border <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/can_us_mex_border')

# Shape files downloaded from the USGS (https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd)
great_lakes <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/great lakes/combined great lakes/')


# Habitat predictions -----------------------------------------------------
# SSP585 2070
cor_pred_ssp585_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_70_subs.Rdata')
fus_pred_ssp585_70 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_70_subs.Rdata')
ion_pred_ssp585_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_70_subs.Rdata')
ang_pred_ssp585_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_70_subs.Rdata')
chl_pred_ssp585_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_70_subs.Rdata')

# Extend
cor_pred_ssp585_70_extend <- terra::extend(cor_pred_ssp585_70, na.ext)
fus_pred_ssp585_70_extend <- terra::extend(fus_pred_ssp585_70, na.ext)
ion_pred_ssp585_70_extend <- terra::extend(ion_pred_ssp585_70, na.ext)
ang_pred_ssp585_70_extend <- terra::extend(ang_pred_ssp585_70, na.ext)
chl_pred_ssp585_70_extend <- terra::extend(chl_pred_ssp585_70, na.ext)

# High suit threshold
corPred_threshold_50 <- readRDS(file = './sdm_output/cor/subs/threshold/corPred_threshold_50_subs.Rdata')
fusPred_threshold_50 <- readRDS(file = './sdm_output/fus/subs/threshold/fusPred_threshold_50_subs.Rdata')
ionPred_threshold_50 <- readRDS(file = './sdm_output/ion/subs/threshold/ionPred_threshold_50_subs.Rdata')
angPred_threshold_50 <- readRDS(file = './sdm_output/ang/subs/threshold/angPred_threshold_50_subs.Rdata')
chlPred_threshold_50 <- readRDS(file = './sdm_output/chl/subs/threshold/chlPred_threshold_50_subs.Rdata')

# Load and crop ecoregions ------------------------------------------------
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

# Plot --------------------------------------------------------------------
#################
#### M fusca ####
#################

jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/eco_regions/malus_fusca_ecoregions.png",
  width = 2000, height = 2000, res = 300
)


terra::plot(
  ca_us_mx_map_0,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(ecoNA_fus_old,
            col = '#FFC20A',
            add = T,
            border = NA
)

terra::plot(ecoNA_fus_new,
            col = '#0C7BDC',
            add = T,
            border = NA
)

terra::plot(fus_pred_ssp585_70_extend > fusPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)


legend(
  x = -170,
  y = 35,
  title = c(expression(underline('Masking ecoregion area'))),
  legend = c("Historical ecoregions",
             "Added expansion ecoregions",
             "SSP585 2070 high suitability"),
  fill = c("#FFC20A", "#0C7BDC", alpha('black', 0.3)),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 0.75,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

dev.off()

#####################
#### M coronaria ####
#####################

jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/eco_regions/malus_coronaria_ecoregions.png",
  width = 2000, height = 2000, res = 300
)


terra::plot(
  ca_us_mx_map_0,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(ecoNA_cor_old,
            col = '#FFC20A',
            add = T,
            border = NA
)

terra::plot(ecoNA_cor_new,
            col = '#0C7BDC',
            add = T,
            border = NA
)

terra::plot(cor_pred_ssp585_70_extend > corPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

legend(
  x = -170,
  y = 35,
  title = c(expression(underline('Masking ecoregion area'))),
  legend = c("Historical ecoregions",
             "Added expansion ecoregions",
             "SSP585 2070 high suitability"),
  fill = c("#FFC20A", "#0C7BDC", alpha('black', 0.3)),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 0.75,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

dev.off()

####################
#### M ioensis ####
###################

jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/eco_regions/malus_ioensis_ecoregions.png",
  width = 2000, height = 2000, res = 300
)


terra::plot(
  ca_us_mx_map_0,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(ecoNA_ion_old,
            col = '#FFC20A',
            add = T,
            border = NA
)

terra::plot(ecoNA_ion_new,
            col = '#0C7BDC',
            add = T,
            border = NA
)

terra::plot(ion_pred_ssp585_70_extend > ionPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

legend(
  x = -170,
  y = 35,
  title = c(expression(underline('Masking ecoregion area'))),
  legend = c("Historical ecoregions",
             "Added expansion ecoregions",
             "SSP585 2070 high suitability"),
  fill = c("#FFC20A", "#0C7BDC", alpha('black', 0.3)),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 0.75,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

dev.off()

########################
#### M angustifolia ####
#######################

jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/eco_regions/malus_angustifolia_ecoregions.png",
  width = 2000, height = 2000, res = 300
)


terra::plot(
  ca_us_mx_map_0,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(ecoNA_ang_old,
            col = '#FFC20A',
            add = T,
            border = NA
)

terra::plot(ecoNA_ang_new,
            col = '#0C7BDC',
            add = T,
            border = NA
)

terra::plot(ang_pred_ssp585_70_extend > angPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

legend(
  x = -170,
  y = 35,
  title = c(expression(underline('Masking ecoregion area'))),
  legend = c("Historical ecoregions",
             "Added expansion ecoregions",
             "SSP585 2070 high suitability"),
  fill = c("#FFC20A", "#0C7BDC", alpha('black', 0.3)),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 0.75,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

dev.off()

#####################
#### Chloromeles ####
#####################

jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/eco_regions/chloromeles_ecoregions.png",
  width = 2000, height = 2000, res = 300
)


terra::plot(
  ca_us_mx_map_0,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(ecoNA_chl_old,
            col = '#FFC20A',
            add = T,
            border = NA
)

terra::plot(ecoNA_chl_new,
            col = '#0C7BDC',
            add = T,
            border = NA
)

terra::plot(chl_pred_ssp585_70_extend > chlPred_threshold_50,
            col = c("#FFFFFF00", 'black'),
            alpha = 0.3,
            add = T,
            border = NA,
            legend = F
)

legend(
  x = -170,
  y = 35,
  title = c(expression(underline('Masking ecoregion area'))),
  legend = c("Historical ecoregions",
             "Added expansion ecoregions",
             "SSP585 2070 high suitability"),
  fill = c("#FFC20A", "#0C7BDC", alpha('black', 0.3)),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 0.75,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

dev.off()
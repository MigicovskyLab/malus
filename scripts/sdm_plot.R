# Top ---------------------------------------------------------------------
# SDM plotting and map making
# Terrell Roulston 
# Started May 7th, 2024

library(tidyverse) # Grammar and data management
library(terra) # Spatial Data package


# Load occurrences and  raster/vectors  -----------------------------------
# Occurrence Points in SpatVectors

occThin_cor <- readRDS(file = './occ_data/cor/occThin_cor.Rdata') # M. coronaria
occThin_fus <- readRDS(file = './occ_data/fus/occThin_fus.Rdata') # M. fusca
occThin_ion <- readRDS(file = './occ_data/ion/occThin_ion.Rdata') # M. ioensis
occThin_ang <- readRDS(file = './occ_data/ang/occThin_ang.Rdata') # M. angustifolia
occThin_chl <- readRDS(file = './occ_data/chl/occThin_chl.Rdata') # Chloromeles

# Great Lakes shapefiles for making pretty maps
# Shape files downloaded from the USGS (https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd)
great_lakes <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/great lakes/combined great lakes/')

# Canada/US Border for showing the international line. Gadm admin boundaries trace the entire country, vs this is just the border
# Much easier to see the SDM results along coastlines where tracing obscures the data
# Downloaded from  https://koordinates.com/layer/111012-canada-and-us-border/
can_us_border <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/can_us border')

# Two line segments are in the water and are not needed in this case, lets remove them to make the maps look prettier
segments_to_remove <- c("Gulf of Maine", "Straits of Georgia and Juan de Fuca")
can_us_border <- can_us_border[!can_us_border$SectionEng %in% segments_to_remove, ]

can_us_mex_border <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/can_us_mex_border')

# Predicted habitat suitability rasters
# M. coronaria
cor_pred_hist <- readRDS(file = './sdm_output/cor/subs/cor_pred_hist_subs.Rdata')

cor_pred_ssp245_30 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_30_subs.Rdata')
cor_pred_ssp245_50 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_50_subs.Rdata')
cor_pred_ssp245_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp245_70_subs.Rdata')

cor_pred_ssp585_30 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_30_subs.Rdata')
cor_pred_ssp585_50 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_50_subs.Rdata')
cor_pred_ssp585_70 <- readRDS(file = './sdm_output/cor/subs/cor_pred_ssp585_70_subs.Rdata')

# M. fusca
fus_pred_hist <- readRDS(file = './sdm_output/fus/subs/fus_pred_hist_subs.Rdata')

fus_pred_ssp245_30 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp245_30_subs.Rdata')
fus_pred_ssp245_50 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp245_50_subs.Rdata')
fus_pred_ssp245_70 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp245_70_subs.Rdata')

fus_pred_ssp585_30 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_30_subs.Rdata')
fus_pred_ssp585_50 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_50_subs.Rdata')
fus_pred_ssp585_70 <- readRDS(file = './sdm_output/fus/subs/fus_pred_ssp585_70_subs.Rdata')

# M. ioensis
ion_pred_hist <- readRDS(file = './sdm_output/ion/subs/ion_pred_hist_subs.Rdata')

ion_pred_ssp245_30 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_30_subs.Rdata')
ion_pred_ssp245_50 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_50_subs.Rdata')
ion_pred_ssp245_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp245_70_subs.Rdata')

ion_pred_ssp585_30 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_30_subs.Rdata')
ion_pred_ssp585_50 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_50_subs.Rdata')
ion_pred_ssp585_70 <- readRDS(file = './sdm_output/ion/subs/ion_pred_ssp585_70_subs.Rdata')

# M. angustifolia
ang_pred_hist <- readRDS(file = './sdm_output/ang/subs/ang_pred_hist_subs.Rdata')

ang_pred_ssp245_30 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_30_subs.Rdata')
ang_pred_ssp245_50 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_50_subs.Rdata')
ang_pred_ssp245_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp245_70_subs.Rdata')

ang_pred_ssp585_30 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_30_subs.Rdata')
ang_pred_ssp585_50 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_50_subs.Rdata')
ang_pred_ssp585_70 <- readRDS(file = './sdm_output/ang/subs/ang_pred_ssp585_70_subs.Rdata')

# Chloromeles
chl_pred_hist <- readRDS(file = './sdm_output/chl/subs/chl_pred_hist_subs.Rdata')

chl_pred_ssp245_30 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_30_subs.Rdata')
chl_pred_ssp245_50 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_50_subs.Rdata')
chl_pred_ssp245_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp245_70_subs.Rdata')

chl_pred_ssp585_30 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_30_subs.Rdata')
chl_pred_ssp585_50 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_50_subs.Rdata')
chl_pred_ssp585_70 <- readRDS(file = './sdm_output/chl/subs/chl_pred_ssp585_70_subs.Rdata')

# Thresholds
# M. coronaria
corPred_threshold_1 <- readRDS(file = './sdm_output/cor/subs/threshold/corPred_threshold_1_subs.Rdata')
corPred_threshold_10 <- readRDS(file = './sdm_output/cor/subs/threshold/corPred_threshold_10_subs.Rdata')
corPred_threshold_50 <- readRDS(file = './sdm_output/cor/subs/threshold/corPred_threshold_50_subs.Rdata')

#M. fusca
fusPred_threshold_1 <- readRDS(file = './sdm_output/fus/subs/threshold/fusPred_threshold_1_subs.Rdata')
fusPred_threshold_10 <- readRDS(file = './sdm_output/fus/subs/threshold/fusPred_threshold_10_subs.Rdata')
fusPred_threshold_50 <- readRDS(file = './sdm_output/fus/subs/threshold/fusPred_threshold_50_subs.Rdata')

#M. ioensis
ionPred_threshold_1 <- readRDS(file = './sdm_output/ion/subs/threshold/ionPred_threshold_1_subs.Rdata')
ionPred_threshold_10 <- readRDS(file = './sdm_output/ion/subs/threshold/ionPred_threshold_10_subs.Rdata')
ionPred_threshold_50 <- readRDS(file = './sdm_output/ion/subs/threshold/ionPred_threshold_50_subs.Rdata')

#M. angustifolia
angPred_threshold_1 <- readRDS(file = './sdm_output/ang/subs/threshold/angPred_threshold_1_subs.Rdata')
angPred_threshold_10 <- readRDS(file = './sdm_output/ang/subs/threshold/angPred_threshold_10_subs.Rdata')
angPred_threshold_50 <- readRDS(file = './sdm_output/ang/subs/threshold/angPred_threshold_50_subs.Rdata')

#Chloromeles
chlPred_threshold_1 <- readRDS(file = './sdm_output/chl/subs/threshold/chlPred_threshold_1_subs.Rdata')
chlPred_threshold_10 <- readRDS(file = './sdm_output/chl/subs/threshold/chlPred_threshold_10_subs.Rdata')
chlPred_threshold_50 <- readRDS(file = './sdm_output/chl/subs/threshold/chlPred_threshold_50_subs.Rdata')


# Project to Mollweide projection --------------------------------------
# NOTE: changed the projection to Mollweide eqaul-area projection
projMoll <- "+proj=moll +lon_0=-95 +datum=WGS84 +units=m +no_defs"

#projLam <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Great lakes
great_lakes.moll <- project(great_lakes, projMoll)

# Can/US border
can_us_border.moll <- project(can_us_border, projMoll)
can_us_mex_border.moll <- project(can_us_mex_border, projMoll)

# Occurrences
occThin_cor.moll <- project(occThin_cor, projMoll)
occThin_fus.moll <- project(occThin_fus, projMoll)
occThin_ion.moll <- project(occThin_ion, projMoll)
occThin_ang.moll <- project(occThin_ang, projMoll)
occThin_chl.moll <- project(occThin_chl, projMoll)

# Habitat suitability
# M. coronaria
cor_pred_hist.moll <- project(cor_pred_hist, projMoll)

cor_pred_ssp245_30.moll <- project(cor_pred_ssp245_30, projMoll)
cor_pred_ssp245_50.moll <- project(cor_pred_ssp245_50, projMoll)
cor_pred_ssp245_70.moll <- project(cor_pred_ssp245_70, projMoll)

cor_pred_ssp585_30.moll <- project(cor_pred_ssp585_30, projMoll)
cor_pred_ssp585_50.moll <- project(cor_pred_ssp585_50, projMoll)
cor_pred_ssp585_70.moll <- project(cor_pred_ssp585_70, projMoll)

# M. fusca
fus_pred_hist.moll <- project(fus_pred_hist, projMoll)

fus_pred_ssp245_30.moll <- project(fus_pred_ssp245_30, projMoll)
fus_pred_ssp245_50.moll <- project(fus_pred_ssp245_50, projMoll)
fus_pred_ssp245_70.moll <- project(fus_pred_ssp245_70, projMoll)

fus_pred_ssp585_30.moll <- project(fus_pred_ssp585_30, projMoll)
fus_pred_ssp585_50.moll <- project(fus_pred_ssp585_50, projMoll)
fus_pred_ssp585_70.moll <- project(fus_pred_ssp585_70, projMoll)

# M. ionesis
ion_pred_hist.moll <- project(ion_pred_hist, projMoll)

ion_pred_ssp245_30.moll <- project(ion_pred_ssp245_30, projMoll)
ion_pred_ssp245_50.moll <- project(ion_pred_ssp245_50, projMoll)
ion_pred_ssp245_70.moll <- project(ion_pred_ssp245_70, projMoll)

ion_pred_ssp585_30.moll <- project(ion_pred_ssp585_30, projMoll)
ion_pred_ssp585_50.moll <- project(ion_pred_ssp585_50, projMoll)
ion_pred_ssp585_70.moll <- project(ion_pred_ssp585_70, projMoll)

# M. angustifolia
ang_pred_hist.moll <- project(ang_pred_hist, projMoll)

ang_pred_ssp245_30.moll <- project(ang_pred_ssp245_30, projMoll)
ang_pred_ssp245_50.moll <- project(ang_pred_ssp245_50, projMoll)
ang_pred_ssp245_70.moll <- project(ang_pred_ssp245_70, projMoll)

ang_pred_ssp585_30.moll <- project(ang_pred_ssp585_30, projMoll)
ang_pred_ssp585_50.moll <- project(ang_pred_ssp585_50, projMoll)
ang_pred_ssp585_70.moll <- project(ang_pred_ssp585_70, projMoll)

# Chloromeles
chl_pred_hist.moll <- project(chl_pred_hist, projMoll)

chl_pred_ssp245_30.moll <- project(chl_pred_ssp245_30, projMoll)
chl_pred_ssp245_50.moll <- project(chl_pred_ssp245_50, projMoll)
chl_pred_ssp245_70.moll <- project(chl_pred_ssp245_70, projMoll)

chl_pred_ssp585_30.moll <- project(chl_pred_ssp585_30, projMoll)
chl_pred_ssp585_50.moll <- project(chl_pred_ssp585_50, projMoll)
chl_pred_ssp585_70.moll <- project(chl_pred_ssp585_70, projMoll)


# Clean up non-projected objects ------------------------------------------
rm(
  great_lakes,
  can_us_border,
  can_us_mex_border,
  
  occThin_cor,
  occThin_fus,
  occThin_ion,
  occThin_ang,
  occThin_chl,
  
  cor_pred_hist,
  cor_pred_ssp245_30, cor_pred_ssp245_50, cor_pred_ssp245_70,
  cor_pred_ssp585_30, cor_pred_ssp585_50, cor_pred_ssp585_70,
  
  fus_pred_hist,
  fus_pred_ssp245_30, fus_pred_ssp245_50, fus_pred_ssp245_70,
  fus_pred_ssp585_30, fus_pred_ssp585_50, fus_pred_ssp585_70,
  
  ion_pred_hist,
  ion_pred_ssp245_30, ion_pred_ssp245_50, ion_pred_ssp245_70,
  ion_pred_ssp585_30, ion_pred_ssp585_50, ion_pred_ssp585_70,
  
  ang_pred_hist,
  ang_pred_ssp245_30, ang_pred_ssp245_50, ang_pred_ssp245_70,
  ang_pred_ssp585_30, ang_pred_ssp585_50, ang_pred_ssp585_70,
  
  chl_pred_hist,
  chl_pred_ssp245_30, chl_pred_ssp245_50, chl_pred_ssp245_70,
  chl_pred_ssp585_30, chl_pred_ssp585_50, chl_pred_ssp585_70
)

gc()


# Helper functions for plotting scale bar and graticules ------------------
projMoll <- "+proj=moll +lon_0=-100 +datum=WGS84 +units=m +no_defs"

# Helper to convert lat/lon extent to projected extent
make_zoom_ext <- function(xmin, xmax, ymin, ymax, crs_out) {
  e_ll <- terra::ext(xmin, xmax, ymin, ymax)
  p_ll <- terra::as.polygons(e_ll, crs = "EPSG:4326")
  terra::ext(terra::project(p_ll, crs_out))
}

# Helper for the graticule
make_grat <- function(xmin, xmax, ymin, ymax, by_lon = 10, by_lat = 10, crs_map) {
  terra::graticule(
    lon = seq(xmin, xmax, by = by_lon),
    lat = seq(ymin, ymax, by = by_lat),
    crs = crs_map
  )
}

# Helper for equatorial scale bar
add_scalebar_eq <- function(ext_obj, bar_km = 500, x_frac = 0.08, y_frac = 0.05,
                            n_seg = 5, label_cex = 0.8) {
  e <- ext_obj
  bar_m <- bar_km * 1000
  seg_m <- bar_m / n_seg
  h <- 0.012 * (e[4] - e[3])
  
  x0 <- e[1] + x_frac * (e[2] - e[1])
  y0 <- e[3] + y_frac * (e[4] - e[3])
  
  for (i in 0:(n_seg - 1)) {
    rect(
      xleft = x0 + i * seg_m,
      ybottom = y0,
      xright = x0 + (i + 1) * seg_m,
      ytop = y0 + h,
      col = ifelse(i %% 2 == 0, "black", "white"),
      border = "black",
      xpd = NA
    )
  }
  
  text(x0, y0 - 0.8 * h, "0", adj = c(0.5, 1), cex = label_cex, xpd = NA)
  text(x0 + bar_m, y0 - 0.8 * h, as.character(bar_km), adj = c(0.5, 1), cex = label_cex, xpd = NA)
  text(x0 + bar_m / 2, y0 + 1.7 * h, "km", cex = label_cex, xpd = NA)
}

# Helper to add lon/lat labels on the axes
add_lonlat_labels_box <- function(ext_ll, ext_map, proj_crs,
                                  lon_ticks = seq(-100, -50, by = 10),
                                  lat_ticks = seq(20, 70, by = 10),
                                  cex = 0.9,
                                  x_off = 0.014,
                                  y_off = 0.022) {
  
  dx <- ext_map[2] - ext_map[1]
  dy <- ext_map[4] - ext_map[3]
  
  # longitude labels
  lon_pts <- terra::vect(
    data.frame(
      x = lon_ticks,
      y = rep(ext_ll[3], length(lon_ticks))
    ),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  lon_proj <- terra::project(lon_pts, proj_crs)
  lon_xy <- terra::crds(lon_proj)
  
  text(
    x = lon_xy[, 1],
    y = rep(ext_map[3] - y_off * dy, length(lon_ticks)),
    labels = paste0(abs(lon_ticks), "°W"),
    cex = cex,
    adj = c(0.5, 1),
    xpd = NA
  )
  
  # latitude labels
  lat_pts <- terra::vect(
    data.frame(
      x = rep(ext_ll[1], length(lat_ticks)),
      y = lat_ticks
    ),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  lat_proj <- terra::project(lat_pts, proj_crs)
  lat_xy <- terra::crds(lat_proj)
  
  text(
    x = rep(ext_map[1] - x_off * dx, length(lat_ticks)),
    y = lat_xy[, 2],
    labels = paste0(lat_ticks, "°N"),
    cex = cex,
    adj = c(1, 0.5),
    xpd = NA
  )
}

# Habitat suitability legend ----------------------------------------------
legend_labs <- rev(c('Low', 'Moderate', 'High'))
fill_cols <- rev(c("#FFF7BC", "#FEC44F", "#D95F0E"))

jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/vertBIG_suitability_legend.jpeg", width = 9999, height = 6666, res = 300)
# Plot a legend that can be saved on its own
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
#legend('center', xpd = NA, title = c(as.expression(bquote(bold('Habitat Suitability')))), legend = legend_labs, fill = fill_cols, cex = 3)
legend('center', xpd = NA, box.lwd = 2, legend = legend_labs, fill = fill_cols, cex = 9, horiz = F, bty = "o", title = 'Habitat Suitability')
dev.off()

# M. coronaria future habitat plot ----------------------------------------
dir.create("/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/", recursive = TRUE)

# Plot historical distribtion
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_historical.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_hist.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_hist.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_hist.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_hist.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()

# SSP245
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_ssp245_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_ssp245_30.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_ssp245_30.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_ssp245_30.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_ssp245_30.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_ssp245_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_ssp245_50.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_ssp245_50.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_ssp245_50.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_ssp245_50.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_ssp245_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_ssp245_70.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_ssp245_70.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_ssp245_70.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_ssp245_70.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# SSP585
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_ssp585_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_ssp585_30.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_ssp585_30.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_ssp585_30.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_ssp585_30.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_ssp585_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_ssp585_50.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_ssp585_50.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_ssp585_50.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_ssp585_50.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/cor/coronaria_ssp585_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
cor.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
cor.zoom.vect <- terra::as.polygons(cor.ext, crs = "EPSG:4326")
cor.zoom.moll <- terra::project(cor.zoom.vect, projMoll)
cor.zoom.ext <- terra::ext(cor.zoom.moll)

# species-specific graticule
g.cor <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  cor_pred_ssp585_70.moll > corPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = cor.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  cor_pred_ssp585_70.moll > corPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  cor_pred_ssp585_70.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.cor,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  cor_pred_ssp585_70.moll > corPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = cor.ext,
  ext_map = cor.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(cor.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()
# M. fusca future habitat plot --------------------------------------------
dir.create("/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/", recursive = TRUE)

# Plot historical distribtion
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_historical.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geo)graphic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_hist.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_hist.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_hist.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_hist.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()

# SSP245
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_ssp245_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_ssp245_30.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_ssp245_30.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_ssp245_30.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_ssp245_30.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_ssp245_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_ssp245_50.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_ssp245_50.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_ssp245_50.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_ssp245_50.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_ssp245_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_ssp245_70.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_ssp245_70.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_ssp245_70.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_ssp245_70.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()


# SSP585
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_ssp585_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_ssp585_30.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_ssp585_30.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_ssp585_30.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_ssp585_30.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_ssp585_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_ssp585_50.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_ssp585_50.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_ssp585_50.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_ssp585_50.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/fus/fusca_ssp585_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
fus.ext <- terra::ext(-172, -125, 35, 70)

# projected extent for plotting
fus.zoom.vect <- terra::as.polygons(fus.ext, crs = "EPSG:4326")
fus.zoom.moll <- terra::project(fus.zoom.vect, projMoll)
fus.zoom.ext <- terra::ext(fus.zoom.moll)

# species-specific graticule
g.fus <- terra::graticule(
  lon = seq(-220, -100, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  fus_pred_ssp585_70.moll > fusPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = fus.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  fus_pred_ssp585_70.moll > fusPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  fus_pred_ssp585_70.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.fus,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  fus_pred_ssp585_70.moll > fusPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = fus.ext,
  ext_map = fus.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-170, -120, by = 10),
  lat_ticks = seq(40, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(fus.zoom.ext, bar_km = 500, y_frac = 0.05, x_frac = 0.02)

dev.off()

# M. ionesis future habitat plot ------------------------------------------
dir.create("/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/occ/", recursive = TRUE)

# Plot historical distribtion
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_historical.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_hist.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_hist.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_hist.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_hist.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()

# SSP245
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_ssp245_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_ssp245_30.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_ssp245_30.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_ssp245_30.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_ssp245_30.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_ssp245_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_ssp245_50.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_ssp245_50.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_ssp245_50.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_ssp245_50.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_ssp245_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_ssp245_70.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_ssp245_70.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_ssp245_70.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_ssp245_70.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# SSP585
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_ssp585_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_ssp585_30.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_ssp585_30.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_ssp585_30.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_ssp585_30.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_ssp585_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_ssp585_50.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_ssp585_50.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_ssp585_50.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_ssp585_50.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_ssp585_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_ssp585_70.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_ssp585_70.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_ssp585_70.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_ssp585_70.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()
# M. angustifolia future habitat plot -------------------------------------
dir.create("/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/", recursive = TRUE)

# Plot historical distribtion
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_historical.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_hist.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_hist.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_hist.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_hist.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()# Plot historical distribtion
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ion/ioensis_historical.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ion.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ion.zoom.vect <- terra::as.polygons(ion.ext, crs = "EPSG:4326")
ion.zoom.moll <- terra::project(ion.zoom.vect, projMoll)
ion.zoom.ext <- terra::ext(ion.zoom.moll)

# species-specific graticule
g.ion <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ion_pred_hist.moll > ionPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ion.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ion_pred_hist.moll > ionPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ion_pred_hist.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ion,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ion_pred_hist.moll > ionPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ion.ext,
  ext_map = ion.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ion.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()
# SSP245
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_ssp245_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_ssp245_30.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_ssp245_30.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_ssp245_30.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_ssp245_30.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_ssp245_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_ssp245_50.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_ssp245_50.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_ssp245_50.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_ssp245_50.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_ssp245_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_ssp245_70.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_ssp245_70.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_ssp245_70.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_ssp245_70.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# SSP585
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_ssp585_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_ssp585_30.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_ssp585_30.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_ssp585_30.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_ssp585_30.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_ssp585_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_ssp585_50.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_ssp585_50.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_ssp585_50.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_ssp585_50.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/ang/angustifolia_ssp585_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
ang.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
ang.zoom.vect <- terra::as.polygons(ang.ext, crs = "EPSG:4326")
ang.zoom.moll <- terra::project(ang.zoom.vect, projMoll)
ang.zoom.ext <- terra::ext(ang.zoom.moll)

# species-specific graticule
g.ang <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  ang_pred_ssp585_70.moll > angPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = ang.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  ang_pred_ssp585_70.moll > angPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  ang_pred_ssp585_70.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.ang,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  ang_pred_ssp585_70.moll > angPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = ang.ext,
  ext_map = ang.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(ang.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()

# Chloromeles future habitat plot -----------------------------------------
dir.create("/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/", recursive = TRUE)

# Plot historical distribtion
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_historical.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_hist.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_hist.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_hist.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_hist.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()

# SSP245
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_ssp245_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_ssp245_30.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_ssp245_30.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_ssp245_30.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_ssp245_30.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_ssp245_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_ssp245_50.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_ssp245_50.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_ssp245_50.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_ssp245_50.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_ssp245_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_ssp245_70.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_ssp245_70.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_ssp245_70.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_ssp245_70.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# SSP585
# 2030
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_ssp585_2030.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_ssp585_30.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_ssp585_30.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_ssp585_30.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_ssp585_30.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2050
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_ssp585_2050.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_ssp585_50.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_ssp585_50.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_ssp585_50.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_ssp585_50.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()


# 2070
jpeg(filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/chl/chloromeles_ssp585_2070.jpeg", width = 3333, height = 6666, res = 300)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
chl.ext <- terra::ext(-113, -59, 25, 65)

# projected extent for plotting
chl.zoom.vect <- terra::as.polygons(chl.ext, crs = "EPSG:4326")
chl.zoom.moll <- terra::project(chl.zoom.vect, projMoll)
chl.zoom.ext <- terra::ext(chl.zoom.moll)

# species-specific graticule
g.chl <- terra::graticule(
  lon = seq(-130, -20, by = 10),
  lat = seq(20, 70, by = 10),
  crs = projMoll
)

terra::plot(
  chl_pred_ssp585_70.moll > chlPred_threshold_1,
  col = c('#E8E8E8', '#FFF7BC'),
  background = 'lightskyblue1',
  legend = FALSE,
  ext = chl.zoom.ext,
  main = '',
  axes = FALSE,
  box = FALSE
)

terra::plot(
  chl_pred_ssp585_70.moll > chlPred_threshold_10,
  col = c("#FFFFFF00", '#FEC44F'),
  add = TRUE,
  legend = FALSE
)

terra::plot(
  chl_pred_ssp585_70.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#D95F0E'),
  add = TRUE,
  legend = FALSE
)

# borders
terra::plot(can_us_mex_border.moll, add = TRUE)

# graticules
plot(
  g.chl,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  lab.loc = c("bottom","left"),
  lab.cex = 0.9,
  tick = FALSE
)

# blank plot for adding map box overtop everything
terra::plot(
  chl_pred_ssp585_70.moll > chlPred_threshold_50,
  col = c("#FFFFFF00", '#FFFFFF00'),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat axis labels on frame
add_lonlat_labels_box(
  ext_ll = chl.ext,
  ext_map = chl.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-110, -60, by = 10),
  lat_ticks = seq(30, 60, by = 10),
  cex = 0.9,
  x_off = 0.006,
  y_off = 0.014
)

# equatorial scale bar
add_scalebar_eq(chl.zoom.ext, bar_km = 500, y_frac = 0.0425, x_frac = 0.87)

dev.off()

# Supplemental SDM Figure.  -----------------------------------------------
# # Function for automating the plotting 
# plot_sdm_greyscale <- function(r, threshold1, threshold10, threshold50, xlim, ylim, out_path) {
#   jpeg(filename = out_path, width = 1600, height = 1200, res = 300)
#   terra::plot(r > threshold1, col = c("#F2F2F2", "#CCCCCC"), 
#               legend = FALSE, background = "white",
#               xlim = xlim, ylim = ylim, main = "", axes = FALSE,
#               box = FALSE, mar = c(1, 1, 1, 1))
#   terra::plot(r > threshold10, col = c("#FFFFFF00", "#999999"), add = TRUE, legend = FALSE)
#   terra::plot(r > threshold50, col = c("#FFFFFF00", "#333333"), add = TRUE, legend = FALSE)
#   terra::plot(can_us_mex_border.moll, add = TRUE, col = "black", lwd = 0.6)
#   dev.off()
# }
# 
# # Plot limits
# cor.xlim <- c(-0.9*10^6, 3.1*10^6)  
# cor.ylim <- c(-2.7*10^6, 2.5*10^6) 
# 
# fus.xlim <- c(-4*10^6, -1*10^6)
# fus.ylim <- c(-2*10^6, 3.2*10^6)
# 
# ion.xlim <- c(-0.9*10^6, 3.1*10^6)  
# ion.ylim <- c(-2.7*10^6, 2.5*10^6) 
# 
# ang.xlim <- c(-0.9*10^6, 3.1*10^6) 
# ang.ylim <- c(-2.7*10^6, 2.5*10^6) 
# 
# chl.xlim <- c(-0.9*10^6, 3.1*10^6)  
# chl.ylim <- c(-2.7*10^6, 2.5*10^6) 
# 
# 
# 
# 
# # Setup a meta list for all the SDM layers
# species_info <- list(
#   cor = list(name = "coronaria", rasters = list(
#     hist = cor_pred_hist.moll,
#     ssp245_2030 = cor_pred_ssp245_30.moll,
#     ssp245_2050 = cor_pred_ssp245_50.moll,
#     ssp245_2070 = cor_pred_ssp245_70.moll,
#     ssp585_2030 = cor_pred_ssp585_30.moll,
#     ssp585_2050 = cor_pred_ssp585_50.moll,
#     ssp585_2070 = cor_pred_ssp585_70.moll
#   ), thresholds = list(
#     t1 = corPred_threshold_1,
#     t10 = corPred_threshold_10,
#     t50 = corPred_threshold_50
#   ), xlim = cor.xlim, ylim = cor.ylim),
#   
#   fus = list(name = "fusca", rasters = list(
#     hist = fus_pred_hist.moll,
#     ssp245_2030 = fus_pred_ssp245_30.moll,
#     ssp245_2050 = fus_pred_ssp245_50.moll,
#     ssp245_2070 = fus_pred_ssp245_70.moll,
#     ssp585_2030 = fus_pred_ssp585_30.moll,
#     ssp585_2050 = fus_pred_ssp585_50.moll,
#     ssp585_2070 = fus_pred_ssp585_70.moll
#   ), thresholds = list(
#     t1 = fusPred_threshold_1,
#     t10 = fusPred_threshold_10,
#     t50 = fusPred_threshold_50
#   ), xlim = fus.xlim, ylim = fus.ylim),
#   
#   ion = list(name = "ioensis", rasters = list(
#     hist = ion_pred_hist.moll,
#     ssp245_2030 = ion_pred_ssp245_30.moll,
#     ssp245_2050 = ion_pred_ssp245_50.moll,
#     ssp245_2070 = ion_pred_ssp245_70.moll,
#     ssp585_2030 = ion_pred_ssp585_30.moll,
#     ssp585_2050 = ion_pred_ssp585_50.moll,
#     ssp585_2070 = ion_pred_ssp585_70.moll
#   ), thresholds = list(
#     t1 = ionPred_threshold_1,
#     t10 = ionPred_threshold_10,
#     t50 = ionPred_threshold_50
#   ), xlim = ion.xlim, ylim = ion.ylim),
#   
#   ang = list(name = "angustifolia", rasters = list(
#     hist = ang_pred_hist.moll,
#     ssp245_2030 = ang_pred_ssp245_30.moll,
#     ssp245_2050 = ang_pred_ssp245_50.moll,
#     ssp245_2070 = ang_pred_ssp245_70.moll,
#     ssp585_2030 = ang_pred_ssp585_30.moll,
#     ssp585_2050 = ang_pred_ssp585_50.moll,
#     ssp585_2070 = ang_pred_ssp585_70.moll
#   ), thresholds = list(
#     t1 = angPred_threshold_1,
#     t10 = angPred_threshold_10,
#     t50 = angPred_threshold_50
#   ), xlim = ang.xlim, ylim = ang.ylim),
#   
#   chl = list(name = "chloromeles", rasters = list(
#     hist = chl_pred_hist.moll,
#     ssp245_2030 = chl_pred_ssp245_30.moll,
#     ssp245_2050 = chl_pred_ssp245_50.moll,
#     ssp245_2070 = chl_pred_ssp245_70.moll,
#     ssp585_2030 = chl_pred_ssp585_30.moll,
#     ssp585_2050 = chl_pred_ssp585_50.moll,
#     ssp585_2070 = chl_pred_ssp585_70.moll
#   ), thresholds = list(
#     t1 = chlPred_threshold_1,
#     t10 = chlPred_threshold_10,
#     t50 = chlPred_threshold_50
#   ), xlim = chl.xlim, ylim = chl.ylim)
# )
# 
# # Loop through plots
# scenarios <- c("hist", "ssp245_2030", "ssp245_2050", "ssp245_2070", 
#                "ssp585_2030", "ssp585_2050", "ssp585_2070")
# 
# for (sp in names(species_info)) {
#   s <- species_info[[sp]]
#   for (scen in scenarios) {
#     out_file <- paste0("C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/supplement/", sp, "_", scen, "_grey.jpeg")
#     plot_sdm_greyscale(
#       r = s$rasters[[scen]],
#       threshold1 = s$thresholds$t1,
#       threshold10 = s$thresholds$t10,
#       threshold50 = s$thresholds$t50,
#       xlim = s$xlim,
#       ylim = s$ylim,
#       out_path = out_file
#     )
#   }
# }

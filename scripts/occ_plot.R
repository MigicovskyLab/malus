# Top ---------------------------------------------------------------------
# Plotting thinned Occurence Data
# Figure 1 in SDM Paper
# Terrell Roulston
# Started May 8 2025

library(tidyverse) # grammar and data management 
library(terra) # working with spatial data
library(geodata) # basemaps and climate data

dir.create("/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/occ_plot/", recursive = TRUE)


occThin_cor <- readRDS(file = "./occ_data/cor/occThin_cor.Rdata") # GBIF + Husband
occThin_fus <- readRDS(file = './occ_data/fus/occThin_fus.Rdata') # GBIF + Armstrong + Wickham + Obr. + Fit
occThin_ion <- readRDS(file = './occ_data/ion/occThin_ion.Rdata') # GBIF
occThin_ang <- readRDS(file = './occ_data/ang/occThin_ang.Rdata') # GBIF
occThin_chl <- readRDS(file = './occ_data/chl/occThin_chl.Rdata') 

# Plot thinned occurrence data --------------------------------------------
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

# Shape file for Canada/US/Mexico borders
can_us_mex_border <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/can_us_mex_border')

# Shape files downloaded from the USGS (https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd)
great_lakes <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/great lakes/combined great lakes/')

# seperate GBIF from other data sources
occThin_cor_gbif <- subset(occThin_cor,occThin_cor$source == 'GBIF')
occThin_cor_hus <- subset(occThin_cor,occThin_cor$source == 'Husband')

occThin_fus_gbif <- subset(occThin_fus, occThin_fus$source == 'GBIF')
occThin_fus_arm <- subset(occThin_fus, occThin_fus$source == 'Armstrong')
occThin_fus_obr_fit <- subset(occThin_fus, occThin_fus$source %in% c('Obrist', 'Fitzpatrick'))
occThin_fus_wick <- subset(occThin_fus, occThin_fus$source == 'Wickham')

#Project data
projMoll <- "+proj=moll +lon_0=-100 +datum=WGS84 +units=m +no_defs"

us_map_0.moll <- project(us_map_0, projMoll)
ca_map_0.moll <- project(ca_map_0, projMoll)
mex_map_0.moll <- project(mex_map_0, projMoll)
gl_map_0.moll <- project(gl_map_0, projMoll)
car_map_0.moll <- project(car_map_0, projMoll)

occThin_cor_gbif.moll <- project(occThin_cor_gbif, projMoll)
occThin_cor_hus.moll <- project(occThin_cor_hus, projMoll)

occThin_fus_gbif.moll <- project(occThin_fus_gbif, projMoll)
occThin_fus_arm.moll <- project(occThin_fus_arm, projMoll)
occThin_fus_obr_fit.moll <- project(occThin_fus_obr_fit, projMoll)
occThin_fus_wick.moll <- project(occThin_fus_wick, projMoll)

occThin_ion.moll <- project(occThin_ion, projMoll)
occThin_ang.moll <- project(occThin_ang, projMoll)
occThin_chl.moll <- project(occThin_chl, projMoll)

can_us_mex_border.moll <- project(can_us_mex_border, projMoll)
great_lakes.moll <- project(great_lakes, projMoll)


# Helper functions for lat/lon labels and scale bar -----------------------
add_lonlat_labels_box <- function(ext_ll, ext_map, proj_crs,
                                  lon_ticks = seq(-100, -50, by = 10),
                                  lat_ticks = seq(20, 70, by = 10),
                                  cex = 0.9,
                                  x_off = 0.006,
                                  y_off = 0.014) {
  
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

add_scalebar_eq <- function(ext_obj, bar_km = 500, x_frac = 0.8, y_frac = 0.035,
                            n_seg = 5, label_cex = 0.8) {
  e <- ext_obj
  bar_m <- bar_km * 1000
  seg_m <- bar_m / n_seg
  h <- 0.012 * (e[4] - e[3])
  
  x0 <- e[1] + x_frac * (e[2] - e[1]) - bar_m
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
  text(x0 + bar_m / 2, y0 + 1.9 * h, "km", cex = label_cex, xpd = NA)
}

# Plot thinned occurrence points ------------------------------------------

jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/occ_plot/malus_occurrence_data_sources.png",
  width = 10000, height = 6666, res = 300
)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
na.ext <- terra::ext(-145, -65, 25, 75)

# projected extent for plotting
na.zoom.vect <- terra::as.polygons(na.ext, crs = "EPSG:4326")
na.zoom.moll <- terra::project(na.zoom.vect, projMoll)
na.zoom.ext <- terra::ext(na.zoom.moll)

# graticule
g.na <- terra::graticule(
  lon = seq(-210, 0, by = 10),
  lat = seq(20, 80, by = 10),
  crs = projMoll
)

terra::plot(
  ca_map_0.moll,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.zoom.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(us_map_0.moll, col = "#E8E8E8", add = TRUE, border = "transparent")
terra::plot(mex_map_0.moll, col = "white", add = TRUE, border = "transparent")
terra::plot(gl_map_0.moll, col = "#E8E8E8", add = TRUE, border = "transparent")
terra::plot(car_map_0.moll, col = "white", add = TRUE, border = "transparent")
terra::plot(great_lakes.moll, add = TRUE, col = "lightblue", border = "grey")
terra::plot(can_us_mex_border.moll, add = TRUE, col = "grey")

terra::points(occThin_cor_gbif.moll, pch = 16, col = alpha("#882255", 1), cex = 1.3)
terra::points(occThin_fus_gbif.moll, pch = 16, col = alpha("#228B22", 1), cex = 1.3)
terra::points(occThin_cor_hus.moll, pch = 16, col = alpha("magenta", 1), cex = 1.3)
terra::points(occThin_fus_arm.moll, pch = 16, col = alpha("#333f07", 1), cex = 1.3)
terra::points(occThin_fus_wick.moll, pch = 16, col = alpha("#333f07", 1), cex = 1.3)
terra::points(occThin_fus_obr_fit.moll, pch = 16, col = alpha("#333f07", 1), cex = 1.3)
terra::points(occThin_ion.moll, pch = 16, col = alpha("#E88E00", 1), cex = 1.3)
terra::points(occThin_ang.moll, pch = 16, col = alpha("#007CBE", 1), cex = 1.3)

# graticules
plot(
  g.na,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  tick = FALSE
)

# manual lon/lat labels
add_lonlat_labels_box(
  ext_ll = na.ext,
  ext_map = na.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-140, -70, by = 10),
  lat_ticks = seq(30, 70, by = 10),
  cex = 2.2,
  x_off = 0.0222,
  y_off = 0.0227
)

# scale bar
add_scalebar_eq(
  na.zoom.ext,
  bar_km = 1000,
  x_frac = 0.16,
  y_frac = 0.035,
  label_cex = 2.5
)

legend(
  x = -4.15e6,
  y = 4.55e6,
  title = c(expression(underline('Thinned Occurrence Data'))),
  legend = c(expression(italic("Malus fusca")*"—GBIF"),
             expression(italic("Malus fusca")*"—Suppl. Data"),
             expression(italic("Malus coronaria")*"—GBIF"),
             expression(italic("Malus coronaria")*"—Suppl. Data"),
             expression(italic("Malus ioensis")*"—GBIF"),
             expression(italic("Malus angustifolia")*"—GBIF")),
  fill = c("#228B22", "#333f07", "#882255", "magenta", "#E88E00", "#007CBE"),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 2.75,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

terra::text(1e6, 6.75e6, labels = "Hudson\nBay", cex = 2.5, col = "steelblue")
terra::text(2.8e6, 4.3e6, labels = "Atlantic\nOcean", cex = 2.75, col = "steelblue")
terra::text(-3.25e6, 5.5e6, labels = "Pacific\nOcean", cex = 2.75, col = "steelblue")
terra::text(-0.25e6, 6.65e6, labels = "Canada", cex = 3, col = "black")
terra::text(-0.25e6, 4.75e6, labels = "U.S.A.", cex = 3, col = "black")
terra::text(-3e6, 7.35e6, labels = "U.S.A.", cex = 3, col = "black")
terra::text(-0.35e6, 3.2e6, labels = "Mexico", cex = 3, col = "black")
terra::text(3.1e6, 7.75e6, labels = "Greenland", cex = 3, col = "black")

dev.off()



## Need to finish adjusting the x/y labels





# Plot ONLY sect. Chloromeles
jpeg(
  filename = "C:/Users/terre/Documents/Acadia/Malus Project/sdm_plot/habitat_Moll/occ_plot/chloromeles_occurrence_data.png",
  width = 10000, height = 6666, res = 300
)

par(
  mar = c(1, 1, 1, 1),
  xaxs = "i",
  yaxs = "i"
)

# geographic extent in lon/lat
na.ext <- terra::ext(-170, -50, 20, 75)

# projected extent for plotting
na.zoom.vect <- terra::as.polygons(na.ext, crs = "EPSG:4326")
na.zoom.moll <- terra::project(na.zoom.vect, projMoll)
na.zoom.ext <- terra::ext(na.zoom.moll)

# graticule
g.na <- terra::graticule(
  lon = seq(-170, -50, by = 10),
  lat = seq(20, 80, by = 10),
  crs = projMoll
)

terra::plot(
  ca_map_0.moll,
  col = "white",
  background = "lightblue",
  border = "transparent",
  legend = FALSE,
  ext = na.zoom.ext,
  main = "",
  axes = FALSE,
  box = FALSE
)

terra::plot(us_map_0.moll, col = "#E8E8E8", add = TRUE, border = "transparent")
terra::plot(mex_map_0.moll, col = "white", add = TRUE, border = "transparent")
terra::plot(gl_map_0.moll, col = "#E8E8E8", add = TRUE, border = "transparent")
terra::plot(car_map_0.moll, col = "white", add = TRUE, border = "transparent")
terra::plot(great_lakes.moll, add = TRUE, col = "lightblue", border = "grey")
terra::plot(can_us_mex_border.moll, add = TRUE, col = "grey")

terra::points(occThin_chl.moll, pch = 16, col = alpha("magenta", 1), cex = 1.3)

# graticules
plot(
  g.na,
  add = TRUE,
  col = "grey75",
  labels = FALSE,
  tick = FALSE
)

# blank plot for map box
terra::plot(
  ca_map_0.moll,
  col = c("#FFFFFF00"),
  add = TRUE,
  legend = FALSE,
  box = TRUE
)

# manual lon/lat labels
add_lonlat_labels_box(
  ext_ll = na.ext,
  ext_map = na.zoom.ext,
  proj_crs = projMoll,
  lon_ticks = seq(-160, -50, by = 10),
  lat_ticks = seq(20, 70, by = 10),
  cex = 1.4,
  x_off = 0.006,
  y_off = 0.014
)

# scale bar
add_scalebar_eq(
  na.zoom.ext,
  bar_km = 1000,
  x_frac = 0.82,
  y_frac = 0.035,
  label_cex = 1.2
)

legend(
  x = 1.7e6,
  y = 2.35e6,
  title = c(expression(underline('Thinned Occurrence Data'))),
  legend = c(expression("Sect. Chloromeles")),
  fill = c("magenta"),
  col = "black",
  box.col = "black",
  bg = "white",
  text.col = "black",
  cex = 2,
  xjust = 0,
  yjust = 1,
  title.adj = 0.25
)

terra::text(533792.2, 1206373.8, labels = "Hudson\nBay", cex = 1.2, col = "steelblue")
terra::text(2525623.3, -617100.0, labels = "North\nAtlantic\nOcean", cex = 1.5, col = "steelblue")
terra::text(-2838692.4, 684470.5, labels = "North\nPacific\nOcean", cex = 1.5, col = "steelblue")
terra::text(-585874.4, 1236224, labels = "Canada", cex = 2, col = "black")
terra::text(-585874.4, -1032531, labels = "U.S.A.", cex = 2, col = "black")

box(which = "figure")

dev.off()
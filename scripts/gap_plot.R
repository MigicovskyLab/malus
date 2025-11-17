
library(tidyverse)
library(ggplot2)



# Load gap analysis data --------------------------------------------------
# note this df includes the no migration and full migration gap analysis results for both SSPs
migration_df <- readr::read_csv("./gap_analysis/malus_gap_analysis_combined.csv")

# Plotting Helper Function ------------------------------------------------
# This function plots the two migration scenarios, and includes options to
# 1) select which SSP to plot, I plan to plot seperately 
# 2) decide if you want to include the historical gaps as well
# Note I commented out helpful legend elements that I will add in post later
# But if you need to uncomment them the quickly improve clarity

# Gap plot by metric nested within year -----------------------------------
plot_gap_by_metric <- function(df, sp, ssp_sel = "585", include_historical = TRUE) {
  metrics     <- c("SRSin","GRSin","ERSin","FCSin")
  years       <- c(2000, 2030, 2050, 2070)
  year_labels <- c("1970–2000","2030","2050","2070")
  metric_cols <- c("#A1D99B", "#7FCDBB", "#41AE76", "#0868AC")
  
  dat <- df %>%
    dplyr::filter(sp_code == sp) %>%
    dplyr::mutate(period_label = ifelse(period == 2000, "Historical", as.character(period)))
  
  
  # layout: one row, four metric panels
  op <- par(mfrow = c(1, length(metrics)),
            oma = c(0.5, 0.5, 4, 0.5),
            xaxs = "i", yaxs = "i",
            mgp = c(2, 1.2, 0),
            cex.axis = 2.2)
  on.exit(par(op), add = TRUE)
  
  for (i in seq_along(metrics)) {
    met <- metrics[i]
    col <- metric_cols[i]
    
    # margins: give y-axis to first panel only
    left_mar  <- if (i == 1) 6 else 1.2
    right_mar <- if (i == length(metrics)) 1.2 else 0.8
    par(mar = c(8.5, left_mar, 1, right_mar))
    
    # empty panel
    plot(NA, xlim = c(0.5, 4.5), ylim = c(0, 100),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
    
    # faint year grid
    abline(v = 1:4, lty = 3, col = "grey70")
    
    # y-axis + label only once
    if (i == 1) {
      axis(2, at = seq(0, 100, 25), cex.axis = 2.0)
      mtext("In situ conservation score", side = 2, line = 3.9, cex = 2.3)
    }
    
    # x-axis (years)
    axis(1, at = 1:4, labels = year_labels, las = 1, cex.axis = 1.9, tck = 0)
    
    # Conservation thresholds
    par(xpd = TRUE)
    segments(x0 = 0.55, y0 = 0, x1 = 1, y1 = 0, lty = 2, col = "black", lwd = 1.5)
    segments(x0 = 4, y0 = 0, x1 = 4.25, y1 = 0, lty = 2, col = "black", lwd = 1.5)
    segments(x0 = 0.55, y0 = 25, x1 = 4.25, y1 = 25, lty = 2, col = "black", lwd = 1.5)
    segments(x0 = 0.55, y0 = 50, x1 = 4.25, y1 = 50, lty = 2, col = "black", lwd = 1.5)
    segments(x0 = 0.55, y0 = 75, x1 = 4.25, y1 = 75, lty = 2, col = "black", lwd = 1.5)
    segments(x0 = 0.55, y0 = 100, x1 = 4.25, y1 = 100, lty = 2, col = "black", lwd = 1.5)
    
    # Add labels only for rightmost panel
    if (i == 4) {
      text(x = 4.35, y = 0, labels = "CP", col = "black", cex = 2.1, font = 1, adj = c(0, 0.5))
      text(x = 4.35, y = 25, labels = "HP", col = "black", cex = 2.1, font = 1, adj = c(0, 0.5))
      text(x = 4.35, y = 50, labels = "MP", col = "black", cex = 2.1, font = 1, adj = c(0, 0.5))
      text(x = 4.35, y = 75, labels = "LP", col = "black", cex = 2.1, font = 1, adj = c(0, 0.5))
      text(x = 4.35, y = 100, labels = "SC", col = "black", cex = 2.1, font = 1, adj = c(0, 0.5))
      
    }
    
    # plot points/segments per year for this metric
    for (j in seq_along(years)) {
      yr <- years[j]
      rows <- dat %>% dplyr::filter(period == yr)
      
      if (include_historical && yr == 2000) {
        # Prefer historical no-migration, fall back to any historical
        rh <- rows %>%
          dplyr::filter(ssp == "historical", mode == "no_migration")
        if (nrow(rh) == 0)
          rh <- rows %>% dplyr::filter(ssp == "historical")
        
        if (nrow(rh) > 0) {
          y <- rh[[met]][1]                 # single value fixes length mismatch
          segments(j, 0, j, y, lwd = 6, col = col)
          points(j, y, pch = 15, cex = 3.5, col = col)
        }
      }
      
      # future: no_migration (open circle, left) & migration (filled circle, right)
      r_no  <- rows %>% dplyr::filter(ssp == ssp_sel, mode == "no_migration")
      r_yes <- rows %>% dplyr::filter(ssp == ssp_sel, mode == "migration")
      
      if (nrow(r_no) > 0) {
        y <- r_no[[met]][1]
        x <- j - 0.08
        segments(x, 0, x, y, lwd = 6, col = col)
        points(x, y, pch = 1, cex = 3.5, lwd = 3, col = col)
      }
      if (nrow(r_yes) > 0) {
        y <- r_yes[[met]][1]
        x <- j + 0.08
        segments(x, 0, x, y, lwd = 6, col = col)
        points(x, y, pch = 19, cex = 3.5, col = col)
      }
      
    }
    
    par(xpd = FALSE)
    
    # panel title = metric name
    mtext(met, side = 1, line = 5, cex = 2, font = 1, col = 'black')
  }
  
  # outer species title
  taxon_label <- switch(sp,
                        "fus" = expression(italic("Malus fusca")),
                        "cor" = expression(italic("Malus coronaria")),
                        "ion" = expression(italic("Malus ioensis")),
                        "ang" = expression(italic("Malus angustifolia")),
                        "chl" = expression(Sect.~italic("Chloromeles")))
  mtext(taxon_label, outer = TRUE, side = 3, line = 0.5, cex = 2.75)
}


png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/gap_by_metric_TEST2.png",
    width = 6000, height = 2000, res = 300)
plot_gap_by_metric(migration_df, sp = "cor", ssp_sel = "585", include_historical = TRUE)
dev.off()

# Plotting ----------------------------------------------------------------
# This is a function to plot both SSP245 and SSP585 
# Individually

species_to_plot <- c("fus","cor","ion","ang","chl")
for (ssp in c("585","245")) {
  out_dir <- file.path("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis", paste0("SSP", ssp))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  for (sp in species_to_plot) {
    png(
      filename = file.path(out_dir, sprintf("gap_analysis_%s.png", sp)),
      width = 6000, height = 2000, res = 300
    )
    plot_gap_by_metric(migration_df, sp = sp, ssp_sel = ssp, include_historical = TRUE)
    dev.off()
  }
}


# Legends -----------------------------------------------------------------
# Point legends
png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/migration_legend.png", width = 2.5*2.2, height = 1.5*2.2, res = 300, units = 'in')
  
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(
    "center",
    legend = c("Historical", "No migration", "Full migration"),
    pch = c(15, 1, 19),          
    pt.bg = c("black", "black", "black"), 
    col = c("black", "black", "black"),   
    pt.cex = 0.75*2.2,
    bty = "n",
    horiz = FALSE,
    cex = 0.75*2.2,
    title = "Migration scenario"
  )
  
  dev.off()

# Metric colours
png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/gap_legend.png", width = 2.5*2.2, height = 1.5*2.2, res = 300, units = 'in')

metric_cols <- c("#A1D99B", "#7FCDBB", "#41AE76", "#0868AC")
metric_names <- c("SRSin", "GRSin", "ERSin", "FCSin")

par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "center",
  legend = metric_names,
  fill = metric_cols,
  border = NA,
  ncol = 1,
  bty = "n",
  cex = 0.75*2.2,
  title = "In situ gap metric"
)

dev.off()

# Conservation thresholds
png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/threshold_legend.png", width = 2.5*2.2, height = 1.5*2.2, res = 300, units = 'in')

par(mar = c(0, 0, 0, 0))
plot.new()
legend("right",
       legend = c(
         "CP = Critical priority (FCSin = 0)",
         "HP = High priority (FCSin < 25)",
         "MP = Medium priority (25 ≤ FCSin < 50)",
         "LP = Low priority (50 ≤ FCSin < 75)",
         "SC = Sufficiently conserved (FCSin ≥ 75)"
       ),
       bty = "n",
       cex = 0.75*2.2
       # title = "Conservation priority categories"
)

dev.off()

# Arrange in grid ---------------------------------------------------------
library(png)
library(grid)
library(gridExtra)

read_grob <- function(path) rasterGrob(png::readPNG(path), interpolate = TRUE)

make_grid_for_ssp <- function(ssp_dir) {
  g_fus <- read_grob(file.path(ssp_dir, "gap_analysis_fus.png"))
  g_cor <- read_grob(file.path(ssp_dir, "gap_analysis_cor.png"))
  g_ion <- read_grob(file.path(ssp_dir, "gap_analysis_ion.png"))
  g_ang <- read_grob(file.path(ssp_dir, "gap_analysis_ang.png"))
  g_chl <- read_grob(file.path(ssp_dir, "gap_analysis_chl.png"))
  
  # legends (stacked vertically in the 6th cell)
  # g_leg_mig <- read_grob(file.path(dirname(ssp_dir), "migration_legend.png"))
  # g_leg_gap <- read_grob(file.path(dirname(ssp_dir), "gap_legend.png"))
  # g_leg_the <- read_grob(file.path(dirname(ssp_dir), "threshold_legend.png"))
  # g_leg_v   <- arrangeGrob(g_leg_mig, g_leg_gap, g_leg_the, ncol = 2, heights = c(1, 1, 1))
  
  grid <- arrangeGrob(g_fus, g_cor, g_ion,
                      g_ang, g_chl,
                      ncol = 1,
                      padding = unit(-1.2, "line"))
  grid
}

# Example: build and save the SSP585 composite
ssp585_dir <- "C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/SSP585"
g <- make_grid_for_ssp(ssp585_dir)
ggsave(file.path(ssp585_dir, "gap_analysis_grid_SSP585_v2.png"),
       g, width = 18, height = 27, dpi = 300)




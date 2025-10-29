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

plot_gap_panels_mode <- function(df, sp, ssp_sel = "585", include_historical = TRUE) {
  metrics     <- c("SRSin","GRSin","ERSin","FCSin")
  periods     <- c(2000, 2030, 2050, 2070)
  metric_cols <- c("#A1D99B", "#7FCDBB", "#41AE76", "#0868AC")
  dat <- df %>%
    dplyr::filter(sp_code == sp) %>%
    dplyr::mutate(period_label = ifelse(period == 2000, "Historical", as.character(period)))
  
  op <- par(mfrow = c(length(periods), 1),
            mar = c(0.2, 7.5, 1, 1.2),
            oma = c(3.2, 0.5, 0.8, 3),
            xaxs = "i", yaxs = "i", mgp = c(2, 0.6, 0),
            mgp = c(2, 1.5, 0),
            cex.axis = 3)
  
  on.exit(par(op), add = TRUE)
  
  for (i in seq_along(periods)) {
    yr <- periods[i]
    plot(NA, xlim = c(0, 100), ylim = c(0.5, 4.5),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
    abline(h = 1:4, lty = 3, col = "grey70")
    if (yr == tail(periods, 1)) axis(1, at = seq(0, 100, 25), cex.axis = 2.1)
    axis(2, at = 1:4, labels = metrics, las = 1, cex.axis = 2.1, tck = 0)
    # mtext(dat$period_label[dat$period == yr][1], side = 3, adj = 0, line = -1, cex = 1.0)
    
    rows <- dat %>% dplyr::filter(period == yr)
    
    # --- Historical row (only for 2000, optional)
    if (include_historical && yr == 2000 && any(rows$ssp == "historical")) {
      r <- rows %>% dplyr::filter(ssp == "historical")
      x <- as.numeric(r[1, metrics]); y <- 1:4
      par(xpd = TRUE)
      segments(0, y, x, y, lwd = 6, col = metric_cols)
      points(x, y, pch = 19, cex = 4, col = metric_cols)   # solid circles
      par(xpd = FALSE)
      next
    }
    
    # --- FUTURE rows: compare MODES (no_migration vs migration) for chosen SSP
    r_nomig <- rows %>% dplyr::filter(ssp == ssp_sel, mode == "no_migration")
    r_mig   <- rows %>% dplyr::filter(ssp == ssp_sel, mode == "migration")
    
    # no_migration, slight down offset
    if (nrow(r_nomig) > 0) {
      x <- as.numeric(r_nomig[1, metrics]); y <- 1:4 - 0.2
      par(xpd = TRUE)
      segments(0, y, x, y, lwd = 6, lty = 1, col = metric_cols)
      points(x, y, pch = 25, cex = 4, col = metric_cols, bg = metric_cols)   # downwards triangle
      par(xpd = FALSE)
    }
    # migration, slight up offset
    if (nrow(r_mig) > 0) {
      x <- as.numeric(r_mig[1, metrics]); y <- 1:4 + 0.2
      par(xpd = TRUE)
      segments(0, y, x, y, lwd = 6, lty = 1, col = metric_cols)
      points(x, y, pch = 24, cex = 4, col = metric_cols, bg = metric_cols)   # upwards triangle
      par(xpd = FALSE)
    }
  }
  
  par(xpd = NA)
  # Mode legend (shapes)
  leg_shapes <- c("Historical", "No migration", "Full migration")
  leg_pch    <- c(19, 25, 17)
  # legend("bottomright", inset = c(0.02, -0.015), horiz = TRUE, bty = "n",
  #        legend = leg_shapes, pch = leg_pch, pt.cex = 1.5, x.intersp = 0.7, pt.bg = c('black','black', 'black'), col = c('black','black', 'black'))
  
  # Metric legend (colors)
  # legend("bottomright", inset = c(0.03, 0.1), bty = "n",
  #        legend = metrics, fill = metric_cols, border = NA, ncol = 1, cex = 1.0)
  # par(xpd = FALSE)
  
  # High priority threshold line
  segments(
    x0 = 25, y0 = 0.6,    # start just above x-axis baseline
    x1 = 25, y1 = 17,    # stop just below top FCSin bar
    lty = 2, col = "black", lwd = 1.5
  )
  
  # Allow text in outer margins
  par(xpd = NA)
  
  # Add subtle label
  text(
    x = 25, y = 17.25, labels = "HP",
    col = "black", cex = 2.1, font = 2, adj = c(0.5,0.5)
  )
  
  # Reset clipping
  par(xpd = FALSE)
  
}


# Example usage
png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/gap_analysis_TEST.png", width = 5000, height = 3333, res = 300)
plot_gap_panels_mode(migration_df, sp = "cor", ssp_sel = "585", include_historical = TRUE)
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
      width = 5000, height = 3333, res = 300
    )
    plot_gap_panels_mode(migration_df, sp = sp, ssp_sel = ssp, include_historical = TRUE)
    dev.off()
  }
}


# Legends -----------------------------------------------------------------
# Point legends
png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/migration_legend.png", width = 1200*2, height = 800*2, res = 300)

par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "center",
  legend = c("Historical", "No migration", "Full migration"),
  pch = c(19, 25, 24),           # ● ▼ ▲
  pt.bg = c("black", "black", "black"), # fill for ▼
  col = c("black", "black", "black"),     # outlines
  pt.cex = 3,
  bty = "n",
  horiz = FALSE,
  cex = 3,
  title = "Migration scenario"
)

dev.off()

# Metric colours
png("C:/Users/terre/Documents/Acadia/Malus Project/statistical analysis/gap_analysis/gap_legend.png", width = 1200*2, height = 800*2, res = 300)

metric_cols <- rev(c("#A1D99B", "#7FCDBB", "#41AE76", "#0868AC"))
metric_names <- rev(c("SRSin", "GRSin", "ERSin", "FCSin"))

par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "center",
  legend = metric_names,
  fill = metric_cols,
  border = NA,
  ncol = 1,
  bty = "n",
  cex = 3,
  title = "In situ gap metrics"
)

dev.off()

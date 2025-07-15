# create_plots_manuscript.R


library(data.table)
library(ggplot2)
library(rjutils)
path <- "data/out/"

# Figure 1 and 2 : Heat and cold cumulative and lag effect ---------------------


# Select exposure.
for (exposure in c("heat", "cold")) {

# Load cumulative curve and lags at a given quantile.
cball <- readRDS(paste0(path, "cb_tvar_overall_", exposure, "_main.rds"))
cblag <- readRDS(paste0(path, "cb_lag_reduce_", exposure, "_main.rds"))

# Convert to data.table to plot in ggplot.
cball_dt <- data.table(
    x     = cball$predvar,
    y     = cball$RRfit,
    ymin  = cball$RRlow,
    ymax = cball$RRhigh
)
cblag_dt <- data.table(
    x     = seq(0, cblag$lag[2], by = 0.1),
    y     = cblag$RRfit,
    ymin  = cblag$RRlow,
    ymax = cblag$RRhigh
)

if (exposure == "heat") {
    col <- pal_rdbu$red
    x_range <- seq(0.45, 1, by = 0.001)
} else if (exposure == "cold") {
    col <- pal_rdbu$blue
    x_range <- seq(0, 0.55, by = 0.001)
}

# Combine both plot.
jarrange(list(
    
# Plot a) Cumulative overall effect
ggplot(cball_dt[x %in% x_range, ], aes(x = x * 100, y = y)) +
geom_hline(yintercept = 1, lty = 2, lwd = 0.5) + 
geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = col, alpha = 0.15, col = NA) + 
geom_line(col = col, lwd = 0.8) +
#scale_x_continuous(breaks = x_breaks) + 
labs(x = "Temperature percentile", y = "Odds ratio (OR)") + 
ggtitle("a) Overall cumulative effect") + 
jtheme(title_hjust = 0, title_size = 12L,show_grid = TRUE)
,

# Plot b) Lag effect at Q95.
ggplot(cblag_dt, aes(x = x, y = y)) +
geom_hline(yintercept = 1, lty = 2, lwd = 0.5) + 
geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = col, alpha = 0.15, col = NA) + 
geom_line(col = col, lwd = 0.8) +
#scale_x_continuous(breaks = seq(0, cblag$lag[2], by = 1)) + 
labs(x = "Lags (in days)", y = "") + 
ggtitle(paste0("b) Lag effect at ", cblag$value * 100, "ᵗʰ temperature percentile")) + 
jtheme(title_hjust = 0, title_size = 12L, show_grid = TRUE)

))

# Save plot.
save_ggplot(paste0("plots/manuscript/fig_", ifelse(exposure == "heat", 1, 2), "_", exposure, ".png"), size = c(8, 3.5))

}


# Figure S1 : Full-year cumulative and lag effect ------------------------------


# Set exposure to full.
exposure <- "full"

# Load cumulative curve and lags at a given quantile.
cball <- readRDS(paste0(path, "cb_tvar_overall_", exposure, "_main.rds"))
cblag <- readRDS(paste0(path, "cb_lag_reduce_", exposure, "_main.rds"))
cblag2 <- readRDS(paste0(path, "cb_lag_reduce_", exposure, "_05pctl.rds"))

# Convert to data.table to plot in ggplot.
cball_dt <- data.table(
    x     = cball$predvar,
    y     = cball$RRfit,
    ymin  = cball$RRlow,
    ymax = cball$RRhigh
)
cblag_dt <- data.table(
    x     = seq(0, cblag$lag[2], by = 0.1),
    y     = cblag$RRfit,
    ymin  = cblag$RRlow,
    ymax = cblag$RRhigh
)
cblag_dt2 <- data.table(
    x     = seq(0, cblag2$lag[2], by = 0.1),
    y     = cblag2$RRfit,
    ymin  = cblag2$RRlow,
    ymax = cblag2$RRhigh
)
    
col_heat <- pal_rdbu$red
col_cold <- pal_rdbu$blue
col <- "grey20"
x_range = seq(0, 1, by = 0.001)
    
# Combine the three plot.
jarrange(list(

# Plot a) Cumulative overall effect
ggplot(cball_dt[x %in% x_range, ], aes(x = x * 100, y = y)) +
    geom_hline(yintercept = 1, lty = 2, lwd = 0.5) + 
    geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = col, alpha = 0.15, col = NA) + 
    geom_line(col = col, lwd = 0.8) +
    #scale_x_continuous(breaks = x_breaks) + 
    labs(x = "Temperature percentile", y = "Odds ratio (OR)") + 
    ggtitle("a) Overall cumulative effect") + 
    jtheme(title_hjust = 0, title_size = 12L,show_grid = TRUE)
,

# Plot b) Lag effect at Q95.
ggplot(cblag_dt, aes(x = x, y = y)) +
geom_hline(yintercept = 1, lty = 2, lwd = 0.5) + 
geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = col_heat, alpha = 0.15, col = NA) + 
geom_line(col = col_heat, lwd = 0.8) +
#scale_x_continuous(breaks = seq(0, cblag$lag[2], by = 1)) + 
labs(x = "Lags (in days)", y = "Odds ratio (OR)") + 
ggtitle(paste0("b) Lag effect at ", cblag$value * 100, "ᵗʰ temperature percentile")) + 
jtheme(title_hjust = 0, title_size = 12L, show_grid = TRUE)
,

# Plot b) Lag effect at Q05
ggplot(cblag_dt2, aes(x = x, y = y)) +
geom_hline(yintercept = 1, lty = 2, lwd = 0.5) + 
geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = col_cold, alpha = 0.15, col = NA) + 
geom_line(col = col_cold, lwd = 0.8) +
#scale_x_continuous(breaks = seq(0, cblag$lag[2], by = 1)) + 
labs(x = "Lags (in days)", y = "Odds ratio (OR)") + 
ggtitle(paste0("c) Lag effect at ", cblag2$value * 100, "ᵗʰ temperature percentile")) + 
jtheme(title_hjust = 0, title_size = 12L, show_grid = TRUE)


))

# Save plot.
save_ggplot(paste0("plots/manuscript/fig_s1_full.png"), size = c(8, 7))


# Figure 3 : Sensitivity and stratified analyses -------------------------------


# Exposure.
exposure <- "heat"

# Quantile.
row <- ifelse(exposure == "heat", 2, 1)

# Load all results of interest.
names <- c(
    "(i) Main",
    #"June to August",
    "(ii) Knots = 25ᵗʰ, 75ᵗʰ",
    "(iii) Percentile = 97.5ᵗʰ",
    "(iv) Max. lag = 3 days",
    "(v) Max. lag = 14 days",
    "(vi) Temperature = Tmax",
    "(vii) Temperature = Tmin",
    "(viii) Non medical centers",
    "(ix) Medical centers only",
    "(x) Midwest",
    "(xi) Northeast",
    "(xii) South",
    "(xiii) West",
    "(xiv) Age < 57",
    "(xv) Age ≥ 57",
    "(xvi) White",
    "(xvii) Non-white"
)
res_heat <- rbind(
    fread(paste0(path, "q_dt_1stage_", exposure, "_main.csv"))[row, ],
    #fread(paste0(path, "q_dt_1stage_", exposure, "_junjulaug.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_knots2575.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_q975.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_lag3days.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_lag14days.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_tmaxq.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_tminq.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_med0.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_med1.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_regMidwest.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_regNortheast.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_regSouth.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_regWest.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_age<57.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_age>57.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_racewhite.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_racenonwhite.csv"))[row, ]
)
res_heat$NAME <- factor(names, level = rev(names))
res_heat$COL <- c(1, rep(2, 2L), rep(3, 2L), rep(4L, 2), rep(5L, 2L), rep(6, 4L), rep(7L, 2L), rep(8L, 2))

# Exposure.
exposure <- "cold"

# Quantile.
row <- ifelse(exposure == "heat", 2, 1)

# Load all results of interest.
names <- c(
    "Main",
    #"June to August",
    "Knots = 25ᵗʰ, 75ᵗʰ",
    "Percentile = 2.5ᵗʰ",
    "Max. lag = 7 days",
    "Max. lag = 21 days",
    "Temperature = Tmax",
    "Temperature = Tmin",
    "Non medical centers",
    "Medical centers only",
    "Midwest",
    "Northeast",
    "South",
    "West",
    "Age < 57",
    "Age ≥ 57",
    "White",
    "Non-white"
)
res_cold <- rbind(
    fread(paste0(path, "q_dt_1stage_", exposure, "_main.csv"))[row, ],
    #fread(paste0(path, "q_dt_1stage_", exposure, "_junjulaug.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_25_75_knots.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_025pctl.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_7dlag.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_21dlag.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_tmaxq.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_tminq.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_medcenter0.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_medcenter1.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_MidwestRegion.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_NortheastRegion.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_SouthRegion.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_WestRegion.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_AgeAboveMedian.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_AgeBelowMedian.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_White.csv"))[row, ],
    fread(paste0(path, "q_dt_1stage_", exposure, "_Nonwhite.csv"))[row, ]
)
res_cold$NAME <- factor(names, level = rev(names))
res_cold$COL <- c(1, rep(2, 2L), rep(3, 2L), rep(4L, 2), rep(5L, 2L), rep(6, 4L), rep(7L, 2L), rep(8L, 2))
cols <- c("black", "darkred", pal_rbow$red, "darkblue", pal_rbow$blue1, pal_rbow$purple, pal_rbow$green3, pal_rbow$green1)

# Plot results.
p1 <- ggplot(res_heat, aes(y = NAME, x = OR, xmin = OR_LOW, xmax = OR_HIGH, fill = as.factor(COL), col = as.factor(COL))) + 
    geom_hline(yintercept = 1:8 * 2 - 1, lwd = 6, alpha = 0.1) + 
    geom_hline(yintercept = 17.1, lwd = 6.6, alpha = 0.15) + 
    geom_point(size = 1.5, pch = 23, show.legend = FALSE) + 
    geom_errorbar(width = 0.2, linewidth = 0.3, show.legend = FALSE) +
    geom_vline(xintercept = 1, lwd = 0.3, lty = 3) + 
    geom_hline(yintercept = c(2, 4, 8, 10, 12, 14, 16) + 0.5, lwd = 0.2, lty = 1) + 
    labs(y = NULL, x = "Odds ratio (OR)") + 
    scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) + 
    scale_color_manual(values = cols) + 
    scale_fill_manual(values = cols) + 
    ggtitle("a) Extreme heat (95ᵗʰ percentile)") + 
    jtheme(title_hjust = 0, title_size = 12L)

p2 <- ggplot(res_cold, aes(y = NAME, x = OR, xmin = OR_LOW, xmax = OR_HIGH, fill = as.factor(COL), col = as.factor(COL))) + 
    geom_hline(yintercept = 1:8 * 2 - 1, lwd = 6, alpha = 0.1) + 
    geom_hline(yintercept = 17.1, lwd = 6.6, alpha = 0.15) + 
    geom_point(size = 1.5, pch = 23, show.legend = FALSE) + 
    geom_errorbar(width = 0.2, linewidth = 0.3, show.legend = FALSE) +
    geom_hline(yintercept = c(2, 4, 8, 10, 12, 14, 16) + 0.5, lwd = 0.2, lty = 1) + 
    geom_vline(xintercept = 1, lwd = 0.3, lty = 3) + 
    #geom_hline(yintercept = c(2, 4, 8, 10, 12, 14, 16) + 0.5, lwd = 0.2, lty = 1) + 
    labs(y = NULL, x = "Odds ratio (OR)") + 
    scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5)) + 
    scale_color_manual(values = cols) + 
    scale_fill_manual(values = cols) + 
    ggtitle("b) Extreme cold (5ᵗʰ percentile)") + 
    jtheme(title_hjust = 0, title_size = 12L)

jarrange(list(p1, p2))

save_ggplot("plots/manuscript/fig_3_sens_strat.png", size = c(9, 4))        


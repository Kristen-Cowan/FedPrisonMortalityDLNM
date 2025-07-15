# 1StageHeat.R


# Author : Kristen Cowan
# Imports : rjutils [remotes::install_github("jeremieboudreault/rjutils")]

#Updated by Kristen 1/15/2025
#Updated by Jeremie 03/11/2025

# Libraries --------------------------------------------------------------------

library(data.table)
library(dlnm)
library(ggplot2)
library(mixmeta)
#library(rjutils)
library(splines)
library(survival)
library(extrafont)
library(dplyr)


# Imports ----------------------------------------------------------------------


# Case crossover dataset.
load(file="TempMortalityCaseCrossoverFinal_Daymet.Rda")
#x<-data.table(case_cross_final[case_cross_final$Region!="Northwest",])
x<-data.table(case_cross_final)
x$Region[x$Region %in% c("Southwest", "West", "Northwest")] <- "West"
x$Region[x$Region %in% c("Ohio Valley", "Upper Midwest")] <- "Midwest"
x$Region[x$Region %in% c("Southeast", "South")] <- "South"
x[, sum(death), by = "Region"]

# 
x[, sum(death), by = RaceWhite]
x[, sum(death)]
# Synthetic data.
#x <- qs::qread("data/synthetic/synthetic_data_crossover_new.qs")


# Parameters -------------------------------------------------------------------


# Temperature & adjustment variables.
temp_var <- "tmean_q_daymet"  #### SHOULD USE QUANTILE VALUES OF Daymet over 2009-2021
tvar <- paste0(temp_var, 0)

# Adjustment variable. Set to NULL for no adjustment.
adj_var <- "relhm_daymet"
avar <- paste0(adj_var, 0)

# Maximum lag 
max_lag <- 7

# Knots for x_lag to put into arglag_tvar.
nk_xlag <- 2

# Position of knots for the main exposure.
#knots_xvar <- c(10, 50, 75)/100 # Gasparinni et al., 2015
knots_xvar <- c(50, 90)/100

# Quantiles for the MMT search.
q_mmt <- seq(10L, 90L, by = 1L)/100 # All year (1th to 99th in Gasparrini et al.)

# Months for the analysis.
months <- c(5:9) # Summer

# Quantile for heat and cold.
q_heat <- 0.95
q_cold <- 0.05

# Verbose.
verbose <- TRUE

# Extract regions.
regs <- unique(x$Region)

# Create identifier to save results.
file_id <- paste0(
  "heat", "_", 
  "fullpop"
)


# Data preparation for DLNM ----------------------------------------------------


# Subset data based on supplied months.
x_sub <- x[month %in% months, ]

# Extract temperatureat lags 0-max_lag day in a list.
temp <- as.matrix(x_sub[, paste0(temp_var, 0:max_lag), with = FALSE])

# Extract humidity lags 0-max_lag day in a list.
if (!is.null(adj_var)) {
  adj <- as.matrix(x_sub[, paste0(adj_var, 0:max_lag), with = FALSE])
}

# Compute value of heat and cold quantile.
#val_heat <- quantile(x_sub[[tvar]], probs = q_heat)
#val_cold <- quantile(x_sub[[tvar]], probs = q_cold)
val_heat <- q_heat
val_cold <- q_cold


# Step 1 : DLNM model fitting for the whole US ---------------------------------


# Model specification for temperature variable and control
arglag_tvar <- list(fun = "ns", knots = logknots(max_lag, nk = nk_xlag))
arglag_avar <- arglag_tvar

# Empty elements to store results.
#coef_list  <- as.list(rep(NA, length.out = length(reg_sel)))
#vcov_list  <- as.list(rep(NA, length.out = length(reg_sel)))
#cr_list    <- as.list(rep(NA, length.out = length(reg_sel)))

# Cross-basis definition.
argvar_tvar <- list(
  fun   = "ns",                                                
  knots = quantile(x_sub[[tvar]], knots_xvar, na.rm = TRUE),
  Bound = range(x_sub[[tvar]])
)   
argvar_avar <- list(fun = "lin")  

# Create the cross-basis for temperature.
cb_temp <- crossbasis(temp, lag = max_lag, argvar = argvar_tvar, arglag = arglag_tvar)

# Create the cross-basis for humidity
if (!is.null(adj_var)) 
  cb_adj <- crossbasis(adj, lag = max_lag, argvar = argvar_avar, arglag = arglag_avar)

# Model.
if (!is.null(adj_var)) {
  dlnm_us <- clogit(x_sub$death ~ cb_temp + cb_adj + strata(x_sub$DEATH_ID))
} else {
  dlnm_us <- clogit(x_sub$death ~ cb_temp + strata(x_sub$DEATH_ID))
}

# Reduce to overall cumulative effect using mean temperature.
redall <- dlnm::crossreduce(cb_temp, dlnm_us, cen = mean(x_sub[[tvar]], na.rm = TRUE))

# Reduce DLNM effect using mean values as MMT.
cb_tvar_reduce <- crossreduce(
  basis = cb_temp, 
  model = dlnm_us, 
  cen   = mean(x_sub[[tvar]]),
  at    = quantile(x_sub[[tvar]], probs = q_mmt)
)

# Extract MMT and extreme temperature threshold.
#mmt <- quantile(x_sub[[tvar]], probs = q_mmt[which.min(cb_tvar_reduce$RRfit)])
mmt <- 0.5

# Reduced function at MMT for plotting.
cb_tvar_reduce <- crossreduce(
  basis = cb_temp, 
  model = dlnm_us, 
  cen   = mmt,
  at    = seq(0.025, 0.975, by = 0.001)
  #at    = seq(ceiling(min(x_sub[[tvar]]) * 10)/10, floor(max(x_sub[[tvar]]) * 10)/10, by = 0.1)
)

# Save for future use.
saveRDS(cb_tvar_reduce, paste0("out/cb_tvar_overall_", file_id, ".rds"))

# Plot reduce cumulative effect.
png(paste0("out/fig_", file_id, ".png"), width = 600 * 3.5, height = 700 * 3.5, res = 300)
par(mfrow = c(2, 1))
plot(cb_tvar_reduce, 
     main = "a) Overall cumulative effect", 
     xlab = "Quantile of temperature",
     ylab = "Odds ratio",
     xlim = quantile(x_sub[[tvar]], probs = c(0.02, 0.98)),
     ylim = c(0.5, 1.5),
     lwd  = 1.5,
)
#abline(v = val_cold, lty = 2 , col = "blue")
#abline(v = val_heat, lty = 2 , col = "red")

# Extract OR at q_cold and q_heat.
cold_ind <- which(abs(cb_tvar_reduce$predvar - val_cold) < 0.0001)
heat_ind <- which(abs(cb_tvar_reduce$predvar - val_heat) < 0.0001)
q_dt <- data.table(
  Q       = c(q_cold, q_heat),
  QVAL    = c(val_cold, val_heat),
  OR      = cb_tvar_reduce$RRfit[c(cold_ind, heat_ind)],
  OR_LOW  = cb_tvar_reduce$RRlow[c(cold_ind, heat_ind)],
  OR_HIGH = cb_tvar_reduce$RRhigh[c(cold_ind, heat_ind)]
)
fwrite(q_dt, paste0("out/q_dt_1stage_", file_id, ".csv"))

# Extract reduce effect of lags at val_heat and val_cold.
#par(mfrow = c(1, 1))
#for (cold_heat in c("cold", "heat")) {
 cold_heat <- "heat" 
  # Reduce exposure-response function.
  cb_lag_reduce <- crossreduce(
    basis  = cb_temp, 
    model  = dlnm_us, 
    type   = "var",
    cen    = mmt,
    value  = get(paste0("val_", cold_heat)),
    bylag  = 0.1
  )
  
  # Save for future use.
  #qs::qsave(cb_lag_reduce, paste0("out/cb_lag_reduce_", cold_heat, "_", file_id, ".qs"))
  saveRDS(cb_lag_reduce, paste0("out/cb_lag_reduce_", file_id, ".rds"))
  
  # Plot the lag structure for heat.
  plot(cb_lag_reduce,
       main = paste0("b) Lag structure at ", get(paste0("val_", cold_heat)) * 100, "th percentile of temperature"),
       xlab = "Lag (in days)",
       ylab = "Odds ratio",
       lwd  = 1.5,
  )
  
  
  dev.off()
  

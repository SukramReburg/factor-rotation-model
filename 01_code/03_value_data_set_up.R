# The Vaulation could be done with the EPS per style from the iShares USMV, 
# QUAL, VLUE. As it is not feasible to scrape this data within the project time-
# line, resp. without any premium subscription.
# 0. Set up Environment ----
rm(list = ls())

source("./01_code/00_initialize.R")

# Define necessary packages
pkgs <- c(data.table = "1.15.0", stringr = "1.5.1")

# Check if all packages are installed with the correct versions.
check_packages(pkgs)

# Load all necessary packages.
load_packages(pkgs)

# 1. Load Data ----
# exported in script 01_macro_model_update.R 
dt_base <- fread(paste0(path_02, "style_ind_incl.csv"))

# 1. Earnings Yield (EY) ----
dt_base[, ey := snp_500_eps / msci_usa]

# Shift so we only use history up to t-1.
dt_base[, ey_last := shift(ey)]

# 3. Expanding score  ----
# of mean and sd of ey up to t-1.
dt_base[, ey_mean_exp := cumsum(fifelse(is.na(ey_last), 0, ey_last)) / pmax(seq_len(.N) - 1, 1)]
dt_base[, ey_var_exp  := cumsum(fifelse(is.na(ey_last), 0, (ey_last - ey_mean_exp)^2)) / pmax(seq_len(.N) - 1, 1)]
dt_base[, ey_sd_exp   := sqrt(ey_var_exp)]

# Expanding z-score (Valuation Score)
dt_base[, val_score := (ey - ey_mean_exp) / ey_sd_exp]
dt_base[val_score == "Inf", val_score := 0]

# 5. Map to style adjustments ----
dt_base[, `:=`(
  val_adj_value   = fifelse(val_score >  1, +0.25, 0),
  val_adj_quality = fifelse(val_score < -1, +0.25, 0),
  val_adj_minVol  = fifelse(val_score < -1, +0.25, 0)
)]

# 3. Export Data ----
# Export the data including value adjusted score and weight changes.. 

fwrite(dt_base, paste0(path_02, "style_ind_incl.csv"))


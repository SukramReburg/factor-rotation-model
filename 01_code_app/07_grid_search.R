# 0. Set up Environment ----
rm(list = ls())

source("./01_code/00_initialize.R")
source("./01_code/04_weights_schemes.R")
source("./01_code/05_backtesting.R")


# Define necessary packages
pkgs <- c(data.table = "1.15.0", ggplot2 = "3.5.0")

# Check if all packages are installed with the correct versions.
check_packages(pkgs)

# Load all necessary packages.
load_packages(pkgs)

# 1. Load Data ----
# exported in script 01_macro_model_update.R
dt_base <- fread(paste0(path_02, "style_ind_incl.csv"))

dt_model <- dt_base[, .(
  date, msci_usa, msci_usa_value, msci_usa_quality, msci_usa_minvol, 
  us_10y_yield = us_10y_yield/(100*12), macro_indicator, macro_trend,
  msci_usa_value_mom, msci_usa_quality_mom, msci_usa_minvol_mom, mom_leader,
  val_score, val_adj_value, val_adj_quality, val_adj_minVol
)]

# Observation: Macro trend is drifting with a negative constant. Maybe
# correction leads to better results.
dt_model[, macro_trend_corr := scale(macro_trend + (1:.N) / 75)]

dt_train <- dt_model[date <= cut_off_date]
dt_test <- dt_model[date > cut_off_date]

# sensible base-weight presets (sum to 1)
.base_hi_opts <- list(
  # High-risk regime (defensive)
  c(Value = .05, Quality = .35, MinVol = .60),
  # middle defensive
  c(Value = .20, Quality = .40, MinVol = .40)
)
.base_lo_opts <- list(
  # Low-risk regime (pro-cyclical)
  c(Value = .40, Quality = .40, MinVol = .20),
  # aggressive
  c(Value = .70, Quality = .25, MinVol = .05)    
)

res <- grid_search_regime(dt_train, cost_bps = 5)
print(res$best)
fwrite(res$best, paste0(path_01, "shiny_input/best_parameters_grid_search.csv"))
head(res$leaderboard, 10)  # top-10 configs

bp <- res$best

bt_best <- backtest_all(
  dt_test,
  cost_bps    = 5,
  base_hi     = c(
    Value = bp$hi_Value,
    Quality = bp$hi_Quality,
    MinVol = bp$hi_MinVol
  ),
  base_lo     = c(
    Value = bp$lo_Value,
    Quality = bp$lo_Quality,
    MinVol = bp$lo_MinVol
  ),
  nudge_val   = bp$nudge_val,
  nudge_mom   = bp$nudge_mom,
  high_risk_th = bp$high_risk_th
)
bt_best$perf[Strategy == "Regime"]


# Optional: cumulative curves
plot_cum <- function(x, nm) {
  data.table(
    date = x$date,
    cum_disc = cumprod(1 + fifelse(is.na(x$ret_disc), 0, x$ret_disc)),
    cum_soft = cumprod(1 + fifelse(is.na(x$ret_soft), 0, x$ret_soft)),
    cum_lin  = cumprod(1 + fifelse(is.na(x$ret_lin), 0, x$ret_lin)),
    cum_reg  = cumprod(1 + fifelse(is.na(x$ret_reg), 0, x$ret_reg)),
    cum_bh   = cumprod(1 + fifelse(is.na(x$ret_bh), 0, x$ret_bh)),
    cum_eq   = cumprod(1 + fifelse(is.na(x$ret_eq), 0, x$ret_eq))
  ) |>
    melt(id.vars = "date",
         variable.name = "Strategy",
         value.name = "CumReturn") |>
    ggplot(aes(date, CumReturn, color = Strategy)) + geom_line() + labs(title =
                                                                          nm, y = "Cumulative (×)")
}
plot_cum(bt_best$dt, "Strategies vs Benchmarks")





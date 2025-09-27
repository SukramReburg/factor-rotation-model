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

dt_model[, macro_trend_corr := scale(macro_trend + (1:.N)/75)]

dt_model <- dt_model[date <= cut_off_date]

# 2. Run it and see results
bt <- backtest_all(dt_model, cost_bps = 5, 
                   base_hi = c(Value = .10,
                               Quality = .40,
                               MinVol = .50),
                   base_lo = c(Value = .60,
                               Quality = .30,
                               MinVol = .10),
                   nudge_val = 0.10, nudge_mom = .10,
                   high_risk_th = 0)

bt$perf[Strategy == "Regime", ]$Sharpe

# Optional: cumulative curves
plot_cum <- function(x, nm) {
  data.table(date = x$date,
             cum_disc = cumprod(1 + fifelse(is.na(x$ret_disc), 0, x$ret_disc)),
             cum_soft = cumprod(1 + fifelse(is.na(x$ret_soft), 0, x$ret_soft)),
             cum_lin  = cumprod(1 + fifelse(is.na(x$ret_lin),  0, x$ret_lin)),
             cum_reg  = cumprod(1 + fifelse(is.na(x$ret_reg),  0, x$ret_reg)),
             cum_bh   = cumprod(1 + fifelse(is.na(x$ret_bh),   0, x$ret_bh)),
             cum_eq   = cumprod(1 + fifelse(is.na(x$ret_eq),   0, x$ret_eq))) |>
    melt(id.vars="date", variable.name="Strategy", value.name="CumReturn") |>
    ggplot(aes(date, CumReturn, color=Strategy)) + geom_line() + labs(title=nm, y="Cumulative (×)")
}
plot_cum(bt$dt, "Strategies vs Benchmarks")


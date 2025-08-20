# 1. Prepare returns ----
prep_returns <- function(dt) {
  dt <- copy(dt)
  setorder(dt, date)
  # monthly arithmetic returns from index levels
  dt[, `:=`(
    r_mkt   = msci_usa / shift(msci_usa) - 1,
    r_val   = msci_usa_value   / shift(msci_usa_value)   - 1,
    r_qual  = msci_usa_quality / shift(msci_usa_quality) - 1,
    r_minv  = msci_usa_minvol  / shift(msci_usa_minvol)  - 1
  )]
  dt
}

# # 2. Build strategy weights ----
# build_weights <- function(dt) {
#   # uses run_all_strategies() from previous step
#   run_all_strategies(dt)
# }

# 3. Apply weights at t to returns at t+1 (no look-ahead) ----
shift_weights_forward <- function(dt, pref="disc") {
  # pref in {"disc","soft","lin","reg"} to pick which columns to use
  nm <- switch(pref,
               "disc" = c("w_disc_Value","w_disc_Quality","w_disc_MinVol"),
               "soft" = c("w_soft_Value","w_soft_Quality","w_soft_MinVol"),
               "lin"  = c("w_lin_Value","w_lin_Quality","w_lin_MinVol"),
               "reg"  = c("w_reg_Value","w_reg_Quality","w_reg_MinVol"))
  dt[, paste0(nm, "_lag1") := lapply(.SD, shift), .SDcols = nm]
  setnames(dt, paste0(nm, "_lag1"), paste0("w_", pref, c("_val","_qual","_minv")))
  dt
}

# 4. Compute portfolio returns & turnover ----
compute_portfolio <- function(dt, pref = "disc", cost_bps = 5) {
  # picks lagged weights created by shift_weights_forward
  wcols <- paste0("w_", pref, c("_val","_qual","_minv"))
  
  # gross portfolio return at t uses lagged weights times style returns at t
  dt[, paste0("ret_", pref) :=
       dt[[wcols[1]]] * r_val +
       dt[[wcols[2]]] * r_qual +
       dt[[wcols[3]]] * r_minv]
  
  # monthly turnover (one-way) from *decision-time* weights
  nm_now <- switch(pref,
                   "disc" = c("w_disc_Value","w_disc_Quality","w_disc_MinVol"),
                   "soft" = c("w_soft_Value","w_soft_Quality","w_soft_MinVol"),
                   "lin"  = c("w_lin_Value","w_lin_Quality","w_lin_MinVol"),
                   "reg"  = c("w_reg_Value","w_reg_Quality","w_reg_MinVol"))
  
  dt[, paste0("turn_", pref) :=
       0.5 * ( abs(get(nm_now[1]) - shift(get(nm_now[1]))) +
                 abs(get(nm_now[2]) - shift(get(nm_now[2]))) +
                 abs(get(nm_now[3]) - shift(get(nm_now[3]))) )]
  
  # net return after transaction costs (cost_bps is ONE-WAY in basis points)
  # TC = turnover (one-way) × cost_per_trade
  dt[, paste0("ret_", pref) :=
       get(paste0("ret_", pref)) - get(paste0("turn_", pref)) * (cost_bps / 1e4)]
  
  dt
}

# 5. Benchmarks ----
add_benchmarks <- function(dt) {
  # Buy & Hold MSCI USA
  dt[, ret_bh := r_mkt]
  
  # Equal-weight 1/3 of styles (rebalanced monthly, no look-ahead needed since constant)
  dt[, ret_eq := (r_val + r_qual + r_minv) / 3]
  dt
}

# 6. Performance helpers ----
perf_stats <- function(rets, rf_rets, freq = 12) {
  
  rets <- rets[is.finite(rets)]
  
  n <- length(rets)
  
  rf_rets <- rf_rets[(length(rf_rets) - n+1):length(rf_rets)]
  
  if (n < 2) return(list(CAGR=NA, Vol=NA, Sharpe=NA, MaxDD=NA))
  
  # CAGR
  g <- prod(1 + rets, na.rm=TRUE)
  yrs <- n / freq
  cagr <- g^(1/yrs) - 1
  
  # Vol (annualized)
  vol <- sd(rets - rf_rets, na.rm=TRUE) * sqrt(freq)
  
  # Excess Return
  r_ex <- mean(rets, na.rm=TRUE) - mean(rf_rets, na.rm = T)
  
  # Sharpe 
  shr <- if (vol > 0) r_ex * sqrt(freq) / vol else NA_real_
  
  # Max Drawdown
  eq <- cumprod(1 + rets)
  peak <- cummax(eq)
  dd <- (eq / peak) - 1
  maxdd <- min(dd, na.rm=TRUE)
  
  list(CAGR=cagr, Vol=vol, Sharpe=shr, MaxDD=maxdd)
}

# 7. End-to-end runner ----
backtest_all <- function(dt_model,
                         w_macro=0.4, w_mom=0.4, w_val=0.2,
                         gap=0.15, lambda=2, floors=0.05, caps=0.80,
                         cost_bps = 5, 
                         base_hi = c(Value = .10,
                                     Quality = .40,
                                     MinVol = .50),
                         base_lo = c(Value = .60,
                                     Quality = .30,
                                     MinVol = .10),
                         nudge_val = 0.10, nudge_mom = .10,
                         high_risk_th = .5) {
  
  # 7.1 returns
  dt <- prep_returns(dt_model)
  
  # 7.2 strategies (adds comp_* and weight columns)
  dt <- run_all_strategies(dt,
                           w_macro=w_macro, w_mom=w_mom, w_val=w_val,
                           gap=gap, lambda=lambda, floors=floors, caps=caps, 
                           cost_bps = cost_bps, 
                           base_hi = base_hi,
                           base_lo = base_lo,
                           nudge_val = nudge_val, nudge_mom = nudge_mom,
                           high_risk_th=high_risk_th)
  
  # 7.3 create lagged weights for each scheme
  for (pref in c("disc","soft","lin","reg")) {
    dt <- shift_weights_forward(dt, pref=pref)
    dt <- compute_portfolio(dt, pref=pref, cost_bps)
  }
  
  # 7.4 add benchmarks
  dt <- add_benchmarks(dt)
  
  # 7.5 tidy performance table
  res <- list(
    Discrete = perf_stats(dt$ret_disc, dt$us_10y_yield),
    Softmax  = perf_stats(dt$ret_soft, dt$us_10y_yield),
    Linear   = perf_stats(dt$ret_lin, dt$us_10y_yield),
    Regime   = perf_stats(dt$ret_reg, dt$us_10y_yield),
    BuyHold  = perf_stats(dt$ret_bh, dt$us_10y_yield),
    EqWeight = perf_stats(dt$ret_eq, dt$us_10y_yield)
  )
  
  tab <- rbindlist(lapply(names(res), function(nm) {
    as.data.table(cbind(Strategy = nm, as.data.frame(res[[nm]])))
  }), fill=TRUE)
  
  # 7.6 average turnover (for strategies only)
  to_tab <- data.table(
    Strategy=c("Discrete","Softmax","Linear","Regime"),
    Turnover= c(mean(dt$turn_disc, na.rm=TRUE),
                mean(dt$turn_soft, na.rm=TRUE),
                mean(dt$turn_lin,  na.rm=TRUE),
                mean(dt$turn_reg,  na.rm=TRUE))
  )
  
  list(dt = dt, perf = merge(tab, to_tab, by="Strategy", all=TRUE))
}

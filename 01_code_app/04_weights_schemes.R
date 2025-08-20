library(data.table)

# 0. Helper: per-row comp scores from your signals ----
compose_comp_scores <- function(x,
                                w_macro = 0.4,
                                w_mom = 0.4,
                                w_val = 0.2) {
  # x is a single-row data.table (.SD in by=row ops), names as in dt_model
  # Map market-wide macro & valuation into style-specific scores
  Macro <- c(
    Value   = 1 - x$macro_indicator,
    # low macro (low risk) -> Value
    Quality = x$macro_indicator,
    # high macro (high risk) -> defensives
    MinVol  = x$macro_indicator
  )
  Val <- c(
    Value   = x$val_score,
    # cheap -> Value
    Quality = 1 - x$val_score,
    # expensive -> defensives
    MinVol  = 1 - x$val_score
  )
  Mom <- c(
    Value   = x$msci_usa_value_mom,
    Quality = x$msci_usa_quality_mom,
    MinVol  = x$msci_usa_minvol_mom
  )
  
  # Optional extra adjustments if present (or zero otherwise)
  val_adj <- c(
    Value   = if (!is.null(x$val_adj_value))
      x$val_adj_value
    else
      0,
    Quality = if (!is.null(x$val_adj_quality))
      x$val_adj_quality
    else
      0,
    MinVol  = if (!is.null(x$val_adj_minVol))
      x$val_adj_minVol
    else
      0
  )
  
  # Composite score per style (all pillars already in [0,1])
  comp <- w_macro * Macro + w_mom * Mom + w_val * Val + val_adj
  
  # Return list suitable for := assignment
  as.list(comp)
}

# 1. Discrete rotation ----
# (winner-take-all) with gap + macro guardrail
pick_discrete_row <- function(comp,
                              mom_leader = NA_character_,
                              macro_trend = 0,
                              gap = 0.15) {
  # comp: named numeric c(Value=?, Quality=?, MinVol=?)
  if (anyNA(comp)) {
    w <- rep(NA_real_, length(comp))
    names(w) <- names(comp)
    return(w)
  }
  ord <- order(comp, decreasing = TRUE)
  top <- names(comp)[ord[1]]
  lead_gap <- comp[ord[1]] - comp[ord[2]]
  
  # gap rule: if too close, prefer momentum leader; else keep top
  if (!is.na(mom_leader) &&
      lead_gap < gap && mom_leader %in% names(comp)) {
    top <- mom_leader
  }
  # macro guardrail: if trending down and winner is Value -> switch to MinVol
  if (!is.na(macro_trend) &&
      macro_trend < 0 && top == "Value" && "MinVol" %in% names(comp)) {
    top <- "MinVol"
  }
  
  w <- setNames(rep(0, length(comp)), names(comp))
  w[top] <- 1
  w
}

# 2. Softmax weighting ----
softmax_row <- function(comp,
                        lambda = 2,
                        floors = 0.0,
                        caps = 1.0) {
  if (anyNA(comp)) {
    w <- rep(NA_real_, length(comp))
    names(w) <- names(comp)
    return(w)
  }
  x <- lambda * comp
  x <- x - max(x)
  w <- exp(x)
  w <- w / sum(w)
  # floors/caps
  floors <- if (length(floors) == 1)
    setNames(rep(floors, length(w)), names(w))
  else
    floors
  caps   <- if (length(caps) == 1)
    setNames(rep(caps, length(w)), names(w))
  else
    caps
  w <- pmax(w, floors)
  w <- pmin(w, caps)
  w / sum(w)
}

# 3. Linear rescale weighting ----
linear_row <- function(comp, floors = 0.05, caps = 0.80) {
  if (anyNA(comp)) {
    w <- rep(NA_real_, length(comp))
    names(w) <- names(comp)
    return(w)
  }
  rng <- range(comp)
  width <- diff(rng)
  s <- if (width == 0)
    rep(1 / length(comp), length(comp))
  else
    (comp - rng[1])  / width
  w <- s / sum(s)
  floors <- if (length(floors) == 1)
    setNames(rep(floors, length(w)), names(w))
  else
    floors
  caps   <- if (length(caps) == 1)
    setNames(rep(caps, length(w)), names(w))
  else
    caps
  w <- pmax(w, floors)
  w <- pmin(w, caps)
  w / sum(w)
}

# 4. Regime-based blend ----
# (HighRisk vs LowRisk from macro_indicator) 
regime_blend_row <- function(macro_indicator,
                             val_score,
                             mom = c(Value = NA,
                                     Quality = NA,
                                     MinVol = NA),
                             base_hi = c(Value = .10,
                                         Quality = .40,
                                         MinVol = .50),
                             base_lo = c(Value = .60,
                                         Quality = .30,
                                         MinVol = .10),
                             nudge_val = .10, nudge_mom = .10,
                             high_risk_th = .50) {
  # Determine regime: high risk if macro >= 0.5 
  base <- if (!is.na(macro_indicator) &&
              macro_indicator >= high_risk_th)
    base_hi
  else
    base_lo
  
  # Valuation tilt: cheap -> shift to Value, expensive -> to defensives
  if (!is.na(val_score)) {
    shift_amt <- (val_score) * nudge_val # map [0,1]->[-nudge,+nudge]
    base["Value"]  <- base["Value"]  + shift_amt
    base["Quality"] <- base["Quality"] - shift_amt / 2
    base["MinVol"] <- base["MinVol"] - shift_amt / 2
  }
  
  if (!all(is.na(mom))) {
    m <- mom
    m[!is.finite(m)] <- 0
    base["Value"] <- base["Value"] + nudge_mom * m["Value"]          
    base["Quality"] <- base["Quality"] + nudge_mom * m["Quality"]          
    base["MinVol"] <- base["MinVol"] + nudge_mom * m["MinVol"]          
    base[base < 0] <- 0
    base <- base / sum(base)
  }
  
  base <- pmax(base, 0)
  w <- base / sum(base)
  w
}

# DRIVER: apply all strategies over dt_model ----
run_all_strategies <- function(dt_model,
                               w_macro = 0.4,
                               w_mom = 0.4,
                               w_val = 0.2,
                               gap = 0.15,
                               lambda = 2,
                               floors = 0.05,
                               caps = 0.80, 
                               cost_bps = 5, 
                               base_hi = c(Value = .10,
                                           Quality = .40,
                                           MinVol = .50),
                               base_lo = c(Value = .60,
                                           Quality = .30,
                                           MinVol = .10),
                               nudge_val = .10, nudge_mom = .10,
                               high_risk_th = .5) {
  dt <- copy(dt_model)
  setorder(dt, date)
  
  # 1) Build composite scores per style
  dt[, c("comp_Value", "comp_Quality", "comp_MinVol") :=
       compose_comp_scores(.SD, w_macro, w_mom, w_val), by = seq_len(nrow(dt))]
  
  # 2) Strategy 1: Discrete rotation
  dt[, c("w_disc_Value", "w_disc_Quality", "w_disc_MinVol") := {
    comp <- c(Value = comp_Value,
              Quality = comp_Quality,
              MinVol = comp_MinVol)
    w <- pick_discrete_row(comp,
                           mom_leader = mom_leader,
                           macro_trend = macro_trend,
                           gap = gap)
    as.list(w)
  }, by = seq_len(nrow(dt))]
  
  # 3) Strategy 2: Softmax
  dt[, c("w_soft_Value", "w_soft_Quality", "w_soft_MinVol") := {
    comp <- c(Value = comp_Value,
              Quality = comp_Quality,
              MinVol = comp_MinVol)
    w <- softmax_row(comp,
                     lambda = lambda,
                     floors = floors,
                     caps = caps)
    as.list(w)
  }, by = seq_len(nrow(dt))]
  
  # 4) Strategy 3: Linear rescale
  dt[, c("w_lin_Value", "w_lin_Quality", "w_lin_MinVol") := {
    comp <- c(Value = comp_Value,
              Quality = comp_Quality,
              MinVol = comp_MinVol)
    w <- linear_row(comp, floors = floors, caps = caps)
    as.list(w)
  }, by = seq_len(nrow(dt))]
  
  # 5) Strategy 4: Regime-based blend
  dt[, c("w_reg_Value", "w_reg_Quality", "w_reg_MinVol") := {
    w <- regime_blend_row(
      macro_indicator = macro_indicator,
      val_score = val_score,
      mom = c(
        Value = msci_usa_value_mom,
        Quality = msci_usa_quality_mom,
        MinVol = msci_usa_minvol_mom
      ),
      base_hi = base_hi,
      base_lo = base_lo,
      nudge_val = nudge_val, nudge_mom = nudge_mom,
      high_risk_th = high_risk_th
    )
    as.list(w)
  }, by = seq_len(nrow(dt))]
  
  dt[]
}

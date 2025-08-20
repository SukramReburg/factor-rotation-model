# 0. Set up Environment ----
rm(list = ls())

source("./01_code/00_initialize.R")

# Define necessary packages
pkgs <- c(data.table = "1.15.0", openxlsx2 = "1.18", lubridate = "1.9.3", stringr = "1.5.1", 
          ggplot2 = "3.5.0", genlasso = "1.6.1")

# Check if all packages are installed with the correct versions.
check_packages(pkgs)

# Load all necessary packages.
load_packages(pkgs)

# 1. Data Loading ----
# Style Indices data
dt_base <- as.data.table(openxlsx2::read_xlsx(file = paste0(path_02, "Style-Indices_final.xlsx")))

# Baa Corporate Bond Yields
dt_corp_bond_y <- as.data.table(openxlsx2::read_xlsx(
  file = paste0(path_02, "federal_reserve_bank_stlouis.xlsx"),
  sheet = "CORP_Baa_bond_yields"
))

# US Unemployment claims
dt_unemployement <- as.data.table(openxlsx2::read_xlsx(
  file = paste0(path_02, "federal_reserve_bank_stlouis.xlsx"),
  sheet = "unemployment_weekly_claims"
))

# New Housing units issued
dt_housing <- as.data.table(openxlsx2::read_xlsx(
  file = paste0(path_02, "federal_reserve_bank_stlouis.xlsx"),
  sheet = "new_privately_owned_housing_uni"
))

# Volatility Index
dt_vix <- as.data.table(openxlsx2::read_xlsx(
  file = paste0(path_02, "vix-daily.xlsx")
))

setnames(dt_vix, tolower(names(dt_vix)))

dt_vix[, date := fifelse(
  str_detect(date, "/"),
  as.Date(date, format = "%m/%d/%Y"),
  as.Date(date, format = "%Y-%d-%m")
)]

dt_vix[, c("open", "high", "low", "close") := lapply(.SD, as.numeric), 
       .SDcols = c("open", "high", "low", "close")]

dt_vix <- dt_vix[, .(
  close_vix = mean(close)), 
  by = .(year(date), month(date))]

dt_vix[, date := as.Date(paste0(year, "-", month, "-", 1))]

dt_vix[, date := month_ultimo(date)]


dt_vix <- dt_vix[, !c("month", "year")]

# 2. Data pre processing ----
# Some basic checks.

## 2.1 Style Indices ----
sapply(dt_base, class)

setnames(
  dt_base,
  c(
    "date",
    "msci_usa",
    "msci_usa_value",
    "msci_usa_quality",
    "msci_usa_minvol",
    "un",
    "gold",
    "copper",
    "gold_copper_rel",
    "us_3m_yield",
    "us_10y_yield",
    "snp_500_eps"
  )
)

dt_base[, lapply(.SD, function(x) sum(is.na(x)))]

dt_base <- dt_base[, !"un"]

# To normalize it, even possibly no trading day, we transform to ultimo.
dt_base[, date := month_ultimo(date)]

## 2.2 CORP bond yield ----
sapply(dt_corp_bond_y, class)

setnames(dt_corp_bond_y, "yield", "corp_bond_yield")
dt_corp_bond_y[, lapply(.SD, function(x) sum(is.na(x)))]

range(dt_corp_bond_y$date, na.rm = T)

dt_corp_bond_y <- na.omit(dt_corp_bond_y)

range(dt_corp_bond_y$date, na.rm = T)

# St. Louis Fed reports first of the following month.
dt_corp_bond_y[, date := month_ultimo(date)]

## 2.3 Unemployment ----
sapply(dt_unemployement, class)

setnames(dt_unemployement, "num_claims", "num_unempl_claims")
dt_unemployement[, lapply(.SD, function(x) sum(is.na(x)))]

range(dt_unemployement$date, na.rm = T)

dt_unemployement <- dt_unemployement[, .(
  num_unempl_claims = mean(num_unempl_claims)), 
                 by = .(year(date), month(date))]

dt_unemployement[, date := as.Date(paste0(year, "-", month, "-", 1))]

dt_unemployement[, date := month_ultimo(date)]

dt_unemployement <- dt_unemployement[, !c("month", "year")]

## 2.4 Housing permits ----
# we need to shift such that the data is available at trading time. 
dt_housing[, date := date %m+% months(1)]
dt_housing[, date := month_ultimo(date)]

## 2.5 Combine data set ----

dt_base <- merge(dt_base, dt_corp_bond_y, by = "date", all.x = T)

dt_base[, table(is.na(corp_bond_yield))]

dt_base <- merge(dt_base, dt_housing, by = "date", all.x = T)

dt_base[, table(is.na(num_permits))]

dt_base <- merge(dt_base, dt_unemployement, by = "date", all.x = T)

dt_base[, table(is.na(num_unempl_claims))]

dt_base <- merge(dt_base, dt_vix, by = "date", all.x = T)

dt_base[, table(is.na(close_vix))]


setorder(dt_base, date)

## 2.6 Attach Macroeconomic columns ----
# Yield Curve Slope = 10-year Treasury yield minus 3-month Treasury yield
# Measures the steepness of the yield curve.
# Positive slope → normal curve, expectations of growth.
# Negative slope → inverted curve, often signals recession.
dt_base[, yield_curve_slope := us_10y_yield - us_3m_yield]

# Credit Spread = Moody’s Baa corporate bond yield minus 10-year Treasury yield
# Captures additional comp. investors demand for holding riskier corp bonds.
# Widening spread → higher credit risk, financial stress (risk-off).
# Narrowing spread → lower credit risk, supportive of equities (risk-on).
dt_base[, credit_spread := corp_bond_yield - us_10y_yield]

# 2.7 standardize Data ----
macro_vars <- c("yield_curve_slope", "credit_spread",
                "num_unempl_claims", "num_permits", "close_vix", 
                "gold", "copper", "gold_copper_rel")

# NOTE: In this prototype we standardize the macro indicators and run PCA 
# on the full sample. This is fine for exploration, but in a strict backtest 
# it introduces look-ahead bias (because the mean/sd and PCA loadings use 
# future information). 
# A production-grade version should recompute normalization and PCA loadings 
# on an expanding (or rolling) window so that, at each point in time, only 
# data available up to t-1 is used.

for (v in macro_vars) {
  if (v %in% c("credit_spread",
             "num_unempl_claims",
             "close_vix",
             "gold",
             "gold_copper_rel")){
    dt_base[, (paste0(v, "_z")) := scale(get(v))]
  } else {
    dt_base[, (paste0(v, "_z")) := scale(-get(v))]
  }
}

macro_mat <- as.matrix(dt_base[, paste0(macro_vars, "_z"), with = FALSE])
pca_res <- prcomp(macro_mat, center = FALSE, scale. = FALSE)

# Extract first principal component
dt_base[, macro_indicator := pca_res$x[,1]]

# Plot macro indicator vs. individual macroeconomic indicators.
dt_long <- melt(
  dt_base,
  id.vars = "date",
  measure.vars = c(paste0(macro_vars, "_z"), "macro_indicator"),
  variable.name = "indicator",
  value.name = "value"
)

ggplot(
  dt_long,
  aes(
    x = date,
    y = value,
    color = indicator,
    linewidth = indicator == "macro_indicator"
  )
) +
  geom_line() +
  scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.5), guide = "none") +
  labs(x = "Date", y = "Value", color = "Macro Variable") +
  theme_minimal()


# 1. Fit trend filtering model
y <- dt_base$macro_indicator
tf_res <- trendfilter(y, ord = 1)   # piecewise linear trend

# 2. Cross-validation to select lambda
cv <- cv.trendfilter(tf_res)        # just pass the fitted object

# 3. Predict betas to see degree of freedom for labmbda.se1 and select it.
fit <- predict(tf_res, lambda = cv$lambda.1se)
df_val <- fit$df

# Choose smoothness by target degrees of freedom (df): smaller = smoother
dfs <- c(2,5,10.25,50,100,137,150)

fits <- lapply(dfs, function(d) stats::filter(y, rep(1/d, d), sides = 2))
names(fits) <- paste0("df=", dfs)

plot_trend_fits(y, fits, dfs, df_val = df_val)
# Especially the higher dfs seem to deliver unreasonable results from the 
# penalization. 
# As selecting df and lambda with Cross Validation seems not feasible at this 
# point, a selected set of degree of freedoms will be tested and selected on 
# "expert-judgment".
dfs <- c(2,7,17,22)

fits <- lapply(dfs, function(d) stats::filter(y, rep(1/d, d), sides = 2))
names(fits) <- paste0("df=", dfs)

plot_trend_fits(y, fits, dfs, df_val = df_val)

# 4. Extract coefficients at the chosen degree of freedom.
df_final = 5
fits <- lapply(df_final, function(d) stats::filter(y, rep(1/d, d), sides = 2))
names(fits) <- paste0("df=", df_final)

plot_trend_fits(y, fits, df_final, df_val = df_final)

beta_hat <- fits$`df=5`

# 5. Add to the data.table

dt_base[, macro_trend := as.numeric(beta_hat)]

# As interpolation is only done except in the last ad first interpolation 
# interval we need to extrapolate these intervals. 

dt_base[, macro_trend := fill_trend_edges(y = y, trend = macro_trend)]



dt_long <- melt(
  dt_base,
  id.vars = "date",
  measure.vars = c(paste0(macro_vars, "_z"), "macro_indicator", "macro_trend"),
  variable.name = "indicator",
  value.name = "value"
)

ggplot(dt_long, aes(x = date, y = value, color = indicator, 
                    linewidth = indicator == "macro_trend")) +
  geom_line() +
  scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.5), guide = "none") +
  labs(x = "Date", y = "Value", color = "Macro Variable") +
  theme_minimal()


# 3. Export Data ----
# Export the data including macro trend and macro indicator . Currently inverse 
# to market setiment (i.e. high macro trend / indicator --> high Risk and vice
# versa).

fwrite(dt_base, paste0(path_02, "style_ind_incl.csv"))
# app.R — Report-style Shiny app to replace slides
# Chapters:
# 1) Executive Summary
# 2) Data & Preprocessing (Macro, Valuation, Momentum with LaTeX + plots)
# 3) Model Strategies (formulas + cumulative)
# 4) Regime Optimization (grid search results)
# 5) Results & Dashboard (tables, equity curves, drawdowns, weights, current style)

# Define necessary packages
pkgs <- c(data.table = "1.15.0", shiny = "1.8.0", shinyWidgets = "0.9.0", 
          bslib = "0.6.1", ggplot2 = "3.5.0", plotly = "4.10.4", DT = "0.33",
          stringr = "1.5.1")

# Check if all packages are installed with the correct versions.
check_packages(pkgs)

# Load all necessary packages.
load_packages(pkgs)

# Helper: minimalist plot theme
theme_min <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"),
        legend.position = "bottom")

# If dt_model is not found in the env, allow upload
dt_model <- fread("./shiny_input/style_ind_incl.csv")
has_dt <- exists("dt_model", inherits = TRUE)
source("./00_function_pool.R")
source("./04_weights_schemes.R")
source("./05_backtesting.R")

# ---------- Data bootstrap ----------
has_dt <- exists("dt_model", inherits = TRUE)

# Load optimized hyperparameters (fixed recommendation for display)
opt_hyperparameters <- tryCatch(
  fread("./shiny_input/best_parameters_grid_search.csv"),
  error = function(e) NULL
)
has_opt <- !is.null(opt_hyperparameters) && nrow(opt_hyperparameters) > 0

# Utility helpers
theme_min <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"),
        legend.position = "bottom")

fmt_pct <- function(x, d=1) scales::percent(x, accuracy = 10^-d)

# Rolling Sharpe (annualized) with window n months
roll_sharpe <- function(r, n = 36, freq = 12) {
  z <- rep(NA_real_, length(r))
  for (i in seq_along(r)) {
    if (i >= n) {
      w <- r[(i-n+1):i]
      mu <- mean(w, na.rm=TRUE) * freq
      sd_ <- sd(w, na.rm=TRUE) * sqrt(freq)
      z[i] <- if (sd_ > 0) mu / sd_ else NA_real_
    }
  }
  z
}

# Centered moving average with df (odd/even handled by stats::filter width)
cma <- function(y, df) {
  stats::filter(y, rep(1/df, df), sides = 2)
}

# ======================= UI =======================
ui <- navbarPage(
  title = "Erste AM – Style Rotation Report",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  collapsible = TRUE,
  
  # ============= 1) Executive Summary ============
  tabPanel("Executive Summary",
           fluidRow(
             column(
               width = 3,
               div(class="mb-3 p-2 border rounded bg-light",
                   if (!has_dt) fileInput("file_dt", "Upload dt_model (.rds/.fst/.csv)",
                                          accept = c(".rds",".fst",".csv"))
                   else tags$small("Using in-memory dt_model")),
               h5("Macro signal (for charts & backtest)"),
               radioGroupButtons("macro_sig", NULL,
                                 choices = c("Indicator (raw z)"="ind",
                                             "Precomputed Trend"="trend_pre",
                                             "Centered MA Trend (df)"="trend_df"),
                                 justified = TRUE, size = "sm"),
               sliderInput("df_cma", "Centered moving average df", min = 3, max = 21, value = 5, step = 2),
               helpText("df controls the width of the centered moving average used as macro trend (stats::filter, sides=2)."),
               br(),
               withMathJax()
             ),
             column(
               width = 9,
               br(),
               h4("Management Summary"),
               HTML("
        <p>
        Wir bauen ein dynamisches Rotationsmodell über <b>Value</b>, <b>Quality</b> und <b>Minimum Volatility</b>.
        Drei Säulen liefern das Signal:
        </p>
        <ul>
          <li><b>Macro (marktweit)</b>: PCA-basierter Konjunkturfaktor; höher = riskanteres Umfeld → <i>Quality/MinVol</i> bevorzugt, niedriger = risikoärmer → <i>Value</i>.</li>
          <li><b>Valuation (marktweit)</b>: Earnings Yield = S&amp;P 500 EPS / MSCI USA; höher = günstiger → Value-Neigung.</li>
          <li><b>Momentum (stilspezifisch)</b>: 1/3/6/12M (skip-month) Renditen, cross-sektional z-standardisiert, pro Stil aggregiert.</li>
        </ul>
        <p>
        Wir vergleichen vier Zuweisungsregeln (Diskret, Softmax, Linear, Regime-basiert) und wählen die <b>Regime-Strategie</b>, da sie
        Konjunkturlogik, Bewertungsdisziplin und Marktverhalten (Momentum) integriert.
        </p>
        "),
               br(),
               plotlyOutput("plt_cum_exec", height = 420),
               br(),
               DTOutput("tbl_perf_exec")
             )
           )
  ),
  
  # ============= 2) Data & Preprocessing ============
  tabPanel("Data & Preprocessing",
           tabsetPanel(
             tabPanel("Macro",
                      fluidRow(
                        column(
                          4,
                          withMathJax(),
                          h4("Makro-Konstrukt: Motivation & Methodik"),
                          HTML("
              <p><b>Warum Macro?</b> Stilprämien sind konjunktursensitiv. In risikoarmen Phasen (lockere Finanzierung, steigende Gewinnrevisionen) profitieren zyklische Segmente und damit <i>Value</i>.
              In risikoreichen Phasen (Konjunkturabkühlung, Finanzierungsstress) dominieren Defensiva: <i>Quality</i> und <i>MinVol</i>.</p>
              <p><b>Wie konstruiert?</b> Wir nehmen mehrere breit verfügbare Makro- und Marktnäherungsgrößen (z. B. S&P EPS, Rohstoffe wie Gold/Kupfer, Zinsniveaus) und extrahieren per PCA
              die gemeinsame Komponente:</p>
              $$ Macro_t = PC1\\big(Z(X_{1,t}),\\ldots,Z(X_{k,t})\\big) $$
              <p>Zur Regime-Erkennung glätten wir mit einer <b>zentrierten gleitenden Durchschnittsfilterung</b> der Breite <i>df</i>:</p>
              $$\\hat{m}_t = \\arg\\min_m \\sum_t |x_t - m_t| + \\lambda \\sum_t |m_t - m_{t-1}|$$
              <p>Dies ist äquivalent zu <code>stats::filter(Macro, rep(1/df, df), sides=2)</code>. Die UI steuert hier <b>df</b>, nicht λ.</p>
            "),
                          sliderInput("df_macro_tab", "df für Trend-Plot", min = 3, max = 21, value = 5, step = 2),
                          helpText("Ändern Sie df, um die Regime-Glättung visuell nachzuvollziehen.")
                        ),
                        column(
                          8,
                          plotlyOutput("plt_macro_series", height = 360),
                          br(),
                          plotOutput("plt_macro_hist", height = 180)
                        )
                      )
             ),
             tabPanel("Valuation",
                      fluidRow(
                        column(
                          4, withMathJax(),
                          h4("Bewertung: Motivation & Methodik"),
                          HTML("
              <p><b>Warum Bewertung?</b> Hohe Bewertung (niedrige Earnings Yield) begrenzt künftige Renditen; in <i>teuren</i> Phasen ist defensiver Stilattribution vorteilhaft. Umgekehrt bieten
              <i>günstige</i> Märkte Rückenwind für Value.</p>
              <p><b>Wie konstruiert?</b> Earnings Yield (EY) aus Index-EPS und -Preis:
              $$ EY_t = \\frac{EPS_t}{Price_t}, \\quad val_t = \\frac{EY_t - \\mu_{1:(t-1)}}{\\sigma_{1:(t-1)}} $$
              <p>Wir nutzen <b>expanding</b> Mittelwert/Std. bis t−1 (keine Look-Ahead-Bias) und erhalten einen z‑Score (positiv = günstig → Value-Tilt).</p>
            ")
                        ),
                        column(
                          8,
                          plotOutput("plt_val_series", height = 360),
                          br(),
                          plotOutput("plt_val_density", height = 180)
                        )
                      )
             ),
             tabPanel("Momentum",
                      fluidRow(
                        column(
                          4, withMathJax(),
                          h4("Momentum: Motivation & Methodik"),
                          HTML("
              <p><b>Warum Momentum?</b> Der Momentum-Effekt (Fortsetzung jüngster Gewinner) ist robust über Märkte und Zeiträume. Auf Stil-Ebene hilft er, den <i>aktuellen Favoriten</i> zu erkennen.</p>
              <p><b>Wie konstruiert?</b> Für jeden Stil i und Horizonte h ∈ {1,3,6,12} Monate (mit Skip-Monat):
              $$ R^{h}_{i,t} = \\frac{P_{i,t-1}}{P_{i,t-1-h}} - 1, \\quad z^{h}_{i,t} = \\frac{R^{h}_{i,t} - \\mu^{h}_{t}}{\\sigma^{h}_{t}} $$
              <p>Dann mitteln wir die z‑Scores über Horizonte (ggf. höheres Gewicht auf 3–6M). Ergebnis ist ein <i>stilspezifischer</i> Momentum-z‑Score.</p>
            ")
                        ),
                        column(
                          8,
                          plotOutput("plt_mom_smallmultiples", height = 360),
                          br(),
                          plotOutput("plt_mom_hist", height = 180)
                        )
                      )
             )
           )
  ),
  
  # ============= 3) Model Strategies ============
  tabPanel("Model Strategies",
           fluidRow(
             column(
               12,
               br(),
               withMathJax(),
               h4("Von Signalen zu Gewichten: Vier Zuweisungsregeln"),
               HTML("
          <p>Alle Strategien nutzen dieselben drei Säulen (Macro, Valuation, Momentum), unterscheiden sich jedoch darin, wie sie in Gewichte übersetzt werden.</p>
          <ol>
            <li><b>Diskret (Winner-Takes-All)</b>: Wählt den Stil mit höchstem Komposit, mit Gap-Regel gegen Flips und Makro-Guardrail (kein Value bei neg. Makrotrend).</li>
            <li><b>Softmax</b>: Glatte Alternative: \\(w_{i,t} \\propto e^{\\lambda \\cdot Comp_{i,t}}\\). \\(\\lambda\\) steuert die Konzentration.</li>
            <li><b>Linear</b>: Skaliert Komposits über Stile auf [0,1] und normalisiert. Transparente, wenig extreme Allokation.</li>
            <li><b>Regime</b>: Ökonomisch motiviert: Makro-Regime setzt Basisgewichte (risk-on/off), Bewertung verschiebt Richtung Value/Defensiv, Momentum legt eine kleine Überlagerung obendrauf.</li>
          </ol>
          <p>Komposit pro Stil i:
          $$ Comp_{i,t} = \\alpha\\,Mom_{i,t} + \\beta\\,MacroScore_{i,t} + \\gamma\\,Val_{i,t}, \\quad \\alpha+\\beta+\\gamma=1 $$
          </p>
          <p><i>Warum Regime?</i> Es vereint Zykluslogik (Makro), Rendite-Erwartung (Bewertung) und Persistenz (Momentum) – ein konsistentes Narrativ, das in der Praxis gut kommunizierbar ist.</p>
        "),
               br(),
               plotlyOutput("plt_cum_all", height = 420)
             )
           )
  ),
  
  # ============= 4) Results & Dashboard (with controls) ============
  tabPanel("Results & Dashboard",
           fluidRow(
             column(
               width = 3,
               h5("Run Backtest (fixed or manual params)"),
               # Fixed optimized params (loaded)
               if (has_opt) div(class="mb-2 p-2 border rounded bg-light",
                                tags$b("Recommended (from optimization):"),
                                br(),
                                tags$small(HTML(sprintf(
                                  "macro_type: <b>%s</b>, high_risk_th: <b>%.2f</b>, nudge_val: <b>%.2f</b>, nudge_mom: <b>%.2f</b>",
                                  opt_hyperparameters$macro_type[1],
                                  opt_hyperparameters$high_risk_th[1],
                                  opt_hyperparameters$nudge_val[1],
                                  opt_hyperparameters$nudge_mom[1]
                                ))),
                                br(),
                                tags$small(HTML(sprintf(
                                  "base_hi (V/Q/MV): <b>%.2f / %.2f / %.2f</b><br>base_lo (V/Q/MV): <b>%.2f / %.2f / %.2f</b>",
                                  opt_hyperparameters$hi_Value[1], opt_hyperparameters$hi_Quality[1], opt_hyperparameters$hi_MinVol[1],
                                  opt_hyperparameters$lo_Value[1], opt_hyperparameters$lo_Quality[1], opt_hyperparameters$lo_MinVol[1]
                                )))
               ),
               checkboxInput("use_opt", "Use recommended params", value = has_opt),
               
               # Manual params (enabled if not using recommended)
               h5("Manual Parameters"),
               sliderInput("cost_bps", "Transaction costs (bps, one-way)", 0, 25, 5, step = 1),
               numericInput("hi_val",  "High-risk: Value",   value = if (has_opt) opt_hyperparameters$hi_Value[1]   else 0.10, min=0, max=1, step=.01),
               numericInput("hi_qual", "High-risk: Quality", value = if (has_opt) opt_hyperparameters$hi_Quality[1] else 0.40, min=0, max=1, step=.01),
               numericInput("hi_minv", "High-risk: MinVol",  value = if (has_opt) opt_hyperparameters$hi_MinVol[1]  else 0.50, min=0, max=1, step=.01),
               numericInput("lo_val",  "Low-risk: Value",    value = if (has_opt) opt_hyperparameters$lo_Value[1]   else 0.60, min=0, max=1, step=.01),
               numericInput("lo_qual", "Low-risk: Quality",  value = if (has_opt) opt_hyperparameters$lo_Quality[1] else 0.30, min=0, max=1, step=.01),
               numericInput("lo_minv", "Low-risk: MinVol",   value = if (has_opt) opt_hyperparameters$lo_MinVol[1]  else 0.10, min=0, max=1, step=.01),
               sliderInput("nudge_val", "Valuation nudge (Δw to Value at |z|~1)", 0, 0.2, if (has_opt) opt_hyperparameters$nudge_val[1] else 0.10, step = .01),
               sliderInput("nudge_mom", "Momentum overlay (strength)",           0, 0.2, if (has_opt) opt_hyperparameters$nudge_mom[1] else 0.05, step = .01),
               
               # Macro source for backtest: tie to Executive choice (incl. df option)
               helpText("Macro signal used in backtest follows selection on Executive Summary (Indicator / Precomputed Trend / df-based Trend)."),
               actionBttn("run_bt", "Run Backtest", style="fill", color="primary", icon=icon("play"), block=TRUE),
               br(),
               withMathJax()
             ),
             column(
               width = 9,
               br(),
               plotlyOutput("plt_cum_regime", height = 360),
               br(),
               DTOutput("tbl_perf_res"),
               br(),
               fluidRow(
                 column(6, plotOutput("plt_dd", height = 240)),
                 column(6, plotOutput("plt_weights_area", height = 240))
               ),
               br(),
               fluidRow(
                 column(6, plotOutput("plt_rolling_sharpe", height = 240)),
                 column(6, plotOutput("plt_monthly_hist", height = 240))
               ),
               br(),
               uiOutput("current_style_box")
             )
           )
  )
)

# ======================= SERVER =======================
server <- function(input, output, session) {
  
  # ---- Data loader ----
  r_dt <- reactive({
    if (has_dt) return(dt_model)
    f <- input$file_dt
    validate(need(!is.null(f), "Upload dt_model to proceed"))
    ext <- tools::file_ext(f$datapath)
    if (ext == "rds") readRDS(f$datapath)
    else if (ext == "fst") { requireNamespace("fst"); fst::read_fst(f$datapath, as.data.table = TRUE) }
    else if (ext == "csv") fread(f$datapath)
    else validate("Unsupported file type. Use .rds, .fst, or .csv.")
  })
  
  # ---- Macro selection for charts/backtest: 'ind', 'trend_pre', 'trend_df' ----
  r_dt_with_macro <- reactive({
    dt <- copy(r_dt())
    # compute df-based centered MA on the raw indicator (for plotting/backtest if chosen)
    df_use <- input$df_cma
    if (!is.null(df_use) && df_use >= 3) {
      dt[, macro_trend_df := as.numeric(cma(macro_indicator, df_use))]
    } else {
      dt[, macro_trend_df := macro_trend]  # fallback
    }
    # choose macro_use per selection
    if (input$macro_sig == "ind") {
      dt[, macro_use := macro_indicator]
    } else if (input$macro_sig == "trend_pre") {
      dt[, macro_use := macro_trend]
    } else {
      dt[, macro_use := macro_trend_df]
    }
    dt
  })
  
  # ---------- Executive Summary ----------
  output$plt_cum_exec <- renderPlotly({
    # quick snapshot using default params (or recommended) for a headline plot
    dt_use <- r_dt_with_macro()
    # choose base weights from recommended (if available) for headline
    if (has_opt && isTRUE(input$use_opt)) {
      base_hi <- c(Value=opt_hyperparameters$hi_Value[1], Quality=opt_hyperparameters$hi_Quality[1], MinVol=opt_hyperparameters$hi_MinVol[1])
      base_lo <- c(Value=opt_hyperparameters$lo_Value[1], Quality=opt_hyperparameters$lo_Quality[1], MinVol=opt_hyperparameters$lo_MinVol[1])
      th <- opt_hyperparameters$high_risk_th[1]
      nv <- opt_hyperparameters$nudge_val[1]
      nm <- opt_hyperparameters$nudge_mom[1]
    } else {
      base_hi <- c(Value=.10, Quality=.40, MinVol=.50)
      base_lo <- c(Value=.60, Quality=.30, MinVol=.10)
      th <- .5; nv <- .10; nm <- .05
    }
    bt <- backtest_all(
      dt_use,
      cost_bps    = 5,
      base_hi     = base_hi,
      base_lo     = base_lo,
      nudge_val   = nv,
      nudge_mom   = nm,
      high_risk_th= th
    )
    x <- bt$dt
    plt <- data.table(
      date = x$date,
      Regime   = cumprod(1 + fifelse(is.na(x$ret_reg_net), 0, x$ret_reg_net)),
      EqWeight = cumprod(1 + fifelse(is.na(x$ret_eq),      0, x$ret_eq)),
      BuyHold  = cumprod(1 + fifelse(is.na(x$ret_bh),      0, x$ret_bh))
    ) |>
      melt(id.vars="date", variable.name="Strategy", value.name="Cum")
    p <- ggplot(plt, aes(date, Cum, color=Strategy)) +
      geom_line(linewidth=.8) + labs(title="Cumulative Return (×)", x=NULL, y=NULL) + theme_min
    ggplotly(p) %>% layout(legend = list(orientation="h", y=-0.2))
  })
  output$tbl_perf_exec <- renderDT({
    # same as above but just a headline summary table
    dt_use <- r_dt_with_macro()
    if (has_opt && isTRUE(input$use_opt)) {
      base_hi <- c(Value=opt_hyperparameters$hi_Value[1], Quality=opt_hyperparameters$hi_Quality[1], MinVol=opt_hyperparameters$hi_MinVol[1])
      base_lo <- c(Value=opt_hyperparameters$lo_Value[1], Quality=opt_hyperparameters$lo_Quality[1], MinVol=opt_hyperparameters$lo_MinVol[1])
      th <- opt_hyperparameters$high_risk_th[1]
      nv <- opt_hyperparameters$nudge_val[1]
      nm <- opt_hyperparameters$nudge_mom[1]
    } else {
      base_hi <- c(Value=.10, Quality=.40, MinVol=.50)
      base_lo <- c(Value=.60, Quality=.30, MinVol=.10)
      th <- .5; nv <- .10; nm <- .05
    }
    bt <- backtest_all(
      dt_use,
      cost_bps    = 5,
      base_hi     = base_hi,
      base_lo     = base_lo,
      nudge_val   = nv,
      nudge_mom   = nm,
      high_risk_th= th
    )
    perf <- copy(bt$perf)
    perf[, `:=`(
      CAGR  = fmt_pct(CAGR),
      Vol   = fmt_pct(Vol),
      MaxDD = fmt_pct(MaxDD),
      Turnover = ifelse(is.na(Turnover), "", fmt_pct(Turnover))
    )]
    datatable(perf, rownames=FALSE, options=list(dom="tp", pageLength=10))
  })
  
  # ---------- Data & Preprocessing: Macro ----------
  output$plt_macro_series <- renderPlotly({
    dt <- r_dt_with_macro()
    df_plot <- input$df_macro_tab
    y <- dt$macro_indicator
    tr_df <- as.numeric(cma(y, df_plot))
    dd <- data.table(date=dt$date, Indicator=y, Trend_df=tr_df)
    m  <- melt(dd, id.vars="date", variable.name="Series", value.name="Value")
    p <- ggplot(m, aes(date, Value, color=Series)) + geom_line() +
      labs(title=sprintf("Macro: Indicator vs Centered MA (df=%d)", df_plot), x=NULL, y="z-score") + theme_min
    ggplotly(p)
  })
  output$plt_macro_hist <- renderPlot({
    dt <- r_dt_with_macro()
    ggplot(data.table(x=dt$macro_use), aes(x)) +
      geom_histogram(bins=40) + labs(title="Macro (selected signal) distribution", x=NULL, y=NULL) + theme_min
  })
  
  # ---------- Data & Preprocessing: Valuation ----------
  output$plt_val_series <- renderPlot({
    dt <- r_dt()
    ggplot(dt, aes(date, val_score)) + geom_line() +
      labs(title="Valuation z-score (Earnings Yield expanding z)", x=NULL, y="z") + theme_min
  })
  output$plt_val_density <- renderPlot({
    dt <- r_dt()
    ggplot(dt, aes(val_score)) + geom_density() +
      labs(title="Valuation density", x="z", y=NULL) + theme_min
  })
  
  # ---------- Data & Preprocessing: Momentum ----------
  output$plt_mom_smallmultiples <- renderPlot({
    dt <- r_dt()
    m <- melt(dt[, .(date,
                     Value = msci_usa_value_mom,
                     Quality = msci_usa_quality_mom,
                     `MinVol` = msci_usa_minvol_mom)],
              id.vars="date", variable.name="Style", value.name="Mom")
    ggplot(m, aes(date, Mom)) + geom_line() +
      facet_wrap(~Style, ncol=1, scales="free_y") +
      labs(title="Momentum z-scores per style", x=NULL, y="z") + theme_min
  })
  output$plt_mom_hist <- renderPlot({
    dt <- r_dt()
    m <- melt(dt[, .(Value = msci_usa_value_mom,
                     Quality = msci_usa_quality_mom,
                     MinVol = msci_usa_minvol_mom)],
              variable.name="Style", value.name="Mom")
    ggplot(m, aes(Mom, fill=Style)) + geom_histogram(alpha=.7, position="identity", bins=40) +
      labs(title="Momentum distribution (z)", x="z", y=NULL) + theme_min
  })
  
  # ---------- Model Strategies cumulative ----------
  output$plt_cum_all <- renderPlotly({
    # Use Results-params? Keep this tab focused on method comparison with recommended
    dt_use <- r_dt_with_macro()
    if (has_opt) {
      base_hi <- c(Value=opt_hyperparameters$hi_Value[1], Quality=opt_hyperparameters$hi_Quality[1], MinVol=opt_hyperparameters$hi_MinVol[1])
      base_lo <- c(Value=opt_hyperparameters$lo_Value[1], Quality=opt_hyperparameters$lo_Quality[1], MinVol=opt_hyperparameters$lo_MinVol[1])
      th <- opt_hyperparameters$high_risk_th[1]
      nv <- opt_hyperparameters$nudge_val[1]
      nm <- opt_hyperparameters$nudge_mom[1]
    } else {
      base_hi <- c(Value=.10, Quality=.40, MinVol=.50)
      base_lo <- c(Value=.60, Quality=.30, MinVol=.10)
      th <- .5; nv <- .10; nm <- .05
    }
    bt <- backtest_all(
      dt_use,
      cost_bps    = 5,
      base_hi     = base_hi,
      base_lo     = base_lo,
      nudge_val   = nv,
      nudge_mom   = nm,
      high_risk_th= th
    )
    x <- bt$dt
    plt <- data.table(
      date = x$date,
      Discrete = cumprod(1 + fifelse(is.na(x$ret_disc_net),0,x$ret_disc_net)),
      Softmax  = cumprod(1 + fifelse(is.na(x$ret_soft_net),0,x$ret_soft_net)),
      Linear   = cumprod(1 + fifelse(is.na(x$ret_lin_net), 0,x$ret_lin_net)),
      Regime   = cumprod(1 + fifelse(is.na(x$ret_reg_net), 0,x$ret_reg_net)),
      BuyHold  = cumprod(1 + fifelse(is.na(x$ret_bh),      0,x$ret_bh)),
      EqWeight = cumprod(1 + fifelse(is.na(x$ret_eq),      0,x$ret_eq))
    ) |>
      melt(id.vars="date", variable.name="Strategy", value.name="Cum")
    p <- ggplot(plt, aes(date, Cum, color=Strategy)) + geom_line(linewidth=.8) +
      labs(title="Cumulative Returns (×)", x=NULL, y=NULL) + theme_min
    ggplotly(p) %>% layout(legend = list(orientation="h", y=-0.2))
  })
  
  # ---------- Results & Dashboard: run backtest ----------
  r_bt <- eventReactive(input$run_bt, {
    dt_use <- r_dt_with_macro()
    
    # choose params: recommended or manual
    if (isTRUE(input$use_opt) && has_opt) {
      base_hi <- c(Value=opt_hyperparameters$hi_Value[1], Quality=opt_hyperparameters$hi_Quality[1], MinVol=opt_hyperparameters$hi_MinVol[1])
      base_lo <- c(Value=opt_hyperparameters$lo_Value[1], Quality=opt_hyperparameters$lo_Quality[1], MinVol=opt_hyperparameters$lo_MinVol[1])
      th <- opt_hyperparameters$high_risk_th[1]
      nv <- opt_hyperparameters$nudge_val[1]
      nm <- opt_hyperparameters$nudge_mom[1]
    } else {
      base_hi <- c(Value=input$hi_val, Quality=input$hi_qual, MinVol=input$hi_minv)
      s <- sum(base_hi); if (is.finite(s) && s>0) base_hi <- base_hi/s
      base_lo <- c(Value=input$lo_val, Quality=input$lo_qual, MinVol=input$lo_minv)
      s2 <- sum(base_lo); if (is.finite(s2) && s2>0) base_lo <- base_lo/s2
      th <- 0.5  # using Executive selection implies we use macro_use, threshold on that scale
      nv <- input$nudge_val
      nm <- input$nudge_mom
    }
    
    backtest_all(
      dt_use,
      cost_bps    = input$cost_bps,
      base_hi     = base_hi,
      base_lo     = base_lo,
      nudge_val   = nv,
      nudge_mom   = nm,
      high_risk_th= th
    )
  }, ignoreInit = TRUE)
  
  # Plots/tables in Results
  output$plt_cum_regime <- renderPlotly({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    x <- bt$dt
    plt <- data.table(
      date = x$date,
      Regime   = cumprod(1 + fifelse(is.na(x$ret_reg_net), 0, x$ret_reg_net)),
      BuyHold  = cumprod(1 + fifelse(is.na(x$ret_bh),      0, x$ret_bh)),
      EqWeight = cumprod(1 + fifelse(is.na(x$ret_eq),      0, x$ret_eq))
    ) |>
      melt(id.vars="date", variable.name="Strategy", value.name="Cum")
    p <- ggplot(plt, aes(date, Cum, color=Strategy)) + geom_line(linewidth=.9) +
      labs(title="Regime vs Benchmarks (Cumulative, net of costs)", x=NULL, y=NULL) + theme_min
    ggplotly(p) %>% layout(legend=list(orientation="h", y=-0.2))
  })
  
  output$tbl_perf_res <- renderDT({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    perf <- copy(bt$perf)
    perf[, `:=`(
      CAGR  = fmt_pct(CAGR),
      Vol   = fmt_pct(Vol),
      MaxDD = fmt_pct(MaxDD),
      Turnover = ifelse(is.na(Turnover), "", fmt_pct(Turnover))
    )]
    datatable(perf, rownames=FALSE, options=list(dom="tp", pageLength=10))
  })
  
  output$plt_dd <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    r <- bt$dt$ret_reg_net
    eq <- cumprod(1 + fifelse(is.na(r), 0, r))
    dd <- eq / cummax(eq) - 1
    df <- data.table(date = bt$dt$date, Drawdown = dd)
    ggplot(df, aes(date, Drawdown)) + geom_area() +
      scale_y_continuous(labels = scales::percent) +
      labs(title="Regime Strategy Drawdown", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_weights_area <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    w <- bt$dt[, .(date, Value=w_reg_Value, Quality=w_reg_Quality, MinVol=w_reg_MinVol)]
    m <- melt(w, id.vars="date", variable.name="Style", value.name="Weight")
    ggplot(m, aes(date, Weight, fill=Style)) +
      geom_area(position="stack", alpha=.85) +
      scale_y_continuous(labels=scales::percent) +
      labs(title="Regime Weights Over Time", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_rolling_sharpe <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    rs <- roll_sharpe(bt$dt$ret_reg_net, n = 36, freq = 12)
    df <- data.table(date = bt$dt$date, RollingSharpe = rs)
    ggplot(df, aes(date, RollingSharpe)) + geom_line() +
      labs(title="Rolling Sharpe (36M, annualized)", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_monthly_hist <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    d <- data.table(Return = bt$dt$ret_reg_net)
    ggplot(d, aes(Return)) + geom_histogram(bins = 40) +
      scale_x_continuous(labels = scales::percent) +
      labs(title="Monthly Returns Distribution (Regime, net)", x=NULL, y=NULL) + theme_min
  })
  
  output$current_style_box <- renderUI({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    last <- tail(bt$dt[, .(Value=w_reg_Value, Quality=w_reg_Quality, MinVol=w_reg_MinVol)], 1)
    sty <- names(last)[which.max(as.numeric(last))]
    wgt <- max(as.numeric(last))
    div(class="p-3 border rounded",
        tags$h4("Current Style in Favor"),
        tags$h2(sprintf("%s (%.0f%%)", sty, 100*wgt)))
  })
}

shinyApp(ui, server)

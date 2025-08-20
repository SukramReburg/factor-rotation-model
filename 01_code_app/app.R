# app.R — Report-style Shiny app to replace slides
# Chapters:
# 1) Executive Summary
# 2) Data & Preprocessing (Macro, Valuation, Momentum with LaTeX + plots)
# 3) Model Strategies (formulas + cumulative)
# 4) Regime Optimization (grid search results)
# 5) Results & Dashboard (tables, equity curves, drawdowns, weights, current style)

source("./00_function_pool.R")

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
dt_model <- dt_model[, .(
  date, msci_usa, msci_usa_value, msci_usa_quality, msci_usa_minvol, 
  us_10y_yield = us_10y_yield/(100*12), macro_indicator, macro_trend,
  msci_usa_value_mom, msci_usa_quality_mom, msci_usa_minvol_mom, mom_leader,
  val_score, val_adj_value, val_adj_quality, val_adj_minVol
)]

has_dt <- exists("dt_model", inherits = TRUE)
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

# ---------- Small helpers ----------
theme_min <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"),
        legend.position = "bottom")

fmt_pct <- function(x, d=1) scales::percent(x, accuracy = 10^-d)

# type-safe series getter: always returns numeric vector (0 if column missing)
safe_series <- function(DT, nm) {
  if (!is.data.table(DT)) DT <- as.data.table(DT)
  if (nm %in% names(DT)) {
    out <- as.numeric(DT[[nm]])
    out[!is.finite(out)] <- NA_real_
    return(out)
  } else {
    return(rep(NA_real_, nrow(DT)))
  }
}

# robust cumulative product: replace non-finite returns with 0 before cumprod
cumcurve <- function(r) {
  rr <- as.numeric(r)
  rr[!is.finite(rr)] <- 0
  cumprod(1 + rr)
}

# Rolling Sharpe (annualized) with window n months
roll_sharpe <- function(r, n = 36, freq = 12) {
  r <- as.numeric(r)
  r[!is.finite(r)] <- NA_real_
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

# Centered moving average with df (odd/even supported by stats::filter width)
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
               helpText("df steuert die Breite des zentrierten gleitenden Durchschnitts (stats::filter, sides=2)."),
               br(),
               withMathJax()
             ),
             column(
               width = 9,
               br(),
               h4("Management Summary"),
               HTML("
        <p>
        Dynamisches Rotationsmodell über <b>Value</b>, <b>Quality</b> und <b>Minimum Volatility</b>.
        Drei Säulen:
        <ul>
          <li><b>Macro</b> (PCA-basiert; gefiltert per zentriertem gleitenden Durchschnitt mit df)</li>
          <li><b>Valuation</b> (Earnings Yield z‑Score; S&amp;P 500 EPS / MSCI USA)</li>
          <li><b>Momentum</b> (stilspezifisch; 1/3/6/12M Skip‑Month; z‑Scores)</li>
        </ul>
        Dieses Modell operationalisiert eine Drei-Säulen-Sicht auf die Märkte – Makro, Bewertung und Momentum – in monatliche Stil-Allokationen über Value, Quality und Minimum Volatility. 
        Wir vermeiden bewusst Look-Ahead, indem wir expandierende Z-Scores (bis t−1) und Skip-Month-Momentum verwenden, und wir wenden einen zentrierten gleitenden Durchschnittstrend mit benutzerkontrollierter <i>df</i> an, um das Makro-Regimesignal zu stabilisieren. 
        Der gewählte regimespezifische Allokator ist sowohl interpretierbar als auch praktisch: Makro definiert eine Basis (Risk-On vs. Risk-Off), Bewertung verschiebt die Basis in Richtung Value, wenn Märkte günstig sind (oder in Richtung Defensivwerte, wenn teuer), und Momentum fügt eine kleine Überlagerung hinzu, um den aktuellen Führer zu begünstigen. 
        Die Out-of-Time-Disziplin wird durch die Fixierung von Hyperparametern aus einem separaten Optimierungsschritt erzwungen, und Transaktionskosten werden über Turnover × Kosten (Standard 5 Basispunkte je Richtung) modelliert, sodass die berichtete Performance nach realistischen Friktionen netto ist. 
        Im Vergleich zu Gleichgewichtungs- und Buy-and-Hold-Benchmarks zielt der Regime-Ansatz auf höhere risikoadjustierte Renditen ab, während er typischerweise Drawdowns während makroökonomischer Stressphasen reduziert.
        </p>
          <p>
          Zur Transparenz sind alle Inputs, Transformationen und Gewichtungen in der App sichtbar: Sie können die Makro-Komponenten, den Bewertungsanker und das Momentum auf Stilebene inspizieren und genau nachvollziehen, wie sie in Allokationen übersetzt werden. 
        Das Dashboard zeigt <i>welcher</i> Stil aktuell bevorzugt wird, wie sich die Gewichtungen im Zeitverlauf entwickelt haben und wie sich das Portfolio in rollierenden Sharpe-Ratios und Monatsrendite-Verteilungen verhalten hat. 
        Wir heben auch Modellannahmen und bekannte Einschränkungen hervor: Bewertung verwendet derzeit den marktweiten Gewinn je Aktie (EPS) als Proxy für fundamentale Stilgrößen; Makro-PCA-Ladungen werden als stabil behandelt; und die monatliche Stichprobe kann intramonthliche Dynamiken verpassen. 
        Geplante Erweiterungen umfassen stil-spezifische Bewertungsinputs (z. B. ETF/Index-Factsheets), regimespezifische Kovarianzen für Risikobudgetierung, Stresstests zu makroökonomischen Schockszenarien sowie eine vollständig walk-forward kalibrierte Pipeline. 
        Insgesamt ist das Modell so gestaltet, dass es in einem Investmentkomitee im Gedächtnis bleibt: Es verbindet klare ökonomische Intuition mit disziplinierter Implementierung und reproduzierbarer Evidenz.
        <ul>
        Wir vergleichen Diskret, Softmax, Linear, Regime – und verwenden optimierte Regime‑Parameter für die finale Darstellung.
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
                          h4("Makro: Motivation & Methodik"),
                          HTML("
              <p><b>Warum Macro?</b> Stilprämien sind konjunktursensitiv. In risikoarmen Phasen (lockere Finanzierung, steigende Gewinnrevisionen) profitieren zyklische Segmente und damit <i>Value</i>.
              In risikoreichen Phasen (Konjunkturabkühlung, Finanzierungsstress) dominieren Defensiva: <i>Quality</i> und <i>MinVol</i>.</p>
              <p><b>Wie konstruiert?</b> 
              <ul> 
              Für die Makro-Säule nutzen wir mehrere öffentlich verfügbare Indikatoren, die als Früh- oder Spätindikatoren für das Marktumfeld gelten:
              <ul>
                <li><b>US 10Y Treasury Yield minus 3-month Treasury yield:</b> repräsentiert die langfristigen Zinserwartungen und dient als Maß für die Geldpolitik und Inflationserwartungen.</li>
                <li><b>Credit Spread (Baa–US Treasury 10y yield):</b> misst das Risiko im Unternehmensanleihenmarkt; steigt er, signalisiert das erhöhte Risikoaversion.</li>
                <li><b>Anzahl der neu angemeldeten Arbeitslosen pro Monat.</b></li>
                <li><b>Goldpreis:</b> traditioneller 'Safe Haven'. Steigende Preise deuten oft auf Unsicherheit hin.</li>
                <li><b>Kupferpreis / Gold-Kupfer-Ratio:</b> Kupfer gilt als 'Dr. Copper', ein guter Frühindikator für industrielle Nachfrage. Das Verhältnis zu Gold ist ein klassisches Barometer für Risikoappetit.</li>
                <li><b>Anzahl der Neuanmledungen von Eigenheimen.<b> Gilt als Frühindikator für die konjunkturelle Entwicklung, da eine steigende Bautätigkeit auf wachsenden Konsum, Investitionen und Vertrauen in die wirtschaftliche Lage hindeutet.</li>
              </ul>
              Wir bündeln diese Variablen über eine <b>PCA</b>, um ein gemeinsames Faktorsignal zu extrahieren. Danach wird dieses Signal geglättet (zentrierter Moving Average), sodass wir eine robuste <b>Makro-Trendserie</b> erhalten.

              $$ Macro_t = PC1\\big(Z(X_{1,t}),\\ldots,Z(X_{k,t})\\big) $$
              <p>Zur Regime-Erkennung glätten wir mit einer <b>zentrierten gleitenden Durchschnittsfilterung</b> der Breite <i>df</i>:</p>
              $$\\hat{m}_t = \\arg\\min_m \\sum_t |x_t - m_t| + \\lambda \\sum_t |m_t - m_{t-1}|$$
              <p>Dies ist äquivalent zu <code>stats::filter(Macro, rep(1/df, df), sides=2)</code>. Die UI steuert hier <b>df</b>, nicht λ.</p>
            "),
                          sliderInput("df_macro_tab", "df für Trend-Plot", min = 3, max = 21, value = 5, step = 2)
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
<p>
Der Value-Score misst, ob der Markt im historischen Vergleich günstig oder teuer ist. 
Dazu nutzen wir das <b>Earnings Yield</b> des Gesamtmarkts:
</p>

$$ EY_t = \\frac{EPS_t}{IndexLevel_t} $$

<p>
Um zu beurteilen, ob das aktuelle Niveau hoch oder niedrig ist, standardisieren wir es als <b>expanding z-score</b> bis zum Zeitpunkt t-1 (keine Look-Ahead-Bias):
</p>

$$ ValScore_t = \\frac{EY_t - \\mu_{1:(t-1)}}{\\sigma_{1:(t-1)}} $$

<p>
Ein <b>positiver ValScore</b> bedeutet: der Markt ist im Vergleich zu seiner Historie günstig → Allokation Richtung <b>Value</b>.  
Ein <b>negativer ValScore</b> bedeutet: teuer → Allokation in <b>defensive Stile</b> (Quality, MinVol).
</p>
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
<p>
Momentum basiert auf der Beobachtung, dass Gewinner über mittlere Zeithorizonte (3–12 Monate) oft weiter outperformen. 
Wir berechnen daher für jeden Stil (Value, Quality, MinVol) die <b>monatlichen logarithmischen Renditen</b> und kumulieren sie über verschiedene Horizonte (1M, 3M, 6M, 12M). 
Der jeweils letzte Monat (t) wird ausgelassen, um Short-Term-Reversal-Effekte zu vermeiden.
</p>

$$ Mom_{i,t}(h) = \\sum_{k=2}^{h+1} r_{i,t-k} $$

<p>
Dabei ist h der Horizont (z.B. 3 Monate) und i der Stil.  
Um die Vergleichbarkeit sicherzustellen, wird jeder dieser Horizonte über die Stile hinweg <b>standardisiert (z-score)</b>. 
Schließlich wird ein Gesamt-Momentum-Score pro Stil gebildet, indem wir die Horizonte mitteln:
</p>

$$ MomScore_{i,t} = \\frac{1}{H} \\sum_{h \\in \\{1,3,6,12\\}} z\\big(Mom_{i,t}(h)\\big) $$

<p>
Hoher Wert = Stil hat in mehreren Horizonten Outperformance gezeigt → Gewichtung in diesem Stil steigt.
</p>
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
          <p>Komposit pro Stil i:
          $$ Comp_{i,t} = \\alpha\\,Mom_{i,t} + \\beta\\,MacroScore_{i,t} + \\gamma\\,Val_{i,t}, \\quad \\alpha + \\beta + \\gamma = 1. $$
          </p>

          <h5>Softmax (volle Formel)</h5>
          <div style='text-align:center;'>
          $$ w_{i,t} = \\frac{\\exp\\big(\\lambda \\cdot Comp_{i,t}\\big)}{\\sum\\limits_{j \\in \\mathcal{S}} \\exp\\big(\\lambda \\cdot Comp_{j,t}\\big)}. $$
          </div>
          <p>Optional Floors/Caps:
          $$ w'_{i,t} = \\frac{\\min(\\max(w_{i,t}, f_i), c_i)}{\\sum\\limits_{j \\in \\mathcal{S}} \\min(\\max(w_{j,t}, f_j), c_j)}. $$
          </p>

          <h5>Linear (volle Formel)</h5>
          <div style='text-align:center;'>
          $$ \\tilde{Comp}_{i,t} = \\frac{Comp_{i,t} - \\min\\limits_j Comp_{j,t}}{\\max\\limits_j Comp_{j,t} - \\min\\limits_j Comp_{j,t} + \\varepsilon}, \\quad
             w_{i,t} = \\frac{\\tilde{Comp}_{i,t}}{\\sum\\limits_j \\tilde{Comp}_{j,t}}. $$
          </div>

          <h5>Diskret & Regime</h5>
          <p><b>Diskret</b>: argmax des Komposits (mit Gap/Guardrail). <b>Regime</b>: Makro setzt Basis (risk‑on/off), Bewertung verschiebt, Momentum überlagert.</p>
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
    # df-based centered MA on the raw indicator (for plotting/backtest if chosen)
    df_use <- input$df_cma
    if (!is.null(df_use) && df_use >= 3) {
      dt[, macro_trend_df := as.numeric(cma(macro_indicator, df_use))]
    } else {
      dt[, macro_trend_df := macro_trend]
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
    dt_use <- r_dt_with_macro()
    
    # Select params (recommended vs default headline)
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
    
    # Safe numeric series for cumcurves
    reg <- safe_series(x, "ret_reg")
    eqw <- safe_series(x, "ret_eq")
    bh  <- safe_series(x, "ret_bh")
    
    plt <- data.table(
      date = x$date,
      Regime   = cumcurve(reg),
      EqWeight = cumcurve(eqw),
      BuyHold  = cumcurve(bh)
    )
    m <- melt(plt, id.vars="date", variable.name="Strategy", value.name="Cum")
    p <- ggplot(m, aes(date, Cum, color=Strategy)) +
      geom_line(linewidth=.8) + labs(title="Cumulative Return (×)", x=NULL, y=NULL) + theme_min
    ggplotly(p) %>% layout(legend = list(orientation="h", y=-0.2))
  })
  
  output$tbl_perf_exec <- renderDT({
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
    # Format numbers (Sharpe rounded to 4 digits)
    if ("Sharpe" %in% names(perf)) perf[, Sharpe := sprintf("%.4f", as.numeric(Sharpe))]
    if ("CAGR"   %in% names(perf)) perf[,  CAGR := fmt_pct(CAGR)]
    if ("Vol"    %in% names(perf)) perf[,   Vol := fmt_pct(Vol)]
    if ("MaxDD"  %in% names(perf)) perf[, MaxDD := fmt_pct(MaxDD)]
    if ("Turnover" %in% names(perf)) perf[, Turnover := ifelse(is.na(Turnover), "", fmt_pct(Turnover))]
    datatable(perf, rownames=FALSE, options=list(dom="tp", pageLength=10))
  })
  
  # ---------- Data & Preprocessing: Macro ----------
  output$plt_macro_series <- renderPlotly({
    dt <- r_dt_with_macro()
    df_plot <- input$df_macro_tab
    y <- safe_series(dt, "macro_indicator")
    tr_df <- as.numeric(cma(y, df_plot))
    dd <- data.table(date=dt$date, Indicator=y, Trend_df=tr_df)
    m  <- melt(dd, id.vars="date", variable.name="Series", value.name="Value")
    p <- ggplot(m, aes(date, Value, color=Series)) + geom_line() +
      labs(title=sprintf("Macro: Indicator vs Centered MA (df=%d)", df_plot), x=NULL, y="z-score") + theme_min
    ggplotly(p)
  })
  output$plt_macro_hist <- renderPlot({
    dt <- r_dt_with_macro()
    ggplot(data.table(x = safe_series(dt, "macro_use")), aes(x)) +
      geom_histogram(bins=40) + labs(title="Macro (selected signal) distribution", x=NULL, y=NULL) + theme_min
  })
  
  # ---------- Data & Preprocessing: Valuation ----------
  output$plt_val_series <- renderPlot({
    dt <- r_dt()
    ggplot(dt, aes(date, safe_series(dt, "val_score"))) + geom_line() +
      labs(title="Valuation z-score (Earnings Yield expanding z)", x=NULL, y="z") + theme_min
  })
  output$plt_val_density <- renderPlot({
    dt <- r_dt()
    d <- data.table(val = safe_series(dt, "val_score"))
    ggplot(d, aes(val)) + geom_density() +
      labs(title="Valuation density", x="z", y=NULL) + theme_min
  })
  
  # ---------- Data & Preprocessing: Momentum ----------
  output$plt_mom_smallmultiples <- renderPlot({
    dt <- r_dt()
    m <- melt(data.table(
      date = dt$date,
      Value   = safe_series(dt, "msci_usa_value_mom"),
      Quality = safe_series(dt, "msci_usa_quality_mom"),
      `MinVol`= safe_series(dt, "msci_usa_minvol_mom")),
      id.vars="date", variable.name="Style", value.name="Mom")
    ggplot(m, aes(date, Mom)) + geom_line() +
      facet_wrap(~Style, ncol=1, scales="free_y") +
      labs(title="Momentum z-scores per style", x=NULL, y="z") + theme_min
  })
  output$plt_mom_hist <- renderPlot({
    dt <- r_dt()
    m <- melt(data.table(
      Value   = safe_series(dt, "msci_usa_value_mom"),
      Quality = safe_series(dt, "msci_usa_quality_mom"),
      MinVol  = safe_series(dt, "msci_usa_minvol_mom")),
      variable.name="Style", value.name="Mom")
    ggplot(m, aes(Mom, fill=Style)) + geom_histogram(alpha=.7, position="identity", bins=40) +
      labs(title="Momentum distribution (z)", x="z", y=NULL) + theme_min
  })
  
  # ---------- Model Strategies cumulative ----------
  output$plt_cum_all <- renderPlotly({
    dt_use <- r_dt_with_macro()
    
    # Use recommended params if available, else defaults
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
    
    disc <- safe_series(x, "ret_disc")
    soft <- safe_series(x, "ret_soft")
    lini <- safe_series(x, "ret_lin")
    reg  <- safe_series(x, "ret_reg")
    bh   <- safe_series(x, "ret_bh")
    eqw  <- safe_series(x, "ret_eq")
    
    plt <- data.table(
      date = x$date,
      Discrete = cumcurve(disc),
      Softmax  = cumcurve(soft),
      Linear   = cumcurve(lini),
      Regime   = cumcurve(reg),
      BuyHold  = cumcurve(bh),
      EqWeight = cumcurve(eqw)
    )
    m <- melt(plt, id.vars="date", variable.name="Strategy", value.name="Cum")
    p <- ggplot(m, aes(date, Cum, color=Strategy)) + geom_line(linewidth=.8) +
      labs(title="Cumulative Returns (×)", x=NULL, y=NULL) + theme_min
    ggplotly(p) %>% layout(legend = list(orientation="h", y=-0.2))
  })
  
  # ---------- Results & Dashboard: run backtest ----------
  r_bt <- eventReactive(input$run_bt, {
    dt_use <- r_dt_with_macro()
    dt_use <- dt_use[date > as.Date("2015-12-31")]
    
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
      th <- 0.5
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
    reg <- safe_series(x, "ret_reg")
    bh  <- safe_series(x, "ret_bh")
    eqw <- safe_series(x, "ret_eq")
    plt <- data.table(
      date = x$date,
      Regime   = cumcurve(reg),
      BuyHold  = cumcurve(bh),
      EqWeight = cumcurve(eqw)
    )
    m <- melt(plt, id.vars="date", variable.name="Strategy", value.name="Cum")
    p <- ggplot(m, aes(date, Cum, color=Strategy)) + geom_line(linewidth=.9) +
      labs(title="Regime vs Benchmarks (Cumulative, net of costs)", x=NULL, y=NULL) + theme_min
    ggplotly(p) %>% layout(legend=list(orientation="h", y=-0.2))
  })
  
  output$tbl_perf_res <- renderDT({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    perf <- copy(bt$perf)
    # Round Sharpe to 4 digits; format % for others
    if ("Sharpe" %in% names(perf)) perf[, Sharpe := sprintf("%.4f", as.numeric(Sharpe))]
    if ("CAGR"   %in% names(perf)) perf[,  CAGR := fmt_pct(CAGR)]
    if ("Vol"    %in% names(perf)) perf[,   Vol := fmt_pct(Vol)]
    if ("MaxDD"  %in% names(perf)) perf[, MaxDD := fmt_pct(MaxDD)]
    if ("Turnover" %in% names(perf)) perf[, Turnover := ifelse(is.na(Turnover), "", fmt_pct(Turnover))]
    datatable(perf, rownames=FALSE, options=list(dom="tp", pageLength=10))
  })
  
  output$plt_dd <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    r <- safe_series(bt$dt, "ret_reg")
    eq <- cumcurve(r)
    dd <- eq / cummax(eq) - 1
    df <- data.table(date = bt$dt$date, Drawdown = dd)
    ggplot(df, aes(date, Drawdown)) + geom_area() +
      scale_y_continuous(labels = scales::percent) +
      labs(title="Regime Strategy Drawdown", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_weights_area <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    w <- bt$dt[, .(date,
                   Value   = safe_series(bt$dt, "w_reg_Value"),
                   Quality = safe_series(bt$dt, "w_reg_Quality"),
                   MinVol  = safe_series(bt$dt, "w_reg_MinVol"))]
    m <- melt(w, id.vars="date", variable.name="Style", value.name="Weight")
    ggplot(m, aes(date, Weight, fill=Style)) +
      geom_area(position="stack", alpha=.85) +
      scale_y_continuous(labels=scales::percent) +
      labs(title="Regime Weights Over Time", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_rolling_sharpe <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    rs <- roll_sharpe(safe_series(bt$dt, "ret_reg"), n = 36, freq = 12)
    df <- data.table(date = bt$dt$date, RollingSharpe = rs)
    ggplot(df, aes(date, RollingSharpe)) + geom_line() +
      labs(title="Rolling Sharpe (36M, annualized)", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_monthly_hist <- renderPlot({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    d <- data.table(Return = safe_series(bt$dt, "ret_reg"))
    ggplot(d, aes(Return)) + geom_histogram(bins = 40) +
      scale_x_continuous(labels = scales::percent) +
      labs(title="Monthly Returns Distribution (Regime, net)", x=NULL, y=NULL) + theme_min
  })
  
  output$current_style_box <- renderUI({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
    last <- tail(data.table(Value   = safe_series(bt$dt, "w_reg_Value"),
                            Quality = safe_series(bt$dt, "w_reg_Quality"),
                            MinVol  = safe_series(bt$dt, "w_reg_MinVol")), 1)
    sty <- names(last)[which.max(as.numeric(last))]
    wgt <- max(as.numeric(last))
    div(class="p-3 border rounded",
        tags$h4("Current Style in Favor"),
        tags$h2(sprintf("%s (%.0f%%)", sty, 100*wgt)))
  })
}

shinyApp(ui, server)

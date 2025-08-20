# app.R — Report-style Shiny app to replace slides
# Chapters:
# 1) Executive Summary
# 2) Data & Preprocessing (Macro, Valuation, Momentum with LaTeX + plots)
# 3) Model Strategies (formulas + cumulative)
# 4) Regime Optimization (grid search results)
# 5) Results & Dashboard (tables, equity curves, drawdowns, weights, current style)

# Define necessary packages
pkgs <- c(data.table = "1.15.0", shiny = "1.8.0", shinyWidgets = "0.9.0", 
          bslib = "0.6.1", ggplot2 = "3.5.0", plotly = "4.10.4", DT = "0.33")

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
# ---------- Try to pre-load data (or upload in UI) ----------
has_dt <- exists("dt_model", inherits = TRUE)

# ---------- Small helpers ----------
theme_min <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"),
        legend.position = "bottom")

fmt_pct <- function(x, d=1) scales::percent(x, accuracy = 10^-d)

# ---------- UI ----------
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
               h5("Key Settings"),
               sliderInput("cost_bps", "Transaction costs (bps, one-way)", 0, 25, 5, step = 1),
               radioGroupButtons("macro_sig", "Macro signal",
                                 choices = c("Indicator"="ind","Trend"="trend"),
                                 justified = TRUE),
               sliderInput("nudge_val", "Valuation nudge (max |Δw| to Value)", 0, 0.2, 0.10, step=.01),
               sliderInput("nudge_mom", "Momentum nudge (overlay strength)", 0, 0.2, 0.05, step=.01),
               sliderInput("thresh_macro", "High-risk threshold", -0.5, 0.8, 0.5, step=.05),
               actionBttn("run_bt", "Run Backtest", style="fill", color="primary", icon=icon("play"), block=TRUE),
               br(),
               withMathJax()
             ),
             column(
               width = 9,
               br(),
               h4("Management Summary"),
               HTML("
        <p>
        We build a dynamic rotation model across <b>Value</b>, <b>Quality</b>, and <b>Min Volatility</b>.
        Signals:
        <ul>
          <li><b>Macro</b> (market-wide): higher = higher risk → tilt Quality/MinVol; lower = lower risk → tilt Value.</li>
          <li><b>Valuation</b> (market-wide): Earnings Yield = S&P 500 EPS / MSCI USA price; higher = cheaper → tilt Value.</li>
          <li><b>Momentum</b> (style-specific): 1/3/6/12M skip-month cumulative returns, cross-sectional z-normalized.</li>
        </ul>
        Allocation schemes compared: Discrete, Softmax, Linear, and Regime-based. We select the <b>Regime</b> scheme and optimize hyperparameters by OOS grid search.
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
                          3,
                          h5("Macro Construction"),
                          withMathJax(),
                          HTML("
              <p>Macro factor is derived via PCA on standardized indicators, then trend-filtered:</p>
              $$\\text{Macro}_t = \\text{PC1}\\big(Z(X_{1,t}),\\ldots,Z(X_{k,t})\\big)$$
              <p>Trend (optional L1):</p>
              $$\\hat{m}_t = \\arg\\min_m \\sum_t |x_t - m_t| + \\lambda \\sum_t |m_t - m_{t-1}|$$
            "),
                          sliderInput("macro_lambda", "Trend λ (if L1 function available)", 0, 20, 6, step = 1),
                          helpText("If no L1 function is provided, this slider is ignored and the precomputed trend is shown.")
                        ),
                        column(
                          9,
                          plotlyOutput("plt_macro_series", height = 360),
                          br(),
                          plotOutput("plt_macro_hist", height = 180)
                        )
                      )
             ),
             tabPanel("Valuation",
                      fluidRow(
                        column(
                          3, withMathJax(),
                          h5("Valuation (Earnings Yield z‑score)"),
                          HTML("
              $$ EY_t = \\frac{EPS_t}{Price_t}, \\quad
                 val_t = \\frac{EY_t - \\mu_{1:(t-1)}}{\\sigma_{1:(t-1)}} $$
              <p>Positive = cheap (Value tilt), negative = expensive (defensive tilt).</p>
            ")
                        ),
                        column(
                          9,
                          plotOutput("plt_val_series", height = 360),
                          br(),
                          plotOutput("plt_val_density", height = 180)
                        )
                      )
             ),
             tabPanel("Momentum",
                      fluidRow(
                        column(
                          3, withMathJax(),
                          h5("Momentum (per style)"),
                          HTML("
              <p>Skip-month horizons:</p>
              $$ R^{h}_{i,t} = \\frac{P_{i,t-1}}{P_{i,t-1-h}} - 1, \\quad h\\in\\{1,3,6,12\\} $$
              <p>Cross-sectional z at each t per horizon, then weighted average.</p>
            ")
                        ),
                        column(
                          9,
                          plotOutput("plt_mom_smallmultiples", height = 360)
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
               h5("Composite (per style i)"),
               HTML("$$ Comp_{i,t} = \\alpha\\,Mom_{i,t} + \\beta\\,MacroScore_{i,t} + \\gamma\\,Val_{i,t},
              \\quad \\alpha+\\beta+\\gamma=1 $$"),
               h5("Allocators"),
               HTML("
          <ul>
            <li><b>Discrete</b>: \\(w_{i,t}=\\mathbf{1}[i=\\arg\\max Comp]\\) with gap & guardrails.</li>
            <li><b>Softmax</b>: \\(w_{i,t} = \\exp(\\lambda Comp_{i,t}) / \\sum_j \\exp(\\lambda Comp_{j,t})\\).</li>
            <li><b>Linear</b>: rescale Comp to [0,1] across styles, normalize.</li>
            <li><b>Regime</b>: macro regime base weights + valuation tilt + momentum overlay.</li>
          </ul>
        "),
               br(),
               plotlyOutput("plt_cum_all", height = 420)
             )
           )
  ),
  
  # ============= 4) Regime Optimization ============
  tabPanel("Optimization",
           fluidRow(
             column(
               3,
               h5("Grid Search (Regime)"),
               helpText("Tune: high-risk threshold, valuation nudge, momentum nudge, base weights; objective = Sharpe (net of costs)."),
               actionBttn("run_grid", "Run Grid Search", style="fill", color="warning", icon=icon("sliders")),
               br(), br(),
               h6("Notes"),
               helpText("You can precompute grid results offline and load them for this tab if runtime is a concern.")
             ),
             column(
               9,
               DTOutput("tbl_grid_top"),
               br(),
               plotOutput("plt_heat_val_mom", height = 320)
             )
           )
  ),
  
  # ============= 5) Results & Dashboard ============
  tabPanel("Results & Dashboard",
           fluidRow(
             column(
               6,
               br(),
               plotlyOutput("plt_cum_regime", height = 360),
               br(),
               plotOutput("plt_dd", height = 200)
             ),
             column(
               6,
               br(),
               DTOutput("tbl_perf_res"),
               br(),
               plotOutput("plt_weights_area", height = 260)
             )
           ),
           fluidRow(
             column(
               12,
               br(),
               uiOutput("current_style_box")
             )
           )
  )
)

# ---------- SERVER ----------
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
  
  # ---- Switch macro signal per user choice (ind vs trend) ----
  r_dt_with_macro <- reactive({
    dt <- copy(r_dt())
    if (input$macro_sig == "trend") {
      # Use provided macro_trend
      dt[, macro_use := macro_trend]
    } else {
      dt[, macro_use := macro_indicator]
    }
    dt
  })
  
  # ---- Backtest runner (exec & model strategy tabs) ----
  r_bt <- eventReactive(input$run_bt, {
    dt_use <- r_dt_with_macro()
    backtest_all(
      dt_use,
      cost_bps    = input$cost_bps,
      base_hi     = c(Value=.10, Quality=.40, MinVol=.50),
      base_lo     = c(Value=.60, Quality=.30, MinVol=.10),
      nudge_val   = input$nudge_val,
      nudge_mom   = input$nudge_mom,
      high_risk_th= input$thresh_macro
    )
  }, ignoreInit = TRUE)
  
  # ---------- Executive Summary ----------
  output$plt_cum_exec <- renderPlotly({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
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
  
  # ---------- Data & Preprocessing ----------
  output$plt_macro_series <- renderPlotly({
    dt <- r_dt_with_macro()
    # Optional interactive L1 trend recompute if user has the function
    macro_tr <- if (exists("macro_l1_trend")) {
      tryCatch(macro_l1_trend(dt$macro_use, lambda = input$macro_lambda), error=function(e) dt$macro_trend)
    } else dt$macro_trend
    dd <- data.table(date=dt$date, Indicator=dt$macro_indicator, Trend=macro_tr)
    m  <- melt(dd, id.vars="date", variable.name="Series", value.name="Value")
    p <- ggplot(m, aes(date, Value, color=Series)) + geom_line() +
      labs(title="Macro: Indicator vs Trend", x=NULL, y="z-score") + theme_min
    ggplotly(p)
  })
  output$plt_macro_hist <- renderPlot({
    dt <- r_dt_with_macro()
    ggplot(data.table(x=dt$macro_use), aes(x)) +
      geom_histogram(bins=40) + labs(title="Macro distribution", x=NULL, y=NULL) + theme_min
  })
  
  output$plt_val_series <- renderPlot({
    dt <- r_dt(); ggplot(dt, aes(date, val_score)) + geom_line() +
      labs(title="Valuation z-score (EY expanding z)", x=NULL, y="z") + theme_min
  })
  output$plt_val_density <- renderPlot({
    dt <- r_dt(); ggplot(dt, aes(val_score)) + geom_density() +
      labs(title="Valuation density", x="z", y=NULL) + theme_min
  })
  
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
  
  # ---------- Model Strategies ----------
  output$plt_cum_all <- renderPlotly({
    bt <- r_bt(); validate(need(!is.null(bt), "Click Run Backtest"))
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
  
  # ---------- Optimization (Grid Search) ----------
  observeEvent(input$run_grid, {
    showNotification("Running grid search…", type="message", duration=2)
  })
  r_grid <- eventReactive(input$run_grid, {
    validate(need(exists("grid_search_regime"), "Define grid_search_regime() or source it."))
    grid_search_regime(r_dt_with_macro(), cost_bps = input$cost_bps)
  }, ignoreInit = TRUE)
  
  output$tbl_grid_top <- renderDT({
    gs <- r_grid(); validate(need(!is.null(gs), "Run Grid Search"))
    top <- head(gs$leaderboard, 20)
    datatable(top, rownames=FALSE, options=list(dom="tp", pageLength=20))
  })
  
  output$plt_heat_val_mom <- renderPlot({
    gs <- r_grid(); validate(need(!is.null(gs), "Run Grid Search"))
    top <- gs$leaderboard
    # simple heatmap Sharpe by nudge_val x nudge_mom (filter one regime set for readability)
    ref <- top[macro_type == top$macro_type[1] & abs(high_risk_th - top$high_risk_th[1]) < 1e-9]
    ggplot(ref, aes(x=factor(nudge_val), y=factor(nudge_mom), fill=Sharpe)) +
      geom_tile() + geom_text(aes(label=sprintf("%.2f", Sharpe)), size=3) +
      labs(title="Sharpe heatmap (by valuation nudge × momentum nudge)",
           x="nudge_val", y="nudge_mom") +
      theme_min + theme(legend.position="right")
  })
  
  # ---------- Results & Dashboard ----------
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

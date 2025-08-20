# app.R
# R Shiny dashboard for Factor Rotation (Value / Quality / MinVol)
# - Interactively compares multiple weighting schemes vs. benchmarks
# - Plots cumulative returns, turnover, and chosen-style quilt (discrete)
# - Includes a concise methodology page
#
# Requirements:
#   data.table, ggplot2, plotly, DT, bslib, shinyWidgets
#   helper functions: run_all_strategies(), backtest_all()
#   dataset: dt_model

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

# 1. Data Loading and Function Sourcing ----
# If dt_model is not found in the env, allow upload
dt_model <- fread("./shiny_input/style_ind_incl.csv")
has_dt <- exists("dt_model", inherits = TRUE)
source("./00_function_pool.R")
source("./04_weights_schemes.R")
source("./05_backtesting.R")
# -------------------------
# UI
# -------------------------
ui <- navbarPage(
  title = "Erste AM – Style Rotation",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  collapsible = TRUE,
  
  tabPanel("Dashboard",
           fluidRow(
             column(
               width = 3,
               # Data input
               if (!has_dt) {
                 fileInput("file_dt", "Upload dt_model (.rds or .fst or .csv)",
                           accept = c(".rds",".fst",".csv"))
               } else {
                 div(class="mb-3 p-2 border rounded bg-light",
                     tags$small("Using in-memory dt_model"))
               },
               # Controls
               h5("Composite Weights"),
               sliderInput("w_macro","Macro weight", min=0, max=1, value=.4, step=.05),
               sliderInput("w_mom",  "Momentum weight", min=0, max=1, value=.4, step=.05),
               sliderInput("w_val",  "Valuation weight", min=0, max=1, value=.2, step=.05),
               
               h5("Discrete"),
               sliderInput("gap","Gap threshold", min=0, max=0.5, value=.15, step=.01),
               
               h5("Softmax"),
               sliderInput("lambda","λ (concentration)", min=0.2, max=6, value=2, step=.1),
               sliderInput("floors","Floor", min=0, max=.2, value=.05, step=.01),
               sliderInput("caps",  "Cap",   min=.3, max=1, value=.80, step=.01),
               
               actionBttn("run_bt", "Run Backtest",
                          style = "fill", color = "primary", size = "md",
                          block = TRUE, icon = icon("play"))
             ),
             column(
               width = 9,
               tabsetPanel(
                 tabPanel("Cumulative",
                          br(),
                          plotlyOutput("plt_cum", height = 480),
                          br(),
                          tags$small("Note: weights decided at t are applied to returns at t+1.")
                 ),
                 tabPanel("Performance",
                          br(),
                          DTOutput("tbl_perf"),
                          br(),
                          plotOutput("plt_turn", height = 240)
                 ),
                 tabPanel("Quilt (Discrete Choice)",
                          br(),
                          plotOutput("plt_quilt", height = 200),
                          tags$small("Monthly chosen style under the discrete scheme (after guardrails).")
                 )
               )
             )
           )
  ),
  
  tabPanel("Methodology",
           fluidRow(
             column(
               width = 10, offset = 1,
               br(),
               h4("Signals & Normalization"),
               tags$ul(
                 tags$li(HTML("<b>Macro</b> (market-wide): high value = high risk → tilt Quality/MinVol; low value = low risk → tilt Value. 
                        Standardized to [0,1]; we map to style-specific MacroScore<sub>i</sub> as 
                        MacroScore<sub>Value</sub>=1−Macro, MacroScore<sub>Quality</sub>=Macro, MacroScore<sub>MinVol</sub>=Macro.")),
                 tags$li(HTML("<b>Valuation</b> (market-wide): Earnings Yield = S&P500 EPS / MSCI USA index. 
                        Higher = cheaper → tilt Value. We standardize to [0,1] and map 
                        ValScore<sub>Value</sub>=Val, ValScore<sub>Quality</sub>=1−Val, ValScore<sub>MinVol</sub>=1−Val.")),
                 tags$li(HTML("<b>Momentum</b> (style-specific): trailing 1/3/6/12M (skip-month) cumulative returns, 
                        cross-sectional z-scores each month, then a weighted average to [0,1]."))
               ),
               h4("Composite Score"),
               HTML("For each style i: 
             <code>Comp<sub>i,t</sub> = w<sub>Macro</sub>·MacroScore<sub>i,t</sub> 
             + w<sub>Mom</sub>·MomScore<sub>i,t</sub> 
             + w<sub>Val</sub>·ValScore<sub>i,t</sub></code>."),
               h4("Allocators"),
               tags$ul(
                 tags$li(HTML("<b>Discrete</b>: pick argmax(Comp). Gap rule avoids flips; macro guardrail prevents Value in downtrends.")),
                 tags$li(HTML("<b>Softmax</b>: w<sub>i</sub> ∝ exp(λ·Comp<sub>i</sub>); floors/caps optional.")),
                 tags$li(HTML("<b>Linear</b>: rescale Comp to [0,1] across styles, normalize, apply floors/caps.")),
                 tags$li(HTML("<b>Regime blend</b>: macro defines base weights (High-Risk vs Low-Risk), valuation nudges; momentum optional."))
               ),
               h4("Backtest Discipline"),
               HTML("<b>No look-ahead</b>: weights at t applied to returns at t+1. 
              Momentum horizons use skip-month. 
              For Macro/Valuation normalization, use expanding estimates in production.")
             )
           )
  )
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  # Reactive data: either in-memory dt_model or uploaded
  r_dt <- reactive({
    if (has_dt) return(dt_model)
    f <- input$file_dt
    validate(need(!is.null(f), "Upload dt_model to proceed"))
    ext <- tools::file_ext(f$datapath)
    if (ext == "rds") {
      readRDS(f$datapath)
    } else if (ext == "fst") {
      if (!requireNamespace("fst", quietly = TRUE)) validate("Install 'fst' to load .fst")
      fst::read_fst(f$datapath, as.data.table = TRUE)
    } else if (ext == "csv") {
      fread(f$datapath)
    } else {
      validate("Unsupported file type. Use .rds, .fst, or .csv")
    }
  })
  
  # Run backtest on click
  r_bt <- eventReactive(input$run_bt, {
    dt_in <- copy(r_dt())
    validate(need(all(c("date","msci_usa","msci_usa_value","msci_usa_quality","msci_usa_minvol",
                        "macro_indicator","macro_trend","msci_usa_value_mom",
                        "msci_usa_quality_mom","msci_usa_minvol_mom",
                        "mom_leader","val_score") %in% names(dt_in)),
                  "dt_model is missing required columns."))
    
    backtest_all(
      dt_in,
      w_macro = input$w_macro,
      w_mom   = input$w_mom,
      w_val   = input$w_val,
      gap     = input$gap,
      lambda  = input$lambda,
      floors  = input$floors,
      caps    = input$caps
    )
  }, ignoreInit = TRUE)
  
  # Cumulative plot
  output$plt_cum <- renderPlotly({
    bt <- r_bt()
    validate(need(!is.null(bt), "Click 'Run Backtest'"))
    
    x <- bt$dt
    plt_dt <- data.table(
      date = x$date,
      Discrete = cumprod(1 + fifelse(is.na(x$ret_disc), 0, x$ret_disc)),
      Softmax  = cumprod(1 + fifelse(is.na(x$ret_soft), 0, x$ret_soft)),
      Linear   = cumprod(1 + fifelse(is.na(x$ret_lin),  0, x$ret_lin)),
      Regime   = cumprod(1 + fifelse(is.na(x$ret_reg),  0, x$ret_reg)),
      BuyHold  = cumprod(1 + fifelse(is.na(x$ret_bh),   0, x$ret_bh)),
      EqWeight = cumprod(1 + fifelse(is.na(x$ret_eq),   0, x$ret_eq))
    )
    m <- melt(plt_dt, id.vars = "date", variable.name = "Strategy", value.name = "Cum")
    p <- ggplot(m, aes(date, Cum, color = Strategy)) +
      geom_line(linewidth = 0.7) +
      labs(title = "Cumulative Return (×)", x = NULL, y = NULL) +
      theme_min
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Performance table
  output$tbl_perf <- renderDT({
    bt <- r_bt()
    validate(need(!is.null(bt), "Click 'Run Backtest'"))
    perf <- copy(bt$perf)
    numcols <- c("CAGR","Vol","Sharpe","MaxDD","Turnover")
    for (cl in intersect(numcols, names(perf))) {
      if (cl %in% c("CAGR","Vol","MaxDD","Turnover")) {
        perf[[cl]] <- scales::percent(perf[[cl]], accuracy = 0.1)
      } else if (cl == "Sharpe") {
        perf[[cl]] <- round(perf[[cl]], 2)
      }
    }
    datatable(perf, rownames = FALSE, options = list(dom = "tp", pageLength = 10))
  })
  
  # Turnover bar
  output$plt_turn <- renderPlot({
    bt <- r_bt()
    validate(need(!is.null(bt), "Click 'Run Backtest'"))
    perf <- bt$perf[Strategy %in% c("Discrete","Softmax","Linear","Regime")]
    ggplot(perf, aes(x = Strategy, y = as.numeric(sub("%","",Turnover))/100)) +
      geom_col(fill = "#2C3E50") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Average Monthly Turnover", x = NULL, y = NULL) +
      theme_min
  })
  
  # Quilt (discrete choice)
  output$plt_quilt <- renderPlot({
    bt <- r_bt()
    validate(need(!is.null(bt), "Click 'Run Backtest'"))
    x <- bt$dt
    # Derive chosen style from unlagged discrete weights
    pick <- x[, .(date,
                  pick = fifelse(w_disc_Value==1,"Value",
                                 fifelse(w_disc_Quality==1,"Quality",
                                         fifelse(w_disc_MinVol==1,"MinVol", NA_character_))))]
    ggplot(pick, aes(x = date, y = 1, fill = pick)) +
      geom_tile() +
      scale_fill_manual(values = c(Value="#1F78B4", Quality="#33A02C", MinVol="#FB9A99"), na.value = "grey80") +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)

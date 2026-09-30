# =====================================================================
# Topic: Forecasting with Moving Averages — MA(N)
# Nahmias, Chapter 2
#
# MA(N) forecast:  F_t = (D_{t-1} + D_{t-2} + ... + D_{t-N}) / N
# Forecast error:  e_t = F_t - D_t
# MAD  = mean of |e_t|
# MSE  = mean of e_t^2
# MAPE = mean of |e_t / D_t| x 100
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS (the calculations; independent of the screen)
# ---------------------------------------------------------------------

# Computes MA(N) forecasts for the demand vector d.
# The output has length n + 1: the last element is the next-period forecast.
# No forecast can be made for the first N periods (NA).
moving_average_forecast <- function(d, N) {
  n <- length(d)
  F <- rep(NA_real_, n + 1)
  for (t in (N + 1):(n + 1)) {
    F[t] <- mean(d[(t - N):(t - 1)])
  }
  F
}

# Computes MAD, MSE and MAPE over periods start ... n.
moving_average_errors <- function(d, F, start) {
  t <- start:length(d)
  e <- F[t] - d[t]
  c(
    MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = mean(abs(e / d[t])[d[t] != 0]) * 100
  )
}

# Tries every N = 1, 2, ..., Nmax.
# For a fair comparison, all N are evaluated over the SAME periods (Nmax+1 ... n).
moving_average_search <- function(d, Nmax) {
  result <- lapply(1:Nmax, function(N) {
    F <- moving_average_forecast(d, N)
    c(N = N, moving_average_errors(d, F, start = Nmax + 1))
  })
  as.data.frame(do.call(rbind, result))
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = ".xlsx"),
      selectInput(ns("column"), "Demand column", choices = NULL),
      radioButtons(ns("mode"), "How should N be chosen?",
                   choices = c("I will choose N" = "manual",
                               "Find the best N" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "N (number of past periods)", value = 3, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        numericInput(ns("Nmax"), "Largest N to try", value = 12, min = 2, step = 1),
        selectInput(ns("criterion"), "Choose the best N by",
                    choices = c("MAD", "MSE", "MAPE"))
      )
    ),
    mainPanel(
      h4(textOutput(ns("title"))),
      plotOutput(ns("forecast_plot"), height = "350px"),
      h4("Error measures"),
      tableOutput(ns("error_table")),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Comparison of all N values"),
        plotOutput(ns("n_plot"), height = "250px"),
        tableOutput(ns("n_table"))
      ),
      h4("Forecasts by period"),
      tableOutput(ns("period_table"))
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # --- Read the Excel file ------------------------------------------
    data <- reactive({
      req(input$file)
      readxl::read_excel(input$file$datapath)
    })
    
    # When a file is uploaded, list its numeric columns
    observeEvent(data(), {
      numeric_cols <- names(data())[sapply(data(), is.numeric)]
      updateSelectInput(session, "column", choices = numeric_cols,
                        selected = tail(numeric_cols, 1))
    })
    
    # Demand data in the selected column
    demand <- reactive({
      req(input$column)
      d <- data()[[input$column]]
      d[!is.na(d)]
    })
    
    # --- Auto mode: try every N ---------------------------------------
    search <- reactive({
      req(input$mode == "auto", input$Nmax)
      d <- demand()
      validate(need(input$Nmax < length(d),
                    "The largest N must be smaller than the number of observations."))
      moving_average_search(d, input$Nmax)
    })
    
    # --- The N to use -------------------------------------------------
    chosen_N <- reactive({
      if (input$mode == "manual") {
        req(input$N)
        validate(need(input$N < length(demand()),
                      "N must be smaller than the number of observations."))
        input$N
      } else {
        tab <- search()
        tab$N[which.min(tab[[input$criterion]])]
      }
    })
    
    # --- Forecasts and errors with the chosen N -----------------------
    result <- reactive({
      d <- demand()
      N <- chosen_N()
      F <- moving_average_forecast(d, N)
      start <- if (input$mode == "manual") N + 1 else input$Nmax + 1
      list(d = d, N = N, F = F,
           errors = moving_average_errors(d, F, start),
           start = start)
    })
    
    # --- Outputs ------------------------------------------------------
    output$title <- renderText({
      r <- result()
      paste0("MA(", r$N, ")  —  next-period forecast: ",
             round(tail(r$F, 1), 2))
    })
    
    output$forecast_plot <- renderPlot({
      r <- result()
      n <- length(r$d)
      plot(1:n, r$d, type = "o", pch = 19, col = "grey30",
           xlim = c(1, n + 1), ylim = range(c(r$d, r$F), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and MA(", r$N, ") forecast"))
      lines(1:(n + 1), r$F, type = "o", pch = 17, lty = 2, col = "#2F6FB0")
      points(n + 1, tail(r$F, 1), pch = 17, cex = 2, col = "#B4501E")
      legend("topleft", bty = "n",
             legend = c("Actual demand", "Forecast", "Next-period forecast"),
             col = c("grey30", "#2F6FB0", "#B4501E"),
             pch = c(19, 17, 17), lty = c(1, 2, NA))
    })
    
    output$error_table <- renderTable({
      r <- result()
      data.frame(
        "Measure" = c("MAD", "MSE", "MAPE (%)"),
        "Value" = round(unname(r$errors), 3),
        "Periods evaluated" = paste0(r$start, "–", length(r$d)),
        check.names = FALSE
      )
    })
    
    output$n_plot <- renderPlot({
      tab <- search()
      k <- input$criterion
      col <- ifelse(tab$N == chosen_N(), "#B4501E", "#9DB4CF")
      barplot(tab[[k]], names.arg = tab$N, col = col, border = NA,
              xlab = "N", ylab = k,
              main = paste0("Best N by ", k, " = ", chosen_N()))
    })
    
    output$n_table <- renderTable({
      tab <- search()
      tab$MAD  <- round(tab$MAD, 3)
      tab$MSE  <- round(tab$MSE, 3)
      tab$MAPE <- round(tab$MAPE, 3)
      tab$N <- as.integer(tab$N)
      tab
    })
    
    output$period_table <- renderTable({
      r <- result()
      n <- length(r$d)
      data.frame(
        "Period" = 1:(n + 1),
        "Demand (D)" = c(r$d, NA),
        "Forecast (F)" = round(r$F, 2),
        "Error (e = F - D)" = round(r$F - c(r$d, NA), 2),
        check.names = FALSE
      )
    }, na = "")
  })
}
learning_curve_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("L"), "Learning rate", 0.6, 0.99, 0.8),
    plotOutput(ns("plot"))
  )
}

learning_curve_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      b <- -log(input$L) / log(2)
      u <- 1:50
      plot(u, 100 * u^(-b), type = "l", xlab = "Unit", ylab = "Time")
    })
  })
}
ogrenme_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("L"), "Öğrenme oranı", 0.6, 0.99, 0.8),
    plotOutput(ns("grafik"))
  )
}

ogrenme_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$grafik <- renderPlot({
      b <- -log(input$L) / log(2)
      u <- 1:50
      plot(u, 100 * u^(-b), type = "l", xlab = "Birim", ylab = "Süre")
    })
  })
}

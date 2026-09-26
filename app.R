library(shiny)

ui <- navbarPage("Üretim Planlama",
  tabPanel("Öğrenme Eğrisi", ogrenme_ui("ogrenme"))
)

server <- function(input, output, session) {
  ogrenme_server("ogrenme")
}

shinyApp(ui, server)

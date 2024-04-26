library(shiny)

ui <- fluidPage(
  titlePanel("Minimal Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Choose a number", 1, 100, 50)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$slider))
  })
}

shinyApp(ui = ui, server = server)

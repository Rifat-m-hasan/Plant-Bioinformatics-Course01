library(shiny)

HistogramPlot <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose your nummber",
              min = 10,
              max = 500,
              value = 50),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

InnerFunction <- function(input, output) {
  data <- reactive({
    rnorm(input$num)
  })
  
  output$hist <- renderPlot({
    hist(data())
  })
  
  output$stats <- renderPrint({
    summary(data())
  })
}

shinyApp(ui = HistogramPlot, server = InnerFunction)

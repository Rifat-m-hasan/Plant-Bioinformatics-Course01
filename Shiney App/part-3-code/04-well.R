# 04-well.R

library(shiny)

ui <- fluidPage(
  wellPanel(
    sliderInput(inputId = "num",
                label = "Choose a number", 
                value = 25, 
                min = 1, 
                max = 100),
    textInput(inputId = "title", 
              label = "Write a title",
              value = "Histogram of Random Normal Values")
  ),
  
  plotOutput("hist")
)

server <- function(input, output) {
  data <- reactive({
    rnorm(input$num)
  })
  
  output$hist <- renderPlot({
    hist(data(), main = input$title)
  })
}

shinyApp(ui = ui, server = server)
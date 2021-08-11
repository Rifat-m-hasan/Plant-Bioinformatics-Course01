# 05-tabs.R

library(shiny)

ui <- fluidPage(title = "Random generator",
  tabsetPanel(              
    tabPanel(title = "Normal data",
             sliderInput(inputId = "num",
                         label = "Choose a number",
                         value = 25,
                         min = 1, 
                         max = 100),
             plotOutput("hist"),
             actionButton(inputId = "go", 
                          label = "Update"))
  )
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
    rnorm(input$num) 
  })
  
  output$hist <- renderPlot({
    hist(data())
  })
  
}

shinyApp(server = server, ui = ui)

















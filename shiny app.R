library(shiny)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_kodas", label = "Imones Kodas", choices = NULL, selected = NULL)
    ),
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentele", tableOutput("table"))
    )
    )
  )
)
server <- function(input, output, session) {
  data <- lab
  updateSelectizeInput(session, "imones_kodas", choices = data$code, server = TRUE)
  
  output$table <- renderTable(
    data %>%
      filter(code == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(code == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage)) +
      geom_line()      
  )
}
shinyApp(ui, server)
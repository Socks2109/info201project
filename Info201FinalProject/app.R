#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

vgsales <- read_delim("vgsales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Game data"),
  navbarPage("",
    # Alexis's part
    tabPanel("Intro",
             h2("Introduction")
    ),
    tabPanel("Country sales vs genre",
             sidebarLayout(
               sidebarPanel(
                 p("This graph illustrates the relationship between each country's sales vs the genre"),
                 br(),
                 p("")
               ), mainPanel(
                 
               )
             )
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sales <- renderPlot({
    vgsales %>%
      filter(mag <= input$max) %>%
      filter(mag >= input$min) %>%
      filter(magType%in%input$type) %>%
      ggplot(aes(x = depth, y= mag)) +
      geom_point(col = input$color) +
      labs(x = "Depth of event (km)", y = "Magnitude of earthquake")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

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
                 p("This graph on the right illustrates the relationship between each country's total sales (in millions) across all year against the genre"),
                 br(),
                 radioButtons("country",
                              "Choose which country you want to view",
                              choices = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
                 ),
                 br(),
                 p("As we transition through the graph, we observe that all of the listed countries have Action and Sports as their top genres. However, Japan is the only country that has Role-Playing as their top genre instead.")
               ), mainPanel(
                 plotOutput("sales"),
                 br(),
                 textOutput("sales_text")
               )
             )
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sales <- renderPlot({
    vgsales %>%
      group_by(Genre) %>%
      reframe(Selected_country_sales = sum(eval(as.symbol(input$country)))) %>%
      ggplot(aes(x = Genre, y = Selected_country_sales)) +
      geom_col(position = "dodge", fill = "#56B1F7", color = "black") +
      labs(x = "Genre", y = paste("Total", input$country, "(in millions)"))
  })
  output$sales_text <- renderText({
    max_val <- vgsales %>%
      group_by(Genre) %>%
      reframe(Selected_country_sales = sum(eval(as.symbol(input$country)))) %>%
      arrange(desc(Selected_country_sales)) %>%
      head(1)
    paste("The top selling genre is", max_val$Genre, "with", max_val$Selected_country_sales, "million dollars")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

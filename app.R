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
library(shinythemes)

vgsales <- read_delim("vgsales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Video Game Data"),
  navbarPage("",
             tabPanel("Intro",
                      mainPanel(
                        img(
                          src = "games.JPG",
                          alt = "Game Pics",
                          width = 500, height = 350
                        )
                      ),
                      h2("Background"),
                      p("In just the past century, the world has seen massive technological advancements, not just on a global scale but on a consumer level as well. While technology increased productivity and communication across the planet, it also became highly integrated in people's personal lives. The 1980s saw the popularization of video games which has since cemented itself in the everyday life of people across the planet. Today, global video game sales settle in the hundred-millions yearly."),
                      h2("Data"),
                      p("The dataset we used focuses on video games since the 1980s. It contains data on", nrow(vgsales), "individual games and includes relevant information like year, platform, genre, publisher, and sales (both global and regional)."),
                      h2("Intentions"),
                      p("For the purposes of this project, the data will be used to display and summarize trends over time and across genre. Our goal is to provide a visual representation of these trends both in a general informative context but also as a potential marketing resource as we consider what factors may display the longest and more profitable trends."),
                      h2("Research Questions"),
                      p("- What platforms are most popular in a given year, and how long do they stay popular?"),
                      p("- Which genres are popular in a given year, and what trends can be associated with their popularity?"),
                      p("- What does the data look like isolated by region?"),
                      h3("Data Sample"),
                      mainPanel(
                        tableOutput("sample")
                      )
             ),
             tabPanel("Country Sales by Genre",
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
             tabPanel("Sales by Genre", 
                      sidebarLayout(
                        sidebarPanel(titlePanel("Video Game Sales by Genre over Time"), 
                                     p("This page allows you to pick a starting a ending year, select
                    different video game genres, and then be able to see a graph that
                    shows you how the global sales compare between your chosen genres
                    and years."),
                                     checkboxGroupInput("genres","Choose Genres", choices = unique(vgsales$Genre)),
                                     sliderInput("min", "Choose Starting Year", min = 1980, 
                                                 max = 2020, value = 1980, step = 1),
                                     sliderInput("max", "Choose Ending Year", min = 1980, 
                                                 max = 2020, value = 2020, step = 1)
                        ),
                        mainPanel(
                          plotOutput("genreSalesPlot"),
                          textOutput("genreSalesPlotDescription")
                        )
                      )
             ),
             tabPanel("Sales by Platform",
                      sidebarLayout(
                        sidebarPanel(p("This graph shows the global sales of the assigned platform under the years when there were releases. This graph provides information about the trend and popularity of the platform.
          Users are able to explore the platform as they wish!"),
                          selectInput("videogame_platforms", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                                            "PC", "NES", "GB", "GBA", "DS", "X360", "SNES",
                                                                                            "3DS", "N64", "XB", "2600", "GEN", "DC",
                                                                                            "PSP", "XOne", "WiiU", "GC", "SAT", "SCD", "WS",
                                                                                            "NG", "TG16", "3DO", "GG", "PCFX"))),
                        mainPanel(
                          plotOutput("videogame_platforms_plot"),
                          textOutput("platform_text")
                        )
                      )
             ),
             tabPanel("Summary",
                      p("Video games, while an extremely profitable industry, are rapidly changing and variable entities. Platforms in particular do not seem to have very long lifespans. Looking at the graphs, most hit their peak of sales in their first year or two but fall rapidly after. Some consoles don't even make it past their first year. Even the most popular consoles like the Wii and Playstation products see sales dip shortly after release. This is less of a surprise considering the rate that large companies are capable of putting out a new console."),
                      p("Genre is more interesting to look at. We see genre stay relatively static both globally and across continents. Action games remain the most popular everywhere except Japan. Japan's highest charting sales are in role-playing games with action games coming in second but still far behind. Genre sales by time give us a look at what genres are popular at what time. Again, action games dominate the market. With the time factor, we can also see what genres are emerging at the time. For example, there are no games under the adventure genre until 1983, and strategy games do not enter the market until 1992."))
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
  output$genreSalesPlot <- renderPlot({
    vgsales %>% 
      filter(!is.na(Global_Sales),!is.na(Year)) %>%
      filter(Genre%in%input$genres) %>%
      filter(Year >= input$min) %>% 
      filter(Year <= input$max) %>% 
      ggplot(aes(x = Genre, y = Global_Sales, fill = factor(Genre))) +
      geom_col() +
      labs(title = "Global Sales by Genre", x = "Genres", y = "Global Sales in Millions")
  })
  output$genreSalesPlotDescription <- renderText({
    paste("The above bar graph shows data between the years of", input$min, " and ", input$max, 
          ". The following selected genres are represented: ", paste(input$genres, collapse=", "))
  })
  output$videogame_platforms_plot <- renderPlot({
    vgsales %>% 
      select(Global_Sales,
             Year,
             Platform) %>% 
      filter(Platform %in% input$videogame_platforms,
             Global_Sales != "N/A", Year != "N/A", Platform != "N/A") %>%
      group_by(Year) %>%
      summarize(global_sales_in_that_year = mean(Global_Sales)) %>% 
      ggplot(aes(x = Year, y = global_sales_in_that_year)) +
      geom_col(fill="#56B1F7", color="black") +
      labs(x = "Year", y = "Sales in Million")
  })
  output$platform_text <- renderText({
    paste("As users compare the sales, it can be seen that Playstation has been particularly consistent with its sales")
  })
  output$sample <- renderTable({
    vgsales %>%
      select(Name, Platform, Year, Global_Sales) %>%
      sample_n(10)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

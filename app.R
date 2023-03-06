library(shiny)
library(shinythemes)
library(tidyverse)
video_game <- read_delim("../vgsales.csv")

##UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  ##Application title
  titlePanel("Video Game Trends Overtime"),
  
  ##Navigation Page
  navbarPage("", 
             ##Introduction
             tabPanel("Introduction",
                      h2(p("This is the ", em("ps6"), ", which is a demo for my ", strong("final project."), 
                           "This data is from Kaggle contains information about video games, such as names, platforms,
                           sales, etc. I am going to use the platform, sales and year columns. Here is a small sample
                           of the data:"))),
             
             ##Drop-down menu
             tabPanel("Platform Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("videogame_platforms_plot", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                                           "PC", "NES", "GB", "GBA", "DS", "x360", "SNES",
                                                                                           "3DS", "N64", "XB", "2600", "GEN", "DC",
                                                                                           "PSP", "XOne", "WiiU", "GC", "SAT", "SCD", "WS",
                                                                                            "NG", "TG16", "3DO", "GG", "PCFX"))),
                                                                                          
                        mainPanel(
                          plotOutput("videogame_platforms_plot")
                        )
                      )
             ),
             
             tabPanel("Platform Table",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("videogame_platforms_table", "Select the platform", c("Wii", "PS", "PS2", "PS3", "PS4", "PSV",
                                                                                            "PC", "NES", "GB", "GBA", "DS", "x360", "SNES",
                                                                                            "3DS", "N64", "XB", "2600", "GEN", "DC",
                                                                                            "PSP", "XOne", "WiiU", "GC", "SAT", "SCD", "WS",
                                                                                            "NG", "TG16", "3DO", "GG", "PCFX"))),
                        mainPanel(
                          tableOutput("videogame_platforms_table")
                        )
                      )
             )
  )
)



##Server
server <- function(input, output) {
  output$videogame_platforms_plot <- renderPlot({
    video_game %>% 
      select(Global_Sales,
             Year,
             Platform) %>% 
      filter(Platform %in% input$videogame_platforms_plot,
             Global_Sales != "N/A", Year != "N/A", Platform != "N/A") %>%
      .groups(Platform, Year) %>% 
      summarize(global_sales_in_that_year = mean(Global_Sales)) %>% 
      ggplot(aes(x = Year, y = global_sales_in_that_year)) +
      geom_col(fill="violet", col="blue") +
      labs(x = "Year", y = "Sales in Million")
  })
  output$videogame_platforms_table <- renderTable({
    video_game %>% 
      select(Global_Sales,
             Year,
             Platform) %>% 
      filter(Platform %in% input$videogame_platforms_table,
             Global_Sales != "N/A", Year != "N/A", Platform != "N/A") %>%
      .groups(Platform, Year) %>% 
      summarize(global_sales_in_that_year = mean(Global_Sales))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

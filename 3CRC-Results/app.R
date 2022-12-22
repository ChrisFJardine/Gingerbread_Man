#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
#source('DataLoad.R')
#df <- read.csv('./2022_23_Results.csv')
pointsdf <- df[df$Run.Date == '2022-11-23',]
pointsdf <-  pointsdf[order(pointsdf$Total.Points),]
runnerList <- unique(df$Athlete.Name)

draw_bar_chart <- function(athlete_filter) {
  filtered_df <-  df %>%
    filter(Athlete.Name == !!athlete_filter)
  ggplot(filtered_df, aes(y = reorder(Athlete.Name, Total.Points), x = Total.Points )) +
    geom_col()
  
}
# Define UI for application that draws a histogram
ui <- fluidPage(
    #titlePanel("Coalfields Cross Country Running Club - Results"),
        inputPanel(
            selectInput(
              "Athlete",
              label = "Select Athlete Name",
              choices = runnerList
            )
        ),
        plotOutput("minPlot")  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$minplot <- renderPlot(draw_bar_chart(input$Athlete))
}

# Run the application 
shinyApp(ui = ui, server = server)

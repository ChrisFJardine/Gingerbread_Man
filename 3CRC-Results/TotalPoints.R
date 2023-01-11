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
library(gghighlight)
library(ggpubr)
library(jpeg)
library(ggimage)
library(png)
library(grid)
#source('DataLoad.R')
#df <- read.csv('./2022_23_Results.csv', stringsAsFactors = F)
df <- df %>% rename ('AvgPaceS' = Avg.Pace..s.,
                     'RunTimeS' = Run.Time..s.,
                     'ClockTimeS' = Clock.Time..s.,
                     'HandicapS' = Handicap..s.)
df <- df[df$Avg.Pace != "0:00",]

best = df %>% 
  group_by( Run.Date, Run.Length)%>%
  summarise(bestTotalPoints = max(Total.Points),
            bestPoints = max(Points), 
            bestPaceS = min(AvgPaceS))
df = merge(df, best)
df$divisionLeader = 0
df <- df %>% mutate(divisionLeader = replace(divisionLeader, Total.Points == bestTotalPoints , 1 ))

runnerList <- unique(df$Athlete.Name[order(df$Athlete.Name)])
lengthList <- unique(df$Run.Length)
dateList <- unique(df$Run.Date[order(df$Run.Date)])


pointscore_plot <- function(athlete_filter, length_filter, date_filter) {
  filtered_df <-  df %>%
    filter((Run.Length == !!length_filter | Run.ClassID == 'g1-3') & Run.Date == !!date_filter )
  ggplot(filtered_df, aes(x = Total.Points, 
                          y = reorder(Athlete.Name, Total.Points),
                          fill = ifelse(Athlete.Name == !!athlete_filter, 'Highlighted','Normal'))) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
     geom_col() +
    theme(legend.position = "none", axis.title.y = element_blank()) 
}
pace_plot <- function(athlete_filter, length_filter) {
  filtered_df <-  df %>%
    filter(Athlete.Name == !!athlete_filter & Run.Length == !!length_filter  )
  ggplot(filtered_df) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_line(aes(x = Run.Date, y = AvgPaceS ), size = 1.7 , group = 1, color = 'blue') +
    geom_line(aes(x = Run.Date, y = bestPaceS ), size = 1.7, group = 1, color = 'slategray2') +
    ylab('Avg Pace (s) / Best Pace') + xlab ('Run Date') +
    theme(axis.text.x = element_text(angle = 45))
}
runpoints_plot <- function(athlete_filter, length_filter, date_filter) {
  filtered_df <-  df %>%
    filter((Run.Length == !!length_filter | Run.ClassID == 'g1-3') & Run.Date == !!date_filter )
  ggplot(filtered_df, aes(x = Points ,
                          y = reorder(Athlete.Name, Points),
                          fill = ifelse(Athlete.Name == !!athlete_filter, 'Highlighted','Normal'))) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_col() +
    theme(legend.position = "none", axis.title.y = element_blank()) 
}
cumpoints_plot <- function(athlete_filter, length_filter) {
  filtered_df <-  df %>%
    filter( Run.Length == !!length_filter  ) %>%
    mutate(Athlete.Name <- as.factor(Athlete.Name)) %>%
    select(Athlete.Name, Run.Date, Total.Points)
  
  ggplot(filtered_df, aes(x = Run.Date, 
                          y = Total.Points, 
                                                    )) +
    annotation_custom(rasterGrob(img, 
                                 width = unit(1,"npc"), 
                                 height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    geom_line(aes(color = Athlete.Name),group = 'Run.Date', size = 1.0) +
    gghighlight(Athlete.Name == !!athlete_filter) +
    ylab('Total Points') + xlab ('Run Date') +
    theme(axis.text.x = element_text(angle = 45))
}

# Define UI for application that draws a histogram
ui <- fluidPage(    
    titlePanel("Coalfields Cross Country Running Club - Results"),
    sidebarLayout(
      sidebarPanel(
        column(width = 10,

               selectInput(
                 "Athlete",
                 label = "Select Athlete Name",
                 choices = runnerList,
                 selected = 'Chris Jardine'
               )
        ),
        column(width = 10, 
               selectInput(
                 "Length",
                 label = "Select Run Length",
                 choices = lengthList,
                 selected = 'Long'
               )
        ),
        column(width = 10, 
               selectInput(
                 "Date",
                 label = "Select Run Date",
                 choices = dateList,
                 selected = max(df$Run.Date)
               )
        ),
        
      ),
        
    mainPanel(
      tabsetPanel(
        tabPanel("Pointscore", plotOutput("minplot")), 
        tabPanel("Run Points", plotOutput("runpointsplot")),
        tabPanel("Total Points", plotOutput("cumpointsplot")),
        tabPanel("Pace", plotOutput("paceplot")) 

      )
    )
  )
)

server <- function(input, output) {
  output$minplot <- renderPlot(pointscore_plot(input$Athlete, input$Length, input$Date))
  output$paceplot <- renderPlot(pace_plot(input$Athlete, input$Length))
  output$runpointsplot <- renderPlot(runpoints_plot(input$Athlete, input$Length, input$Date))
  output$cumpointsplot <- renderPlot(cumpoints_plot(input$Athlete, input$Length))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

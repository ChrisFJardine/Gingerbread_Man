library(dplyr)
library(ggplot2)
library(shiny)

wxdata <- data.frame(
  City = rep(c("Abilene", "Amarillo", "Omaha"), each = 4), 
  Temp = c(12, 15, 16, 17, 32, 34, 36, 37, 8, 6, 3, 4)
)
citylist <- unique(wxdata$City)

draw_plot <- function(city_to_filter_by) {
  filtered_wx <- wxdata %>%
    filter(City == !!city_to_filter_by)
  ggplot(filtered_wx, aes(Temp)) +
    geom_histogram()
}

ui <- fluidPage(
  inputPanel(
    selectInput(
      "PlotCity",
      label = "Select City",
      choices = citylist
    )
  ),
  plotOutput('minplot')
)

server <- function(input, output) {
  output$minplot <- renderPlot(draw_plot(input$PlotCity))

}

shinyApp(ui = ui, server = server)
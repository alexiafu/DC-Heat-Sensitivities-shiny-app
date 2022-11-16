#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

water <- read.csv("final_data.csv")
write.csv(Country_Name, file="../data/Country_Name.csv", row.names = FALSE)
Country_Name <- read_csv("Country_Name.csv")

water %>%
  select(c("City_ID", "Country_Name")) ->Country_Name






# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("water sources.app"),
  varSelectInput("var1", "X variable", data =Country_Name, selected = "Country_Name"),
)
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(water, aes(x = !!(input$var1), y = !!(input$var2), color = !!(input$var3))) +
      geom_point()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

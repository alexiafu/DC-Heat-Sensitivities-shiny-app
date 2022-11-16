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

heat_DC <- read.csv("Heat_Sensitivity_Exposure_Index.csv")

# Define UI for application that draws a histogram
ui2 <- fluidPage(
  titlePanel("Heat_Sensitivity_Exposure.app"),
  varSelectInput("var1", "X variable", data = heat_DC, selected = "TOTALPOP"),
  varSelectInput("var2", "Y variable", data=heat_DC, selected = "HSEI"),
  varSelectInput("var3", "Coloer variable (categorical)", data = heat_DC, selected = "OBESITY"),
  plotOutput("plot")
)
server2 <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(heat_DC, aes(x = !!(input$var1), y = !!(input$var2), color = !!(input$var3))) +
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui2, server = server2)



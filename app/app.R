library(shiny)
library(bslib)
library(shiny)
library(ggplot2)
library(tidyverse)
heat_DC <- read.csv("Heat_Sensitivity_Exposure_Index.csv")

# User Interface Code
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("DC Heat Sensitivity Exposure", windowTitle = "DC Heat Sensitivity Exposure"),
  
  mainPanel(
    tabsetPanel(
      type = "pills",
      tabPanel("Graphical Analysis",
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               )
      ), # End tabPanel1
      tabPanel("Statistical Modeling",
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel())
               ), # End tabPanel2
      tabPanel("Working Data",
               dataTableOutput("dynamic"))
    ) # End tabsetPanel
  ),# End mainPanel
  varSelectInput("var1", "X variable", data = heat_DC, selected = "TOTALPOP"),
  varSelectInput("var2", "Y variable", data=heat_DC, selected = "HEI"),
  varSelectInput("var3", "Coloer variable (categorical)", data = heat_DC, selected = "OBESITY"),
  plotOutput("plot")
) # End fluidPage


# Server Code
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(heat_DC, aes(x = !!(input$var1), y = !!(input$var2), color = !!(input$var3))) +
    geom_boxplot()
  })}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(shiny)
library(ggplot2)
library(tidyverse)
heat_DC <- read_csv("../data/DC_Heat_Island.csv", show_col_types = F)
heat_DC <- heat_DC %>%
  mutate(majority_minority = case_when(P_POC > 50 ~ "TRUE",
                                       TRUE ~ "FALSE")
         )

# User Interface Code
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("DC Heat Sensitivity Exposure", windowTitle = "DC Heat Sensitivity Exposure"),
  
  mainPanel(
    tabsetPanel(
      type = "pills",
      tabPanel("Bivariate Analysis",
               fluidRow(
                 column(6, 
                        varSelectInput("var1", "X variable", data = heat_DC, selected = "TOTALPOP"),
                        varSelectInput("var2", "Y variable", data=heat_DC, selected = "HEI"),
                        varSelectInput("var3", "Color variable (categorical)", data = heat_DC, selected = "majority_minority")
                        ),
                column(
                  6, 
                  sliderInput("bins", "Number of Bins",
                              min = 1, max = 50, value = 30)
                )
              ),
            fluidRow(
              column(6, plotOutput("plot1")),
              column(3, plotOutput("plot2")),
              column(3, plotOutput("plot3"))
            )
      ), #End tabPanel
      tabPanel("Mapping",
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
) # End fluidPage


# Server Code
server <- function(input, output) {
  output$plot1 <- renderPlot({
    if (is.numeric(heat_DC[[input$var3]])) {
    ggplot(heat_DC, aes(x = !!(input$var1), y = !!(input$var2))) +
    geom_point() +
    ggtitle("Please Select Categorical Variable")
  } else {
    ggplot(heat_DC, aes(x = !!(input$var1), y = !!(input$var2), color = !!(input$var3))) +
      geom_point()
  }
  })
  
  output$plot2 <- renderPlot({
    ggplot(heat_DC, aes(x = !!(input$var1))) +
      geom_histogram(bins = input$bins)
  })
  
  output$plot3 <- renderPlot({
    ggplot(heat_DC, aes(x = !!(input$var2))) +
      geom_histogram(bins = input$bins)
  })
    
  output$dynamic <- renderDataTable({
    heat_DC
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

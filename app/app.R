library(shiny)
library(bslib)
library(shiny)
library(ggplot2)
library(tidyverse)
heat_DC <- read_csv("../data/DC_Heat_Island.csv", show_col_types = F)
heat_DC <- heat_DC %>%
  mutate(majority_minority = case_when(P_POC > 50 ~ "TRUE",
                                       TRUE ~ "FALSE")) %>%
  rename(med_income = estimate) %>%
  select(-geometry)

heat_bivariate <- heat_DC %>%
  select(-OBJECTID:-ID, -GIS_ID:-variable, -moe)

# User Interface Code
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("DC Heat Sensitivity and Exposure", windowTitle = "DC Heat Sensitivity and Exposure"),
  
  mainPanel(
    tabsetPanel(
      type = "pills",
      tabPanel("Bivariate Analysis",
               fluidRow(
                 column(6, 
                        varSelectInput("var1", "X variable", data = heat_bivariate, selected = "TOTALPOP"),
                        varSelectInput("var2", "Y variable", data=heat_bivariate, selected = "HEI"),
                        varSelectInput("var3", "Color variable (categorical)", data = heat_bivariate, selected = "majority_minority")
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
               checkboxInput("limit_id", "Remove Duplicate ID Variables?"),
               checkboxInput("hsi_only", "HSI Data Only"),
               checkboxInput("hei_only", "HEI Data Only"),
               dataTableOutput("dynamic"))
    ) # End tabsetPanel
  ),# End mainPanel
) # End fluidPage


# Server Code
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    if (is.numeric(heat_DC[[input$var3]])) {
    ggplot(heat_DC, aes(x = !!input$var1, y = !!input$var2)) +
    geom_point() +
    ggtitle("Please Select Categorical Variable")
  } else {
    ggplot(heat_DC, aes(x = !!input$var1, y = !!input$var2, color = !!input$var3)) +
      geom_point()
  }
  })
  
  output$plot2 <- renderPlot({
    ggplot(heat_DC, aes(x = !!input$var1)) +
      geom_histogram(bins = input$bins)
  })
  
  output$plot3 <- renderPlot({
    ggplot(heat_DC, aes(x = !!input$var2)) +
      geom_histogram(bins = input$bins)
  })
    
  output$dynamic <- renderDataTable({
    if (input$limit_id == TRUE) { 
      if (input$hsi_only == TRUE) {
              heat_DC <- heat_DC %>%
                select(-OBJECTID, -ID2:-ID, -P_TREECOVER:-GEOID)
              heat_DC
      } else if (input$hei_only == TRUE) {
            heat_DC <- heat_DC %>%
              select(-OBJECTID, -ID2:-HSI, -variable:-majority_minority)
            heat_DC
      } else {
        heat_DC <- heat_DC %>%
          select(-OBJECTID, -ID2, -GEO_ID, -NAME, -ID, -GIS_ID:-variable)
        heat_DC
      }
    } else if (input$hsi_only == TRUE){
        heat_DC <- heat_DC %>%
         select(-P_TREECOVER:-GEOID)
        heat_DC
    } else if (input$hei_only == TRUE) {
        heat_DC <- heat_DC %>%
         select(-TOTALPOP:-HSI, -variable:-majority_minority)
        heat_DC
    } else {
      heat_DC
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

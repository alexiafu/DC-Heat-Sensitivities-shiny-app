library(shiny)
library(bslib)
library(shiny)
library(ggplot2)
library(tidyverse)
heat_DC <- read_csv("../data/DC_Heat_Island.csv", show_col_types = F)

stat_heat_DC <- heat_DC %>%
  rename(med_income = estimate) %>%
  select(-geometry,-OBJECTID:-ID, -GIS_ID:-variable, -moe)

stat_heat_DC <- data.frame(stat_heat_DC)

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
                 sidebarPanel(
                   selectInput("outcome", label = h3("Outcome"),
                               choices = list("TOTALPOP" = "TOTALPOP",
                                              "P_POC" = "P_POC",
                                              "P_CHILD" = "P_CHILD",
                                              "P_ELDERLY" = "P_ELDERLY", 
                                              "P_POVERTY"= "P_POVERTY", 
                                              "P_DISABILITY"="P_DISABILITY", 
                                              "P_LIMENG"="P_LIMENG", 
                                              "ASTHMA" ="ASTHMA ", 
                                              "CHD"="CHD", 
                                              "OBESITY"="OBESITY", 
                                              "P_TREECOVER"="P_TREECOVER", 
                                              "P_NOTREE"= "P_NOTREE", 
                                              "P_IMPSURF"= "P_IMPSURF", 
                                              "AIRTEMP_MEAN"="AIRTEMP_MEAN",
                                              "HSI" = "HSI",
                                              "HEI"="HEI", 
                                              "HSEI"="HSEI",
                                              "med_income"="med_income"), selected = 1),
                   
                   selectInput("indepvar", label = h3("Explanatory variable"),
                               choices = list("TOTALPOP" = "TOTALPOP",
                                              "P_POC" = "P_POC",
                                              "P_CHILD" = "P_CHILD",
                                              "P_ELDERLY" = "P_ELDERLY", 
                                              "P_POVERTY"= "P_POVERTY", 
                                              "P_DISABILITY"="P_DISABILITY", 
                                              "P_LIMENG"="P_LIMENG", 
                                              "ASTHMA" ="ASTHMA ", 
                                              "CHD"="CHD", 
                                              "OBESITY"="OBESITY", 
                                              "P_TREECOVER"="P_TREECOVER", 
                                              "P_NOTREE"= "P_NOTREE", 
                                              "P_IMPSURF"= "P_IMPSURF", 
                                              "AIRTEMP_MEAN"="AIRTEMP_MEAN",
                                              "HSI" = "HSI",
                                              "HEI"="HEI", 
                                              "HSEI"="HSEI",
                                              "med_income"="med_income"), selected = 1)
                   
                 ),
                 
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("Distribution", # Plots of distributions
                                        fluidRow(
                                          column(6, plotOutput("distribution1")),
                                          column(6, plotOutput("distribution2"))
                                        )),
                               tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                               tabPanel("Diagnosticplot",
                                        fluidRow(column(6, plotOutput("DiagnosticPlot")))),
                               tabPanel("Data", DT::dataTableOutput('tbl')) # Data as data table
                               
                   )
                 )
               )
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
  
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(stat_heat_DC[,input$outcome] ~ stat_heat_DC[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Diagnostic output
  
  fit2 <- reactive(lm(stat_heat_DC[,input$outcome] ~ stat_heat_DC[,input$indepvar]))
  
  output$DiagnosticPlot <- renderPlot({
    par(mfrow = c(2,2))
    plot(fit2())
  }, height = 600, width = 800)
  
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(stat_heat_DC, options = list(lengthChange = FALSE))
  })
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(stat_heat_DC[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(stat_heat_DC[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(ggplot2)
library(tidyverse)
library(shiny)

heat_DC <- read_csv("../data/DC_Heat_Island.csv", show_col_types = F)
heat_DC <- heat_DC %>%
    rename(med_income = estimate) %>%
    select(-geometry,-OBJECTID:-ID, -GIS_ID:-variable, -moe)

heat_DC <- data.frame(heat_DC)

ui <- fluidPage(
    titlePanel("Regression Model"),
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
    ))



# SERVER
server <- function(input, output) {
    
    
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(heat_DC[,input$outcome] ~ heat_DC[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Diagnostic output
    
    fit2 <- reactive(lm(heat_DC[,input$outcome] ~ heat_DC[,input$indepvar]))
    
    output$DiagnosticPlot <- renderPlot({
        par(mfrow = c(2,2))
        plot(fit2())
    }, height = 600, width = 800)
    
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(heat_DC, options = list(lengthChange = FALSE))
    })
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(heat_DC[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(heat_DC[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
}

shinyApp(ui = ui, server = server)
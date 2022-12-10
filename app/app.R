

library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(viridis)
library(sf)
library(thematic)
library(corrplot)
library(recipes)
library(nortest)
library(leaps)
library(shinydashboard)
library(shinycssloaders)


heat_DC <- read_csv("../data/DC_Heat_Island.csv", show_col_types = F)
tracts <- read_sf("../data/Census_Tracts_in_2020/")
Cooling_Centers <- read_csv("../data/Cooling_Centers_-_District_of_Columbia.csv")
urban_forestry <- read_csv("../data/Urban_Forestry_Street_Trees.csv")
forest_samp <- slice_sample(urban_forestry, prop = .2) 

tracts_clean <- tracts %>%
  st_transform(4326) %>%
  mutate(popup_label = paste0('<br/>Name: ', NAME, '<br/>'))


stat_heat_DC <- heat_DC %>%
  select(-geometry,-OBJECTID:-ID, -GIS_ID:-variable, -moe)

response <- stat_heat_DC %>%
  select(HSI, HEI, HSEI)

heat_DC <- heat_DC %>%
  mutate(majority_minority = case_when(P_POC > 50 ~ "TRUE",
                                       TRUE ~ "FALSE")) %>%
  rename(med_income = estimate) %>%
  select(-geometry)

heat_bivariate <- heat_DC %>%
  dplyr::select(-OBJECTID:-ID, -GIS_ID: -variable, -moe) 

#Clean cooling center data
cooling_centers_clean <- Cooling_Centers %>% 
  rename(latitude = Y,
         longitude = X,
         TYPE = TYPE_) %>% 
  mutate(popup_label = paste(paste0('<br/>Name: ', NAME, '<br/>'),
                             paste0("Type: ", TYPE, '<br/>'),
                             paste0('<br/>Address: ', ADDRESS, '<br/>'),
                             paste0('<br/>Capacity: ', Capacity, '<br/>'),
                             paste0('<br/>Phone: ', PHONE, '<br/>'),
                             paste0('<br/>Hours: ', Hours3, '<br/>')), 
         sep = '<br/>')

#Clean Tree Data
forest_samp <- forest_samp %>% 
  rename(latitude = Y,
         longitude = X) %>% 
  mutate(popup_label = paste(paste0("Type: ", CMMN_NM, '<br/>'),
                             paste0('<br/>Condition: ', CONDITION, '<br/>'),
                             paste0('<br/>Ownership: ', OWNERSHIP, '<br/>'),
                             paste0('<br/>Tree Notes: ', TREE_NOTES, '<br/>')), 
         sep = '<br/>')

#Join tracts to dc heat
heat_map_data <- tracts_clean |>
  select(TRACT, geometry, popup_label) |>
  left_join(heat_DC)

#Add Pallet for Map
pal <- colorNumeric(
  palette = "Reds",
  domain = NULL)



# Data for regression tab


# User Interface Code
thematic::thematic_shiny()
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("DC Heat Sensitivity and Exposure", windowTitle = "DC Heat Sensitivity and Exposure"),
  mainPanel(
    tabsetPanel(
      type = "pills",
      tabPanel("Introduction",
               h3(strong("Description")),
               p(style="text-align: justify; font-size = 25px",
                 "The objective of this app is to provide a tool to analyzes heat sensitivities across DC census tracts. We aim to 
                provide useful statistically model specific correlates of the Heat Sensitivity Exposure Index (HSEI). 
               These correlates can then be taken and used to 
               explore any socio-demographic trends that are prevalent on a census tract basis."
               ),
               h3(strong("Use Case")),
               p(style="text-align: justify; font-size = 25px",
                 "The actor for this app will most likely be someone within the DC government’s Department of Energy and Environment or Health and Human Services. 
               The purpose of the visual representation and statistical modeling would be to assist in the formulation of policy 
               that attempts to mitigate the occurrences of heat-related health incidences."),
               h3(strong("Data")),
               p(style="text-align: justify; font-size = 25px",
                 "The data used in this app comes from a variety of sources. Our main source comes from the",
                 a(href = "https://opendata.dc.gov/datasets/DCGIS::heat-sensitivity-exposure-index/about", "Heat Sensitivity Exposure Index"),
                 "The data was found on Open Data DC and has a variety of sociodemographic variables for each census tract (percent poverty, percent people of color, etc.), 
               as well as some environmental variables explaining heat indexes (percent impervious surface, percent tree color, mean air temperature, etc.)."
               ),
               h3(strong("Attributes introduction")),
               p(style="text-align: justify; font-size = 25px",
                 "TOTALPOP=Total 2020 census population",tags$br(),
                 "P_POC=Percent of people of color population",tags$br(),
                 "P_CHILD=Percent of population below 5 years of age",tags$br(),
                 "P_ELDERLY=Percent of population over 65 years of age",tags$br(),
                 "P_POVERTY=Percent of population below 200% above the Federal Povery Line",tags$br(),
                 "P_DISABILITY=Percent of population that report having a disability",tags$br(),
                 "P_LIMENG=Percent of households that are “limited English speaking”",tags$br(),
                 "ASTHMA=Crude prevalence of adults with asthma",tags$br(),
                 "CHD=Crude prevalence of adults diagnosed with CHD",tags$br(),
                 "OBESITY=Crude prevalence of adults who are obese",tags$br(),
                 "P_TREECOVER=Percent of census tract covered in tree canopy",tags$br(),
                 "P_NOTREE=Percent of census tract not covered in tree canopy",tags$br(),
                 "P_IMPSURF=Percent of impervious surfaces",tags$br(),
                 "AIRTEMP_MEAN=Average ambient air temperature per Census Tract",tags$br(),
                 "HSI=Heat Sensitivity Index",tags$br(),
                 "HEI=Heat Exposure Index",tags$br(),
                 "HSEI=Heat Sensitivity Exposure Index",tags$br()
               )),
      tabPanel("Graphing",
               sidebarLayout(
                 sidebarPanel(
                   varSelectInput("var1", "X variable", data = heat_bivariate, selected = "TOTALPOP"),
                   varSelectInput("var2", "Y variable", data=heat_bivariate, selected = "HEI"),
                   varSelectInput("var3", "Color variable (categorical)", data = heat_bivariate, selected = "majority_minority"),
                   sliderInput("bins_x", "Number of Bins for X Variable", min = 1, max = 50, value = 30),
                   sliderInput("bins_y","Number of Bins for Y Variable", min = 1, max = 50, value = 30),
                   checkboxInput("smooth", "Add Smoothing Method?", value = FALSE),
                   checkboxInput("log_x", "Log the X Variable?", value = FALSE),
                   checkboxInput("log_y", "Log the Y Variable?", value = FALSE)
                 ),
                 mainPanel(
                   tabsetPanel(
                     type = "pills",
                     tabPanel("Bivariate Analysis",
                              fluidRow(
                                column(12, plotOutput("plot1"))),
                              fluidRow(
                                column(6, plotOutput("plot5")),
                                column(6, plotOutput("plot6"))
                              )),
                     tabPanel("Univariate Analysis",
                              fluidRow(
                                column(4, plotOutput("plot2")),
                                column(4, plotOutput("plot3")),
                                column(4, plotOutput("plot4"))))
                   )))), #End tabPanel
      tabPanel("Regression Analysis",
               sidebarLayout(
                 sidebarPanel(width=3,
                              selectInput("y", label = h4("Select Y Variable:"),
                                          choices = names(response), selected = "HSI"),
                              solidHeader = TRUE,
                              status = "primary",           
                              checkboxGroupInput("x", label = h4("Select X Variables:"),
                                                 choices = names(stat_heat_DC), selected = "TOTALPOP"),
                              varSelectInput("bi_variable_x","X Variable - Bivariate Tab:",stat_heat_DC,"TOTALPOP"),
                              varSelectInput("bi_variable_y","Y Variable - Bivariate Tab:",stat_heat_DC,"HSI"),
                              solidHeader = TRUE,
                              status = "primary"),
                 mainPanel(tabsetPanel(type = "pills",
                                       tabPanel("Model Summary",
                                                box(withSpinner(verbatimTextOutput("Model")), width = 12, title = h4("Regression Summary")
                                                ),
                                                
                                                box(withSpinner(verbatimTextOutput("Summ")), width = 12, title = h4("Descriptive Statistics")
                                                )
                                       ),
                                       tabPanel("Correlation",
                                                box(withSpinner(plotOutput("Corr")),width = 12, title = h4("Correlation - by Selected Variables")
                                                )
                                       ),
                                       tabPanel("Bivariate", 
                                                box(withSpinner(plotOutput("bi_plot")),width=12,title=h4("Bivariate Plot - by Selected variables")
                                                ),
                                                tabPanel("Model Fit",
                                                         box(withSpinner(plotOutput("residualPlots")), width = 12, title = h4("Diagnostic Plots")
                                                         )
                                                )
                                       ),
                                       tabPanel("Data",
                                                box(withSpinner(dataTableOutput('tbl')), width = 12, title = h4("Data for DC Heat")
                                                )
                                       )
                 )      
                 )
               )),#End tabPanel2
      tabPanel("Mapping",
               sidebarLayout(
                 sidebarPanel(varSelectInput("var_map", "What metric?", data = heat_bivariate, selected = "TOTALPOP"),
                              checkboxInput("cooling", "Show DC Cooling Centers", value = FALSE),
                              checkboxInput("trees", "Show DC Tree Data?", value = FALSE)),
                 mainPanel(leafletOutput("map_plot", height = "600px"))
               )
      ), # End tabPanel3
      tabPanel("Working Data",
               checkboxInput("limit_id", "Remove Duplicate ID Variables?"),
               checkboxInput("hsi_only", "HSI Data Only"),
               checkboxInput("hei_only", "HEI Data Only"),
               dataTableOutput("dynamic"))
    ) # End tabsetPanel
  )# End mainPanel
)# End fluidPage


# Server Code
server <- function(input, output) {
  
  # Bivariate Analysis Tab
  output$plot1 <- renderPlot({
    plot1 <-  ggplot(heat_DC, aes(x = !!input$var1, y = !!input$var2)) +
      geom_point()
    
    if (is.numeric(heat_DC[[input$var3]])) {
      if (input$log_x == TRUE) {
        if (input$log_y == TRUE) {
          if (input$smooth == TRUE) {
            plot1 + scale_x_log10() + scale_y_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1 + scale_x_log10() + scale_y_log10()
          }
        } else {
          if (input$smooth == TRUE) {
            plot1 + scale_x_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1 + scale_x_log10()
          }
        }
      } else if (input$log_y == TRUE) {
        if (input$log_x == TRUE) {
          if (input$smooth == TRUE) {
            plot1 + scale_x_log10() + scale_y_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1 + scale_x_log10() + scale_y_log10()
          }
        } else {
          if (input$smooth == TRUE) {
            plot1 + scale_y_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1 + scale_y_log10()
          }
        }
      } else {
        if (input$smooth == TRUE) {
          plot1 + geom_smooth(se = F, method = "lm")
        } else {
          plot1
        }
      }
    } else {
      plot1.1 <- ggplot(heat_DC, aes(x = !!input$var1, y = !!input$var2, color = !!input$var3)) +
        geom_point()
      
      if (input$log_x == TRUE) {
        if (input$log_y == TRUE) {
          if (input$smooth == TRUE) {
            plot1.1 + scale_x_log10() + scale_y_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1.1 + scale_x_log10() + scale_y_log10()
          }
        } else {
          if (input$smooth == TRUE) {
            plot1.1 + scale_x_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1.1 + scale_x_log10()
          }
        }
      } else if (input$log_y == TRUE) {
        if (input$log_x == TRUE) {
          if (input$smooth == TRUE) {
            plot1.1 + scale_x_log10() + scale_y_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1.1 + scale_x_log10() + scale_y_log10()
          }
        } else {
          if (input$smooth == TRUE) {
            plot1.1 + scale_y_log10() + geom_smooth(se = F, method = "lm")
          } else {
            plot1.1 + scale_y_log10()
          }
        }
      } else {
        if (input$smooth == TRUE) {
          plot1.1 + geom_smooth(se = F, method = "lm")
        } else {
          plot1.1
        }
      }
    }
  })
  
  output$plot2 <- renderPlot({
    plot2 <- ggplot(heat_DC, aes(x = !!input$var1)) +
      geom_histogram(bins = input$bins_x)
    if (input$log_x == TRUE) {
      plot2 +scale_x_log10()
    } else {
      plot2
    }
  })
  
  output$plot3 <- renderPlot({
    plot3 <- ggplot(heat_DC, aes(x = !!input$var2)) +
      geom_histogram(bins = input$bins_y)
    if (input$log_y == TRUE) {
      plot3 + scale_y_log10()
    } else {
      plot3
    }
  })
  
  output$plot4 <- renderPlot({
    plot4 <- ggplot(heat_DC, aes(!!input$var3)) +
      geom_bar()
    if (is.numeric(heat_DC[[input$var3]])) {
      validate("No Categorical Variable Selected")
    } else {
      plot4
    }
  })
  
  output$plot5 <- renderPlot({
    plot5 <- ggplot(heat_DC, aes(!!input$var3,!!input$var1)) +
      geom_violin()
    
    if (is.numeric(heat_DC[[input$var3]])) {
      validate("No Categorical Variable Selected")
    } else if (input$log_x == TRUE) {
      plot5 + scale_y_log10()
    } else {
      plot5
    }
  })
  
  output$plot6 <- renderPlot({
    plot6 <- ggplot(heat_DC, aes(!!input$var3,!!input$var2)) +
      geom_violin()
    
    if (is.numeric(heat_DC[[input$var3]])) {
      validate("No Categorical Variable Selected")
    } else if (input$log_y == TRUE) {
      plot6 + scale_y_log10()
    } else {
      plot6
    }
  })
  
  # Working Data Tab
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
  
  # For tab Regression by data use
  
  inputdata <- reactive({
    stat_heat_DC})
  # Model Summary
  # Regression Output
  reg <-reactive(stat_heat_DC %>%
                   recipe() %>%
                   update_role(!!!input$y,new_role = "outcome") %>%
                   update_role(!!!input$x,new_role = "predictor") %>%
                   prep() %>%
                   formula()
  )
  lm_reg <- reactive(
    lm(reg(),data = inputdata()))
  output$Model = renderPrint({summary(lm_reg())})
  # Descriptive Statistics
  
  sumstats<- reactive({
    stat_heat_DC %>%
      select(!!input$x,!!input$y) -> sumdf
  })
  output$Summ <- renderPrint(summary(sumstats()))
  # Correlation Plot
  cornum<- reactive({
    stat_heat_DC %>%
      select(!!input$x,!!input$y)->cordf
    round(cor(cordf),1)
  })
  
  output$Corr <-
    renderPlot(corrplot(
      cornum(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  # Bivariate tab
  output$bi_plot<- renderPlot({
    validate(
      need(input$bi_variable_x, "Please Choose a Variable for X"),
      need(input$bi_variable_y,"Please Choose a Variable for Y")
    )
    stat_heat_DC %>%
      ggplot(aes(x=!!input$bi_variable_x,!!input$bi_variable_y))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      theme_bw()
  })
  
  # Model fit tab
  # Residual and QQ-Plot
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(lm_reg())
    par(mfrow = c(1, 1))
  })
  
  # Normality Test
  res <- reactive(rstudent(lm_reg()))
  output$normality <- renderPrint({ad.test(res())})
  
  # Data tab
  inputdata <- reactive({
    stat_heat_DC})
  output$tbl <- renderDataTable(inputdata(), options = list(pageLength = 5))
  
  
  # Mapping Output
  
  output$map_plot <- renderLeaflet({
    map <- leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = heat_map_data,
                  color = 'white',
                  weight = 1.5,
                  opacity = 1,
                  fillColor = ~ pal(heat_map_data[[input$var_map]]),
                  fillOpacity = .8,
                  highlightOptions = highlightOptions(color = "#FFF1BE",
                                                      weight = 5),
                  popup = ~ tracts_clean$NAME)
    
    if (input$cooling == TRUE) {
      map %>%
        addCircleMarkers(data = cooling_centers_clean,
                         popup = ~popup_label,
                         stroke = F,
                         radius = 3, 
                         fillColor= "#4DB6D0",
                         fillOpacity = .8) ->
        map
    }
    if (input$trees == TRUE) {
      map %>%
        addCircleMarkers(data = forest_samp,
                         popup = ~popup_label,
                         stroke = F,
                         radius = .3, 
                         fillColor= "green",
                         fillOpacity = .1)->
        map
    } 
    map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


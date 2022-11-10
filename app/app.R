library(shiny)
library(bslib)

# User Interface Code
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("World's Water Sources and Potetnial Scarcity", windowTitle = "World's Water Sources and Potetnial Scarcity"),
  
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
  ) # End mainPanel
) # End fluidPage


# Server Code
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)

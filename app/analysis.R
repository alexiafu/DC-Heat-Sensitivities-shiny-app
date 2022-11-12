library(shiny)
library(bslib)
library(tidyverse)

data <- read_csv("../data/final_data.csv")

ui <- fluidPage(
    varSelectInput("var1", "X variable", selected = "Country_Name", data = data),
    varSelectInput("var2", "Y variable", selected = "population", data = data),
    varSelectInput("var3", "Color variable (Categorical)", selected = "DVSN_Type", data = data),
    plotOutput("plot"),
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        ggplot(data, aes(x = !!(input$var1), y = !!input$var2, color = !!input$var3)) +
            geom_line()
    })
}

shinyApp(ui, server)

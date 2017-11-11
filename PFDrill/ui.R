library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Heatmap"),
    mainPanel(
        plotlyOutput("heat")
    ),
    verbatimTextOutput("selection")
)


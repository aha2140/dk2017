library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(shinydashboard)
library(ggvis)
library(tidyr)


correlation <- round(cor(mtcars), 3)
nms <- names(mtcars)

loadData <- function() {
    d <- read.csv('Goal1_PFD_1of3.csv.gz')
    d$CalendarYear <- d$CalendarYear + 2000
    c <- read.csv('Cands.csv.gz')
    print(nrow(d))
    return(d)
}
    
getHeatMapData <- function(d) {
    #Todo: avoid this two-step
    tmp <- d %>% group_by(CID, CalendarYear) %>% summarize(n=n()) %>% arrange(desc(n))
    x <- data.frame(CID = tmp$CID, Year=tmp$CalendarYear, n=tmp$n)
    x$Year <- sprintf('Year-%s', x$Year)
    y <- spread(x, Year, n)
    cids <- y$CID
    z <- y[,2:ncol(y)]
    rownames(z) <- cids
    m <- as.matrix(z)
    m[is.na(m)] <- 0
    return (t(m))
}

df <- {
    #load(file='df.Rdata')
    loadData()
}
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$heat <- renderPlotly({
        v <- getHeatMapData(df)
        a <- attributes(v)[[2]]
        cids <- unlist(a[2])
        years <- unlist(a[1])
        m <- matrix(v, nrow=nrow(v))
        plot_ly(x = cids, y = years, z = m, type = "heatmap", 
                colorscale = "Greys", source = "heatplot") %>%
            layout(xaxis = list(title = ""), yaxis = list(title = ""))
    })
    
    output$selection <- renderPrint({
        s <- event_data("plotly_click")
        if (length(s) == 0) {
            "Text to go here"
        } else {
            cat("You selected: \n\n")
            as.list(s)
        }
    })
}
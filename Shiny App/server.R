library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


bankdata <- read.csv("../data.csv", header=TRUE)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Get Data
    getData <- reactive({
        newdata <- bankdata
    })

    # create output of observations    
    output$dataTable <- renderDataTable({
      head(getData())
    })
    
    # Plot
    output$testPlot1 <- renderPlot({
        #get filtered data
        newData <- getData()
        
        #create plot
        ggplot(newData, aes(x = Bankrupt.)) + geom_bar()
        
    })

})


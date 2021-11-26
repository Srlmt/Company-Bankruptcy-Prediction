library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Read data
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
    
    #########
    #  EDA  #
    #########
    
    # Descriptive Statistics
    output$descSummary <- renderTable({
        
        if (input$groupby == FALSE){
              getData() %>% 
                summarize(N = n(), 
                          Min. = min(!!sym(input$selectVar)),
                          Q1 = quantile(!!sym(input$selectVar), 0.25),
                          Mean = mean(!!sym(input$selectVar)), 
                          SD = sd(!!sym(input$selectVar)),
                          Q3 = quantile(!!sym(input$selectVar), 0.75),
                          Max = max(!!sym(input$selectVar))
          ) 

        }else if (input$groupby == TRUE){
          getData() %>%
            group_by(Bankrupt.) %>%
                summarize(N = n(), 
                          Min. = min(!!sym(input$selectVar)),
                          Q1 = quantile(!!sym(input$selectVar), 0.25),
                          Mean = mean(!!sym(input$selectVar)), 
                          SD = sd(!!sym(input$selectVar)),
                          Q3 = quantile(!!sym(input$selectVar), 0.75),
                          Max = max(!!sym(input$selectVar))
            )
        }
    }, 
    digits = 4
    )
    
    # Frequency Table
    output$freqSummary <- renderTable({
      

      var <- getData()[[input$selectVar]]
      
      freq <- cut(var, breaks = input$datacut)
      
      if (input$groupby == FALSE){
         table(freq)
      }else if (input$groupby == TRUE){
         table(freq, getData()$Bankrupt.)
      }   
      
    })
    
    
    # Histogram
    output$Histogram <- renderPlot({
      #get filtered data
      newData <- getData()
      
      varText <- input$selectVar
      
      if(input$groupby == FALSE){
        ggplot(getData(), aes_string(x = varText)) + geom_histogram() 
        
      }else if(input$groupby == TRUE){
        ggplot(getData(), aes_string(x=varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) + geom_histogram()
      }
    })
    
    # BoxPlot
    output$BoxPlot <- renderPlot({
        #get filtered data
        newData <- getData()
        
        varText <- input$selectVar
        
        if(input$groupby == FALSE){
          ggplot(getData(), aes_string(y = varText)) + geom_boxplot() 
          
        }else if(input$groupby == TRUE){
          ggplot(getData(), aes_string(x="as.factor(Bankrupt.)", y = varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) + geom_boxplot()
        }
    })
    
   

})



ggplot(bankdata, aes_string(x = "Equity.to.Liability", group="Bankrupt.", fill="as.factor(Bankrupt.)")) + geom_histogram()

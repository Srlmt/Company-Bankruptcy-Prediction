library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(caret)

# Read data
bankdata <- read.csv("../data.csv", header=TRUE)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # Get Data
    getData <- reactive({
        newdata <- bankdata
    })
    
    
    
    # Get Variable Name
    getVarName <- reactive({
        gsub("\\.", " ", input$selectVar)
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
         freqTable <- as.data.frame(table(freq))
         colnames(freqTable) <- c("Interval", "Frequency")
         freqTable
         
      }else if (input$groupby == TRUE){
         freqTable <- as.data.frame(table(freq, getData()$Bankrupt.))
         colnames(freqTable) <- c("Interval", "Bankrupt", "Frequency")
         freqTable
      }   
      
    })
    
    
    # Histogram
    output$Histogram <- renderPlot({
      #get filtered data
      newData <- getData()
      
      varText <- input$selectVar
      
      if(input$groupby == FALSE){
        ggplot(getData(), aes_string(x = varText)) + 
          geom_histogram() +
          labs(x=getVarName())
        
      }else if(input$groupby == TRUE){
        ggplot(getData(), aes_string(x=varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) +
          geom_histogram() +
          labs(x=getVarName(), fill="Bankrupt")
      }
    })
    
    # BoxPlot
    output$BoxPlot <- renderPlot({
        #get filtered data
        newData <- getData()
        
        varText <- input$selectVar
        
        if(input$groupby == FALSE){
          ggplot(getData(), aes_string(y = varText)) + 
            geom_boxplot() +
            labs(y=getVarName())
          
        }else if(input$groupby == TRUE){
          ggplot(getData(), aes_string(x="as.factor(Bankrupt.)", y = varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) +
            geom_boxplot() +
            labs(x="Bankrupt", y=getVarName(), fill="Bankrupt")
        }
    })
    
    
    ## Update Title based on variable selected ##
    # Plot Title
    output$plotTitle <- renderUI({
      
      if(input$rbPlot == "hist"){
        plotTitle <- paste0("Histogram of ", stringr::str_to_title(getVarName()))
      }else if(input$rbPlot == "box"){
        plotTitle <- paste0("Boxplot of ", stringr::str_to_title(getVarName()))
      }
      
      if(input$groupby == TRUE){
        plotTitle <- paste0(plotTitle, " Grouped by Bankruptcy")
      }
      h3(plotTitle)
    })
   
    # Numeric Summary Title
    output$numSumTitle <- renderUI({
      
      if(input$rbNum == "descStat"){
        plotTitle <- paste0("Descriptive Statistics of ", stringr::str_to_title(getVarName()))
      }else if(input$rbNum == "freq"){
        plotTitle <- paste0("Frequency Table of ", stringr::str_to_title(getVarName()))
      }
      
      if(input$groupby == TRUE){
        plotTitle <- paste0(plotTitle, " Grouped by Bankruptcy")
      }
      h3(plotTitle)
    })
    
    
    ##############
    #  Modeling  #
    ##############

    
    
    
})


#mylogit <- glm(Bankrupt. ~ Cash.flow.rate + Net.Value.Growth.Rate, data=bankdata, family="binomial")
#summary(mylogit)
#predicted <- predict(mylogit, bankdata, type="response")

#confusionMatrix(as.factor(as.numeric(predicted > 0.5)), as.factor(bankdata$Bankrupt.))








#shiny::runGitHub("Srlmt/Company-Bankruptcy-Prediction", ref="main", subdir="Shiny App")




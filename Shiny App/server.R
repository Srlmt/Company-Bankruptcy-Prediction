library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(caret)
library(rattle)

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
      head(bankdata)
    })
    
    #########
    #  EDA  #
    #########
    
    # Descriptive Statistics
    output$descSummary <- renderTable({
        
        if (input$groupby == FALSE){
              bankdata %>% 
                summarize(N = n(), 
                          Min. = min(!!sym(input$selectVar)),
                          Q1 = quantile(!!sym(input$selectVar), 0.25),
                          Mean = mean(!!sym(input$selectVar)), 
                          SD = sd(!!sym(input$selectVar)),
                          Q3 = quantile(!!sym(input$selectVar), 0.75),
                          Max = max(!!sym(input$selectVar))
                ) 

        }else if (input$groupby == TRUE){
          bankdata %>%
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
      
      var <- bankdata[[input$selectVar]]
      
      freq <- cut(var, breaks = input$datacut)
      
      if (input$groupby == FALSE){
         freqTable <- as.data.frame(table(freq))
         colnames(freqTable) <- c("Interval", "Frequency")
         freqTable
         
      }else if (input$groupby == TRUE){
         freqTable <- as.data.frame(table(freq, bankdata$Bankrupt.))
         colnames(freqTable) <- c("Interval", "Bankrupt", "Frequency")
         freqTable
      }   
      
    })
    
    
    # Histogram
    output$Histogram <- renderPlot({
      #get filtered data
      newData <- bankdata
      
      varText <- input$selectVar
      
      if(input$groupby == FALSE){
        ggplot(bankdata, aes_string(x = varText)) + 
          geom_histogram() +
          labs(x=getVarName())
        
      }else if(input$groupby == TRUE){
        ggplot(bankdata, aes_string(x=varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) +
          geom_histogram() +
          labs(x=getVarName(), fill="Bankrupt")
      }
    })
    
    # BoxPlot
    output$BoxPlot <- renderPlot({
        #get filtered data
        newData <- bankdata
        
        varText <- input$selectVar
        
        if(input$groupby == FALSE){
          ggplot(bankdata, aes_string(y = varText)) + 
            geom_boxplot() +
            labs(y=getVarName())
          
        }else if(input$groupby == TRUE){
          ggplot(bankdata, aes_string(x="as.factor(Bankrupt.)", y = varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) +
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
    
    observeEvent(input$startbutton, {
      
      # Set Randomization Seed
      set.seed(556881234)
      
      # Sample Training and Test data using the input p
      partition <- createDataPartition(y = bankdata$Bankrupt., 
                                       p= input$trainpct / 100, 
                                       list = FALSE)
      
      bankTrain <- bankdata[partition,]
      bankTest <- bankdata[-partition,]

      
      ### GLM Model ###
      model_glm <- train(as.factor(Bankrupt.) ~ .,
                         data = bankTrain[, c("Bankrupt.", input$glmVar)],
                         method = "glm",
                         family = "binomial",
                         trControl = trainControl(method = "cv", number = 5)
                   )
      
      # GLM Summary
      output$glmSummary <- renderPrint({summary(model_glm)
      })
      
      # GLM Confusion Matrix
      glmPred <- predict(model_glm, bankTrain, type="raw")
      output$glmMatrix <- renderPrint({
        confusionMatrix(glmPred, as.factor(bankTrain$Bankrupt.))
      })
      
      
      
      ### Tree Model ###
      model_tree <- train(as.factor(Bankrupt.) ~ .,
                         data = bankTrain[, c("Bankrupt.", input$treeVar)],
                         method = "rpart",
                         trControl = trainControl(method = "cv", number = 5)
      )
      
      # Fancy Tree Diagram
      output$treeDiagram <- renderPlot({
        fancyRpartPlot(model_tree$finalModel)
      })
      
      # Tree Model Output
      output$treeOutput <- renderPrint({
        model_tree$finalModel
      })
      
      # Tree Confusion Matrix
      treePred <- predict(model_tree, bankTrain, type="raw")
      output$treeMatrix <- renderPrint({
        confusionMatrix(treePred, as.factor(bankTrain$Bankrupt.))
      })
      
      ### Random Forest Model ###
      model_rf <- train(as.factor(Bankrupt.) ~ .,
                        data = bankTrain[, c("Bankrupt.", input$rfVar)],
                        method = "rf",
                        trControl = trainControl(method = "cv", number = 5),
                        preProcess = c("center", "scale"),
                        tuneGrid = data.frame(mtry = 1:5)
      )
      
      # Random Forest Most Important Feature Plot
      output$rfSummary <- renderPlot({
        ggplot(varImp(model_rf)) + 
          geom_col(fill="navy") 
      })
      
      # Random Forest Model Output
      output$rfOutput <- renderPrint({
        model_rf$finalModel
      })

      
    })
    
})
    



#shiny::runGitHub("Srlmt/Company-Bankruptcy-Prediction", ref="main", subdir="Shiny App")






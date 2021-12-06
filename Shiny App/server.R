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
  
    getFilteredData <- reactive({
      if(input$debtRatio == "above"){
        newdata <- bankdata %>% filter(Debt.ratio.. > mean(Debt.ratio..))
        
      }else if(input$debtRatio == "below"){
        newdata <- bankdata %>% filter(Debt.ratio.. < mean(Debt.ratio..))
        
      }else if (input$debtRatio == "all"){
        newdata <- bankdata
      }
      
      newdata
    })
    
    # Create Variable Name for Display
    getVarName <- reactive({
        gsub("\\.", " ", input$selectVar)
    })

    #########
    #  EDA  #
    #########
    
    # Descriptive Statistics
    output$descSummary <- renderTable({
        
        if (input$groupby == FALSE){
          getFilteredData() %>% 
                summarize(N = n(), 
                          Min. = min(!!sym(input$selectVar)),
                          Q1 = quantile(!!sym(input$selectVar), 0.25),
                          Mean = mean(!!sym(input$selectVar)), 
                          SD = sd(!!sym(input$selectVar)),
                          Q3 = quantile(!!sym(input$selectVar), 0.75),
                          Max = max(!!sym(input$selectVar))
                ) 

        }else if (input$groupby == TRUE){
          getFilteredData() %>%
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
      
      var <- getFilteredData()[[input$selectVar]]
      
      freq <- cut(var, breaks = input$datacut)
      
      if (input$groupby == FALSE){
         freqTable <- as.data.frame(table(freq))
         colnames(freqTable) <- c("Interval", "Frequency")
         freqTable
         
      }else if (input$groupby == TRUE){
         freqTable <- as.data.frame(table(freq, getFilteredData()$Bankrupt.))
         colnames(freqTable) <- c("Interval", "Bankrupt", "Frequency")
         freqTable
      }   
      
    })
    
    
    # Histogram
    output$Histogram <- renderPlot({
      
      # Get the selected variables
      varText <- input$selectVar
      
      if(input$groupby == FALSE){
        ggplot(getFilteredData(), aes_string(x = varText)) + 
          geom_histogram(bins=50) +
          labs(x=getVarName())
        
      }else if(input$groupby == TRUE){
        ggplot(getFilteredData(), aes_string(x=varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) +
          geom_histogram(bins=50) +
          labs(x=getVarName(), fill="Bankrupt")
      }
    })
    
    # BoxPlot
    output$BoxPlot <- renderPlot({
        
        # Get the selected variables
        varText <- input$selectVar
        
        if(input$groupby == FALSE){
          ggplot(getFilteredData(), aes_string(y = varText)) + 
            geom_boxplot() +
            labs(y=getVarName())
          
        }else if(input$groupby == TRUE){
          ggplot(getFilteredData(), aes_string(x="as.factor(Bankrupt.)", y = varText, group="Bankrupt.", fill="as.factor(Bankrupt.)")) +
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
      
      # Create a Progress object
      progress <- Progress$new()
      
      # Close the progress when done with this "observeEvent"
      on.exit(progress$close())
      
      # Start the progress bar, Fitting Model
      progress$set(message = "Fitting Model", value = 0)
      
      # Set Randomization Seed
      set.seed(556881234)
      
      # Sample Training and Test data using the input p
      partition <- createDataPartition(y = bankdata$Bankrupt., 
                                       p= input$trainpct / 100, 
                                       list = FALSE)
      
      bankTrain <- bankdata[partition,]
      bankTest <- bankdata[-partition,]

      # Get the number of cross-validation folds
      folds <- input$folds
      
      ### GLM Model ###
      # Check if "all interaction" box is checked and fit model accordingly
      if (input$allInteraction == FALSE){
        model_glm <- train(as.factor(Bankrupt.) ~ .,
                           data = bankTrain[, c("Bankrupt.", input$glmVar)],
                           method = "glm",
                           family = "binomial",
                           metric = "Accuracy",
                           trControl = trainControl(method = "cv", number = folds),
                           preProcess = c("center", "scale")
        )
      }else if (input$allInteraction == TRUE){
        model_glm <- train(as.factor(Bankrupt.) ~ .^2,
                           data = bankTrain[, c("Bankrupt.", input$glmVar)],
                           method = "glm",
                           family = "binomial",
                           metric = "Accuracy",
                           trControl = trainControl(method = "cv", number = folds),
                           preProcess = c("center", "scale")
        )
      }
      
      # Increment the progress bar, update to Fitting GLM
      progress$inc(0.2, detail="Fitting GLM")

      
      # GLM Summary
      output$glmSummary <- renderPrint({summary(model_glm)
      })
      
      # GLM Confusion Matrix
      glmPred <- predict(model_glm, bankTrain, type="raw")
      output$glmMatrix <- renderPrint({
        confusionMatrix(glmPred, as.factor(bankTrain$Bankrupt.))
      })
      
      # Increment the progress bar, update to Fitting Tree Model
      progress$inc(0.2, detail="Fitting Classification Tree")
      
      ### Tree Model ###
      
      # Get the splitting methodology
      if(input$treeMethod == "gini"){
        treeMethod <- "gini"
        
      }else if(input$treeMethod == "info"){
        treeMethod <- "information"
      }
      
      model_tree <- train(as.factor(Bankrupt.) ~ .,
                         data = bankTrain[, c("Bankrupt.", input$treeVar)],
                         method = "rpart",
                         metric = "Accuracy",
                         trControl = trainControl(method = "cv", number = folds),
                         preProcess = c("center", "scale"),
                         parms=list(split=treeMethod)
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
      
      # Increment the progress bar, update to Fitting Random Forest Model
      progress$inc(0.2, detail="Fitting Random Forest")
      
      ### Random Forest Model ###
      
      # Get Number of Trees
      ntree <- input$ntree
      
      model_rf <- train(as.factor(Bankrupt.) ~ .,
                        data = bankTrain[, c("Bankrupt.", input$rfVar)],
                        method = "rf",
                        ntree = ntree,
                        trControl = trainControl(method = "cv", number = folds),
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
      
      # Increment the progress bar, update to Model Comparison
      progress$inc(0.2, detail="Model Comparison")
      
      ## Model Comparison ##
      output$accTest <- renderPrint({
        
        # Predict using the Test data
        glmPredTest <- predict(model_glm, bankTest, type="raw")
        treePredTest <- predict(model_tree, bankTest, type="raw")
        rfPredTest <- predict(model_rf, bankTest, type="raw")

        # Generate confusion matrix for each model
        glmMatrixTest <- confusionMatrix(glmPredTest, as.factor(bankTest$Bankrupt.))
        treeMatrixTest <- confusionMatrix(treePredTest, as.factor(bankTest$Bankrupt.))
        rfMatrixTest <- confusionMatrix(rfPredTest, as.factor(bankTest$Bankrupt.))
        
        # Get accuracy from the matrix
        acc_glm <- glmMatrixTest$overall[1]
        acc_tree <- treeMatrixTest$overall[1]
        acc_rf <- rfMatrixTest$overall[1]
        
        # Combine the accuracies into a table for reporting
        accTest <- rbind(acc_glm, acc_tree, acc_rf)
        rownames(accTest) <- c("Logistic Regression Model", "Classification Tree Model", "Random Forest Model")
        
        # Assign the confusion matrix as output objects for reporting 
        output$glmMatrixTest <- renderPrint({
          glmMatrixTest$table
        })

        output$treeMatrixTest <- renderPrint({
          treeMatrixTest$table
        })
        
        output$rfMatrixTest <- renderPrint({
          rfMatrixTest$table
        })
        
        # Output accuracy table
        round(accTest, 4)

      })
      
      # Save the fitted models for use in Prediction
      saveRDS(model_glm, "./Fitted Models/model_glm.rds")
      saveRDS(model_tree, "./Fitted Models/model_tree.rds")
      saveRDS(model_rf, "./Fitted Models/model_rf.rds")
      
      
      ################
      #  Prediction  #
      ################
      
      # Function to create an un-ordered list for users to input values
      # based on the variables they chose to train the model
      # The default is the mean value
      createInput <- function(inputVars, modVar){
        tags$ul(tagList(
          lapply(inputVars, function(var){
            numericInput(inputId = paste0(var, "Val_", modVar),
                         label = var,
                         value = round(mean(bankdata[, var], na.rm=TRUE), 4),
                         step = 0.1
            )
          })
        ))
      }
      
      # Predict using GLM 
      output$glmPredInput <- renderUI({  
        createInput(input$glmVar, "glm")
      })
      
      # Predict using Tree model
      output$treePredInput <- renderUI({  
        createInput(input$treeVar, "tree")
      })  
      
      # Predict using Random Forest model
      output$rfPredInput <- renderUI({  
        createInput(input$rfVar, "rf")
      })  
      
    })
    

    ### Calculate prediction when user clicks "Predict" ###
    observeEvent(input$predStart, {
      
      # Load the model and variables based on the model chosen
      if (input$chooseModel == "predglm"){
        varsPredict <- unlist(lapply(input$glmVar, paste0, sep="Val_glm"))
        varColNames <- stringr::str_remove_all(varsPredict, pattern="Val_glm")
        predModel <- readRDS("./Fitted Models/model_glm.rds")
        
      }else if (input$chooseModel == "predtree"){
        varsPredict <- unlist(lapply(input$treeVar, paste0, sep="Val_tree"))
        varColNames <- stringr::str_remove_all(varsPredict, pattern="Val_tree")
        predModel <- readRDS("./Fitted Models/model_tree.rds")
      
      }else if (input$chooseModel == "predrf"){
        varsPredict <- unlist(lapply(input$rfVar, paste0, sep="Val_rf"))
        varColNames <- stringr::str_remove_all(varsPredict, pattern="Val_rf")
        predModel <- readRDS("./Fitted Models/model_rf.rds")
      }
      
      # Create a matrix of user inputs
      # Goal is to create a df containing the variable name and input values
      inVar <- c()
      for(var in varsPredict){
        inVar <- c(inVar, input[[var]])
      }
      
      # Remove "_ValueHolder_" from the list to get the variable names
      inVar <- t(matrix(inVar))
      colnames(inVar) <- varColNames
      
      # Create a data frame for use in prediction
      predData <- as.data.frame(inVar)
      
      # Output two kinds of predictions
      classPred <- predict(predModel, predData, type="raw")
      probPred <- predict(predModel, predData, type="prob")
      
      # Combine and assign column names
      predResult <-  cbind(classPred, probPred)
      colnames(predResult) <- c("Bankruptcy Prediction", 
                               "Predicted Probability of 0",
                               "Predicted Probability of 1")
      
      
      # Output the data frame as a table
      output$predTable <- renderTable({
         predResult
        
      }, digits=4
      )
      
    })
    
    
    ##############
    #  Data Tab  #
    ##############
    
    # Filter the row based on the button selection
    filter_row <- reactive({
      if (input$filter_bankrupt == "bankrupt0"){
        selected_data <- bankdata %>% filter(Bankrupt. == 0)
        
      }else if(input$filter_bankrupt == "bankrupt1"){
        selected_data <- bankdata %>% filter(Bankrupt. == 1)
        
      }else{
        selected_data <- bankdata
      }
      
      selected_data
    })
    
    # Output the possibly filtered table for viewing
    output$filtered_data <- renderDataTable({
   
      filter_row() %>% 
        select(input$filter_column)
    })
    
    # Allow user to download possibly filtered data
    output$downloadData <- downloadHandler(
      file = function(){
        paste("bankdata.csv")
      },
      
      content = function(file){
        write.csv(
          filter_row() %>% 
            select(input$filter_column),
                  
        file,
        row.names=FALSE
        )
      }
      
    )
})
    




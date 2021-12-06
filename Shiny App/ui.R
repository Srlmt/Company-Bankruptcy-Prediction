library(ggplot2)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(rattle)

# Read data
bankdata <- read.csv("../data.csv", header=TRUE)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  dashboardHeader(title = "Bankruptcy Prediction"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName="about", icon = icon("address-card")),
      menuItem("Data Exploration", tabName="EDA", icon = icon("chart-line")),
      menuItem("Modeling", tabName="model", icon = icon("cubes")),
      menuItem("Data", tabName="dataDownload", icon = icon("file-csv"))
    )
  ),
  dashboardBody(

     ###############
     #  About Tab  #
     ############### 
     tabItems(
        tabItem(tabName="about",

                h2("Company Bankruptcy Prediction"),
                br(),
                
                img(src='bankruptcy.jpg'),
                br(),
                
                h3("App Purpose"),
                "The purpose of this app is to examine the Taiwanese Bankruptcy Prediction Data Set",
                " and construct models to try to predict whether companies are bankrupt or not.",
                " You will be able to perform data exploration by creating summaries and graphs. ",
                " You will also be able to construct models and make adjustments.",
                " And lastly, you will be able to filter and download the data.",
                br(),
                
                h3("Data Source"),
                "The Taiwanese Bankruptcy Prediction Data Set is from the ",
                a(href = "https://archive.ics.uci.edu/ml/datasets/Taiwanese+Bankruptcy+Prediction#", 
                    "UCI Machine Learning Repository"),
                ". However, the data is missing from the Data Folder, so I found a ",
                a(href = "https://www.kaggle.com/fedesoriano/company-bankruptcy-prediction?select=data.csv",
                    "Kaggle competition"),
                " that uses the data, and downloaded the data from there.",
                br(),
                br(),
                
                "In the dataset there are 1 target variable (Bankruptcy) and 95 attributes.",
                " To make things simple and reduce run time, this app only facilitates 16 attributes",
                " when performing data exploration and model fitting.",
                " However, the full data is available for download in the \"Data\" tab",
                
                h3("Tabs"),
                "Here is a brief summary for each of the tabs in this app:",
                br(),
                br(),
                
                tags$ul(
                  tags$li(strong("Data Exploration: "), "Allows user to create graphical and numerical summaries of 16 attributes"),
                  br(),
                  
                  tags$li(strong("Modeling: "), "Provides information for 3 different models, ",
                          "allows user to adjust parameters and fit the models, and ",
                          "displays model summaries, fit statistics, and comparison"),
                  br(),
                  
                  tags$li(strong("Data: "), "Displays raw dataset and allows user to filter rows and columns, and ",
                          "download the data in csv format")
                )
        ),
       
        #############
        #  EDA Tab  #
        #############
        tabItem(tabName="EDA",
                h1("Data Exploration"),
                br(),
                fluidPage(
                  sidebarPanel(
                    selectInput("selectVar", "Select the Variable of Interest",
                                c("(1) ROA C Before Interest and Depreciation Before Interest" = "ROA.C..before.interest.and.depreciation.before.interest",
                                  "(2) Net Value Per Share B" = "Net.Value.Per.Share..B.",
                                  "(3) Net Value Per Share C" = "Net.Value.Per.Share..C.",
                                  "(4) Persistent EPS in the Last Four Seasons" = "Persistent.EPS.in.the.Last.Four.Seasons",
                                  "(5) Per Share Net Profit Before Tax" = "Per.Share.Net.profit.before.tax..Yuan.Â..",
                                  "(6) Total Debt Total Net Worth" = "Total.debt.Total.net.worth",
                                  "(7) Debt Ratio" = "Debt.ratio..",
                                  "(8) Borrowing Dependency" = "Borrowing.dependency",
                                  "(9) Total Asset Turnover" = "Total.Asset.Turnover",
                                  "(10) Fixed Assets Turnover Frequency" = "Fixed.Assets.Turnover.Frequency",
                                  "(11) Cash Total Assets" = "Cash.Total.Assets",
                                  "(12) Current Liabilities Liability" = "Current.Liabilities.Liability",
                                  "(13) Retained Earnings to Total Assets" = "Retained.Earnings.to.Total.Assets",
                                  "(14) Total Expense Assets" = "Total.expense.Assets",
                                  "(15) Cash Turnover Rate" = "Cash.Turnover.Rate",
                                  "(16) Net Income to Total Assets" = "Net.Income.to.Total.Assets"
                                )
                    ),  
                    radioButtons("debtRatio", strong("Select the Rows to Filter"),
                                 choices=c("All Data" = "all",
                                           "Only Rows with Debt Ratio Above Average" = "above",
                                           "Only Rows with Debt Ratio Below Average" = "below"
                                 )
                    ),
                    br(),
                      
                    radioButtons("rbPlot", strong("Select the Type of Plot"),
                                 choices=c("Histogram" = "hist",
                                           "Boxplot" = "box"
                                 )
                    ),
                    br(),

                    radioButtons("rbNum", strong("Select the Type of Numeric Summary"),
                                  choices=c("Descriptive Statistics" = "descStat",
                                            "Frequency Table" = "freq"
                                  )
                    ), 
                    br(),
                    
                    conditionalPanel(condition = "input.rbNum == 'freq'",
                                     sliderInput("datacut",
                                                 "Select Number of data cut Intervals",
                                                 min = 5,
                                                 max = 50,
                                                 value = 10),
                                     br()
                    ),

                    checkboxInput("groupby", strong("Group by Bankruptcy"), FALSE)
                  ),
                  
                  mainPanel(uiOutput("plotTitle"),
                    
                            conditionalPanel(condition = "input.rbPlot == 'box'",
                                             plotOutput("BoxPlot")
                            ),
                            conditionalPanel(condition = "input.rbPlot == 'hist'",
                                             plotOutput("Histogram")               
                            ),
                            br(),
                            
                            
                            uiOutput("numSumTitle"),
                            conditionalPanel(condition = "input.rbNum == 'descStat'",
                                             tableOutput("descSummary")
                            ),
                            conditionalPanel(condition = "input.rbNum == 'freq'",
                                             tableOutput("freqSummary")
                            )
                            
                  )
                  
                )
                
        ),
       
        
        ###############
        #  Model Tab  #
        ###############
        tabItem(tabName="model",
                h1("Modeling"),
                br(),
                tabsetPanel(
                  tabPanel("Modeling Info",
                          fluidPage(
                            withMathJax(),
                            sidebarPanel(
                              radioButtons("selectModelInfo", strong("Select the Model of Interest"),
                                           choices = c("Logistic Regression Model" = "mod_glm",
                                             "Classification Tree Model" = "mod_tree",
                                             "Random Forest Model" = "mod_rf")
                              )
                            ),
                            
                            mainPanel(
                              h2("Modeling Info"),
                              
                              conditionalPanel(condition = "input.selectModelInfo == 'mod_glm'",
                                h3("Logistic Regression Model"),
                                "The logistic regression model is an example of a broad class of models known as Generalized Linear Models (GLM). 
                                For example, GLM also include linear regression, ANOVA, poisson regression, etc. Logistic regression measures the relationship 
                                between the dependent variable and one or more independent variables(features) by estimating probabilities using the underlying 
                                logit function. The logit function or the log-odds is the logarithm of the odds.",
                                br(),
                                br(),
                                "We can look at the formula: ",
                                br(),
                                helpText('$$ ln(\\frac{P(Y=1)}{1 - P(Y=1)}) = log(\\frac{P(Y=1)}{P(Y=0)}) 
                                         = \\beta_0 + \\beta_1x_1 + \\beta_2x_2 + ...+ \\beta_nx_n $$' ),
                                br(),
                                "Logistic regression is the appropriate regression analysis to conduct when the dependent variable is binary.
                                 In our case, we want to predict Bankruptcy which has values of 1 or 0.",
                                br(),
                                br(),
                                
                                "Here are the advantages and disadvantages of the Logistic Regression Model: ",
                                h4(strong("Advantages")),
                                tags$ul(
                                  tags$li("Easy to implement and doesn't require high computation power"),
                                  tags$li("Predicted parameters give inferance about the importance of each feature,
                                           with direction of association (positive or negative)"),
                                  tags$li("Outputs well-calibrated probabilities along with classification results,
                                           instead of just the final classification results")
                                ),
                                br(),
                                
                                h4(strong("Disadvantages")),
                                tags$ul(
                                  tags$li("May overfit on high dimensional datasets"),
                                  tags$li("Non-linear problems can’t be solved because it has a linear decision surface"),
                                  tags$li("Sensitive to outliers")
                                )  
                              ),
                              
                              conditionalPanel(condition = "input.selectModelInfo == 'mod_tree'",
                                h3("Classification Tree Model"),
                                "Decision Trees are a non-parametric supervised learning method used for classification and regression. 
                                 The goal is to create a model that predicts the value of a target variable by learning simple decision rules 
                                 inferred from the data features. A tree can be seen as a piecewise constant approximation. The classification tree is a form of decision tree. 
                                 While designing the decision tree, the features possessing the least value of the Gini Index would get preferred.
                                 The Gini Index is determined by deducting the sum of squared of probabilities of each class from one, mathematically, Gini Index can be expressed as: ",
                                br(),
                                helpText('$$\\text{Gini Index} = 1 - \\sum^n_{i=1}(P_i)^2 $$'),
                                "Where Pi denotes the probability of an element being classified for a distinct class.",
                                br(),
                                br(),
                                
                                "Classification tree models are recommended when the data mining task contains classifications or predictions of outcomes, 
                                 and the goal is to generate rules that can be easily explained.",
                                br(),
                                br(),
                                
                                "Here are the advantages and disadvantages of the Classification Tree Model: ",
                                h4(strong("Advantages")),
                                tags$ul(
                                  tags$li("very intuitive and easy to interpret"),
                                  tags$li("Does not require normalization or scaling of data"),
                                  tags$li("Easy to implement and requires much less computation power compared to Random Forest Model")
                                ),
                                br(),
                                
                                h4(strong("Disadvantages")),
                                tags$ul(
                                  tags$li("A small change in the data can cause a large change in the structure of the decision tree causing instability"),
                                  tags$li("Overfitting, to fit the data it keeps generating new nodes
                                           and ultimately the tree becomes too complex to interpret, and loses its generalization capabilities"),
                                  tags$li("Affected by noise - little bit of noise can make it unstable which leads to wrong predictions.")
                                )  
                              ),
                              
                              conditionalPanel(condition = "input.selectModelInfo == 'mod_rf'",
                                h3("Random Forest Model"),
                                "The random forest model is a classification algorithm that consists of many decision trees.
                                 It creates multiple (e.g. 500) bootstrapped samples and fit decision trees to each of the bootstrapped samples.
                                 When selecting a split point, the learning algorithm selects a random sample of predictors of which to search,
                                 instead of all the predictors. By not looking at all the predictors every time,
                                 it prevents one or two strong predictors to dominate the tree fits. 
                                 The prediction of trees are then averaged (for regression) to get the final predicted value.
                                 This would lead to less variance and better fit over an individual tree fit.",
                                br(),
                                "Regarding the number of predictors to look for, a good rule of thumb is 
                                $$m = \\sqrt{(p)} \\text{  for classification and  } m = \\frac{p}{3} \\text{  for regression} $$",
                                br(),
                                "where m is the number of randomly selected features at each point, and p is the number of input variables.",
                                br(),
                                br(),
                                
                                "Here are the advantages and disadvantages of the Random Forest Model: ",
                                h4(strong("Advantages")),
                                tags$ul(
                                  tags$li("Reduces overfitting in decision trees and helps to improve the accuracy"),
                                  tags$li("Works well with both categorical and continuous values,
                                           and is flexible to both classification and regression problems"),
                                  tags$li("Does not require normalization or scaling of data")
                                ),
                                br(),
                                
                                h4(strong("Disadvantages")),
                                tags$ul(
                                  tags$li("Longer Training Period - it requires much more time to train as compared to classification trees 
                                           as it generates a lot of trees and makes decision on the majority of votes"),
                                  tags$li("Hard to interpret - since it involves averaging or taking the majority of multiple trees")
                                )  
                              ),                              
                              
                            )
                            
                          )        
                  ),
                  
                  
                  tabPanel("Modeling Fitting",
                          
                          fluidPage(
                            sidebarPanel(
                              sliderInput("trainpct",
                                          "Select % of Data to use for Training Set",
                                          min = 50,
                                          max = 90,
                                          value = 75),
                              br(),
                              
                              sliderInput("folds",
                                          "Select Number of Cross-Validation Folds",
                                          min = 3,
                                          max = 10,
                                          value = 5),
                            
                            
                     
                              ## Generalized Linear Model ##
                              h3("Logistic Regression Parameters"),
                              pickerInput("glmVar", strong("Select Variables for this Model"),
                                 choices = c("(1) ROA C Before Interest and Depreciation Before Interest" = "ROA.C..before.interest.and.depreciation.before.interest",
                                             "(2) Net Value Per Share B" = "Net.Value.Per.Share..B.",
                                             "(3) Net Value Per Share C" = "Net.Value.Per.Share..C.",
                                             "(4) Persistent EPS in the Last Four Seasons" = "Persistent.EPS.in.the.Last.Four.Seasons",
                                             "(5) Per Share Net Profit Before Tax" = "Per.Share.Net.profit.before.tax..Yuan.Â..",
                                             "(6) Total Debt Total Net Worth" = "Total.debt.Total.net.worth",
                                             "(7) Debt Ratio" = "Debt.ratio..",
                                             "(8) Borrowing Dependency" = "Borrowing.dependency",
                                             "(9) Total Asset Turnover" = "Total.Asset.Turnover",
                                             "(10) Fixed Assets Turnover Frequency" = "Fixed.Assets.Turnover.Frequency",
                                             "(11) Cash Total Assets" = "Cash.Total.Assets",
                                             "(12) Current Liabilities Liability" = "Current.Liabilities.Liability",
                                             "(13) Retained Earnings to Total Assets" = "Retained.Earnings.to.Total.Assets",
                                             "(14) Total Expense Assets" = "Total.expense.Assets",
                                             "(15) Cash Turnover Rate" = "Cash.Turnover.Rate",
                                             "(16) Net Income to Total Assets" = "Net.Income.to.Total.Assets" 
                                            ),
                                 
                                 selected = c("Persistent.EPS.in.the.Last.Four.Seasons",
                                              "Debt.ratio..",
                                              "Borrowing.dependency",
                                              "Total.Asset.Turnover",
                                              "Fixed.Assets.Turnover.Frequency",
                                              "Cash.Total.Assets",
                                              "Cash.Turnover.Rate",
                                              "Net.Income.to.Total.Assets"
                                             ),
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE
                              ),
                              
                              checkboxInput("allInteraction", strong("Include all first-order Interactions in the Model"), FALSE),
                              br(),
                              
                              ## Classification Tree Model ##
                              h3("Classification Tree Parameters"),
                              pickerInput("treeVar", strong("Select Variables for this Model"),
                                  choices = c("(1) ROA C Before Interest and Depreciation Before Interest" = "ROA.C..before.interest.and.depreciation.before.interest",
                                              "(2) Net Value Per Share B" = "Net.Value.Per.Share..B.",
                                              "(3) Net Value Per Share C" = "Net.Value.Per.Share..C.",
                                              "(4) Persistent EPS in the Last Four Seasons" = "Persistent.EPS.in.the.Last.Four.Seasons",
                                              "(5) Per Share Net Profit Before Tax" = "Per.Share.Net.profit.before.tax..Yuan.Â..",
                                              "(6) Total Debt Total Net Worth" = "Total.debt.Total.net.worth",
                                              "(7) Debt Ratio" = "Debt.ratio..",
                                              "(8) Borrowing Dependency" = "Borrowing.dependency",
                                              "(9) Total Asset Turnover" = "Total.Asset.Turnover",
                                              "(10) Fixed Assets Turnover Frequency" = "Fixed.Assets.Turnover.Frequency",
                                              "(11) Cash Total Assets" = "Cash.Total.Assets",
                                              "(12) Current Liabilities Liability" = "Current.Liabilities.Liability",
                                              "(13) Retained Earnings to Total Assets" = "Retained.Earnings.to.Total.Assets",
                                              "(14) Total Expense Assets" = "Total.expense.Assets",
                                              "(15) Cash Turnover Rate" = "Cash.Turnover.Rate",
                                              "(16) Net Income to Total Assets" = "Net.Income.to.Total.Assets" 
                                  ),
                                  
                                  selected = c("Persistent.EPS.in.the.Last.Four.Seasons",
                                               "Debt.ratio..",
                                               "Borrowing.dependency",
                                               "Total.Asset.Turnover",
                                               "Fixed.Assets.Turnover.Frequency",
                                               "Cash.Total.Assets",
                                               "Cash.Turnover.Rate",
                                               "Net.Income.to.Total.Assets"
                                  ),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                              ),
                              radioButtons("treeMethod", strong("Select the Methodology for Splitting"),
                                           choices=c("Gini Index" = "gini",
                                                     "Information Gain" = "info"
                                           )
                              ),
                              br(),
                              
                              ## Random Forest Model ##
                              h3("Random Forest Parameters"),
                              pickerInput("rfVar", strong("Select Variables for this Model"),
                                  choices = c("(1) ROA C Before Interest and Depreciation Before Interest" = "ROA.C..before.interest.and.depreciation.before.interest",
                                              "(2) Net Value Per Share B" = "Net.Value.Per.Share..B.",
                                              "(3) Net Value Per Share C" = "Net.Value.Per.Share..C.",
                                              "(4) Persistent EPS in the Last Four Seasons" = "Persistent.EPS.in.the.Last.Four.Seasons",
                                              "(5) Per Share Net Profit Before Tax" = "Per.Share.Net.profit.before.tax..Yuan.Â..",
                                              "(6) Total Debt Total Net Worth" = "Total.debt.Total.net.worth",
                                              "(7) Debt Ratio" = "Debt.ratio..",
                                              "(8) Borrowing Dependency" = "Borrowing.dependency",
                                              "(9) Total Asset Turnover" = "Total.Asset.Turnover",
                                              "(10) Fixed Assets Turnover Frequency" = "Fixed.Assets.Turnover.Frequency",
                                              "(11) Cash Total Assets" = "Cash.Total.Assets",
                                              "(12) Current Liabilities Liability" = "Current.Liabilities.Liability",
                                              "(13) Retained Earnings to Total Assets" = "Retained.Earnings.to.Total.Assets",
                                              "(14) Total Expense Assets" = "Total.expense.Assets",
                                              "(15) Cash Turnover Rate" = "Cash.Turnover.Rate",
                                              "(16) Net Income to Total Assets" = "Net.Income.to.Total.Assets" 
                                  ),
                                  
                                  selected = c("Persistent.EPS.in.the.Last.Four.Seasons",
                                               "Debt.ratio..",
                                               "Borrowing.dependency",
                                               "Total.Asset.Turnover",
                                               "Fixed.Assets.Turnover.Frequency",
                                               "Cash.Total.Assets",
                                               "Cash.Turnover.Rate",
                                               "Net.Income.to.Total.Assets"
                                  ),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                              ),
                              br(),
                              sliderInput("ntree",
                                          "Select Number of Trees",
                                          min = 300,
                                          max = 1000,
                                          value = 500,
                                          step = 50),
                              br(),
                              
                              h4("Click button below to fit all 3 Models"),
                              
                              # Action button for fitting all models
                              actionButton("startbutton", "Fit Models"),
                           
                            ),  
                               
                            mainPanel(
                              h2("Model Fitting"),
                              
                              tabsetPanel(
                                tabPanel("GLM Summary",
                                  h3("GLM Model Summary"),
                                  verbatimTextOutput("glmSummary"),
                                  
                                  h3("GLM Confusion Matrix Output"),
                                  verbatimTextOutput("glmMatrix")
                                ),
                                
                                tabPanel("Tree Summary",
                                  h3("Tree Diagram"),
                                  plotOutput("treeDiagram"),
                                  
                                  h3("Final Tree Model Summary"),
                                  verbatimTextOutput("treeOutput"), 
                                  
                                  h3("Tree Confusion Matrix Output"),
                                  verbatimTextOutput("treeMatrix")
                                ),
                                
                                tabPanel("Random Forest Summary",
                                  h3("Graph of Most Important Features"),
                                  plotOutput("rfSummary"),       
                                  
                                  h3("Final Random Forest Model Summary and Confusion Matrix"),
                                  verbatimTextOutput("rfOutput")
                                ),
                                
                                tabPanel("Comparison",
                                  h3("Comparison of Models on Test Data"),
                                  verbatimTextOutput("accTest"),
                                  br(),
                                  
                                  h3("Confusion Matrix of GLM Model on Test Data"),
                                  verbatimTextOutput("glmMatrixTest"),
                                  br(),
                                  
                                  h3("Confusion Matrix of Tree Model on Test Data"),
                                  verbatimTextOutput("treeMatrixTest"),
                                  br(),
                                  
                                  h3("Confusion Matrix of Random Forest Model on Test Data"),
                                  verbatimTextOutput("rfMatrixTest")
                                
                                )
                              
                              )
                              
                            )
                         
                          )        
                  ),
                  
                  ################
                  #  Prediction  #
                  ################
                  tabPanel("Prediction",
                           
                           fluidPage(
                             
                             sidebarPanel(
                               radioButtons("chooseModel", strong("Choose Model to use for Prediction"),
                                            choices = c("Generalized Linear Regression Model" = "predglm",
                                                        "Classification Tree Model" = "predtree",
                                                        "Random Forest Model" = "predrf"
                                            )
                               ),
                               
                               # Add button to start prediction
                               actionButton("predStart", "Predict"),
                               
                               conditionalPanel(condition = "input.chooseModel == 'predglm'",
                                                h3("Model: Generalized Linear Regression Model"),
                                                h3("Input Parameter Values for Prediction"),
                                                h4("(The default is the mean)"),
                                                uiOutput("glmPredInput")
                               ),
                               
                               conditionalPanel(condition = "input.chooseModel == 'predtree'",
                                                h3("Model: Classification Tree Model"),
                                                h3("Input Parameter Values for Prediction"),
                                                h4("(The default is the mean)"),
                                                uiOutput("treePredInput")
                               ),
                               
                               conditionalPanel(condition = "input.chooseModel == 'predrf'",
                                                h3("Model: Random Forest Model"),
                                                h3("Input Parameter Values for Prediction"),
                                                h4("(The default is the mean)"),
                                                uiOutput("rfPredInput")
                               ),
                               
                             ),
                             
                             mainPanel(
                               h2("Prediction"),
                               tableOutput("predTable")
                             )
                             
                           )        
                  )
                )
                
        ),
        
        
        #######################
        #  Data Download Tab  #
        #######################
        tabItem(tabName="dataDownload",
                h1("Data Download"),
                
                fluidPage(
                  sidebarPanel(
                    pickerInput("filter_column", "Select Columns to Filter",
                                choices = colnames(bankdata),
                                selected = colnames(bankdata),
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE
                    ),
                    
                    radioButtons("filter_bankrupt", "Select Rows to Filter",
                                 choices = c("All Rows" = "allrows",
                                             "Bankrupt = 0" = "bankrupt0",
                                             "Bankrupt = 1" = "bankrupt1"
                                 )
                    ),
                    
                    
                    # Create download button to download data set
                    downloadBttn("downloadData", "Download")
                  ),
                  

                  mainPanel(
                    dataTableOutput("filtered_data")
                    
                  )
                )
                
        )
     )
      
    
  )
  
  
))

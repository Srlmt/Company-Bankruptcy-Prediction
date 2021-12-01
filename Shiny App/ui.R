library(ggplot2)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(rattle)

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

  
     tabItems(
        tabItem(tabName="about",
                
                
                h2("Company Bankruptcy Prediction"),
                br(),
                
                img(src='bankruptcy.jpg'),
                br(),
                
                h3("Purpose"),
                br(),
                
                "Data and source... link",
                br(),
                
                "Purpose of each tab",
                br(),
                
                "Picture..."
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
                            
                            sidebarPanel(
                              radioButtons("selectModelInfo", strong("Select the Model of Interest"),
                                           choices = c("Generalized Linear Regression Model" = "mod_glm",
                                             "Classification Tree Model" = "mod_tree",
                                             "Random Forest Model" = "mod_rf")
                              )
                            ),
                            
                            mainPanel(
                              h2("Modeling Info"),
                              
                              conditionalPanel(condition = "input.selectModelInfo == 'mod_glm'",
                                h3("Generalized Linear Regression Model (GLM)")               
                              ),
                              
                              conditionalPanel(condition = "input.selectModelInfo == 'mod_tree'",
                                h3("Classification Tree Model")               
                              ),
                              
                              conditionalPanel(condition = "input.selectModelInfo == 'mod_rf'",
                                h3("Random Forest Model")               
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
        
        
        
        
        tabItem(tabName="dataDownload",
                h1("Data Download"),
                
                fluidPage(
                  sidebarPanel(
                    pickerInput("filter_column", "Select Columns to Filter",
                                choices = colnames(bankdata)[2:96],
                                selected = colnames(bankdata)[2:96],
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE
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

library(ggplot2)
library(DT)
library(shinydashboard)

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
                h2("Company Bankruptcy Prediction")
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
                                c("Operating Profit Rate" = "Operating.Profit.Rate",
                                  "Operating Gross Margin" = "Operating.Gross.Margin",
                                  "Debt Ratio" = "Debt.ratio..",
                                  "Equity to Liability" = "Equity.to.Liability",
                                  "Revenue per Person" = "Revenue.per.person",
                                  "Cash Flow Rate" = "Cash.flow.rate",
                                  "Inventory Turnover Rate Times" = "Inventory.Turnover.Rate..times.",
                                  "Current Liability to Current Assets" = "Current.Liability.to.Current.Assets",
                                  "Cash Total Assets" = "Cash.Total.Assets",
                                  "Realized Sales Gross Margin" = "Realized.Sales.Gross.Margin" 
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
                              textOutput("modelInfo")
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
                                          value = 75)
                            
                            ),
                            
                            mainPanel(
                              h2("Model Fitting")
                              
                            )
                            
                          )        
                  ),
                  
                  
                  tabPanel("Prediction",
                           
                           fluidPage(
                             
                             sidebarPanel(
                               
                               
                             ),
                             
                             mainPanel(
                               h2("Prediction")
                               
                             )
                             
                           )        
                  )
                )
                
                
                
                
                
                
                
        ),
        
        
        
        
        
        
        
        
        tabItem(tabName="dataDownload",
                h1("Data Download"))
     )
      
    
  )
  
  
  
  
))

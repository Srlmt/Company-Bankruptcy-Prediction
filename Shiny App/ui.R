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
                h2("Data Exploration"),
                fluidPage(
                  sidebarPanel(
                    selectInput("selectVar", "Select the Variable of Interest",
                                c("Operating Profit Rate" = "Operating.Profit.Rate",
                                  "Debt Ratio" = "Debt.ratio..",
                                  "Equity to Liability" = "Equity.to.Liability"
                                  
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
                  
                  mainPanel(conditionalPanel(condition = "input.rbPlot == 'box'",
                                             plotOutput("BoxPlot")
                            ),
                            conditionalPanel(condition = "input.rbPlot == 'hist'",
                                             plotOutput("Histogram")               
                            ),
                            br(),
                            
                            
                            h3("Numeric Summary"),
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
                h2("Modeling"),
                fluidPage(
                    sidebarPanel(
                        
                        
                    ),
                    
                    mainPanel(
                        dataTableOutput("dataTable")
                        
                    )
                    
                )
                
        ),
        
        tabItem(tabName="dataDownload",
                h2("Data Download"))
     )
      
    
  )
  
  
  
  
))

library(ggplot2)
library(DT)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  
  dashboardHeader(title = "Company Bankruptcy Prediction"),
  
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
                h2("Tab Name About")
        ),
       
       
        tabItem(tabName="EDA",
                h2("Tab Name EDA"),
                fluidPage(
                  sidebarPanel(
                    sliderInput("obs",
                                "Number of observations:",
                                min = 0,
                                max = 1000,
                                value = 500)
                  ),
                  
                  mainPanel(dataTableOutput("dataTable"),
                            plotOutput("testPlot1")
                            
                  )
                  
                )
                
        ),
       
        tabItem(tabName="model",
                h2("Tab Name Model")        
                
        ),
        
        tabItem(tabName="dataDownload",
                h2("Tab Name Data Download"))
     )
      
    
  )
  
  
  
  
))

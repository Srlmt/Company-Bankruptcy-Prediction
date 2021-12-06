# Company-Bankruptcy-Prediction

## Overview

This app examines the Taiwanese Bankruptcy Prediction Data Set from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Taiwanese+Bankruptcy+Prediction#)  to construct models to try to predict whether companies are bankrupt or not. It includes Data Exploration, Modeling, and Data Download. 

## Required Packages

The following packages are required to run this Shiny App:

- `shiny`: Web Application Framework
- `shinydashboard`: Create dashboard and provides a theme on top of 'Shiny'
- `shinyWidgets`: Custom Inputs Widgets for Shiny
- `dplyr`: Set of functions to perform common data manipulation operations
- `ggplot2`: A system for creating graphics
- `DT`: Provides an R interface to the JavaScript library DataTables
- `caret`: Functions to streamline the model training process for complex regression and classification problems
- `rattle`: Provides a GUI interface to R functionality

This line of code can be used to install these packages:
```
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "dplyr", "ggplot2", "DT", "caret", "rattle"))
```

## Code to Run the App
```
shiny::runGitHub("Srlmt/Company-Bankruptcy-Prediction", ref="main", subdir="Shiny App")
```

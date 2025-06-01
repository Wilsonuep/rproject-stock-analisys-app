# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(quantmod)
library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(zoo)
library(xts)
#Libraries for predictions
library(forecast)
library(randomForest)
library(xgboost)
library(yardstick)
library(dplyr)

# Load helper functions
source("helpers/data_functions.R")
source("helpers/plotting_functions.R")
source("helpers/prediction_models.R")

# Define any global variables or helper functions
# This file is loaded before ui.R and server.R

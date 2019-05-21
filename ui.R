#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(DT)
library(devtools)
library(readr)
library(tidyverse)
library(readxl)
library(forecast)
library(imputeTS)
library(VIM)
library(bsts)
source('global.R')

masterData <- read_csv("Master.csv",na = c("0", "NA"))

head(masterData)


# Define UI for application that draws a histogram
shinyUI <- fluidPage(
  
  
  # Application title
  fluidRow(
  align = "center",  
  titlePanel("Sales predictions using various models")
  ),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #to include scroll down in the sidebarPanel
      tags$head(tags$style(
        type = 'text/css',
        'form.well { max-height: 850px; overflow-y: auto; }'
      )),
      
      radioButtons("radioProduct", "Products to show:",
                   choiceNames = paste(masterData$Product,'_',masterData$Description),
                   choiceValues=masterData$Product,selected=masterData$Product[1]),
      width = 2),
    
   
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
     radioButtons("radioModel", "Select Model:",choiceNames = c("ARIMA","ETS","Bayesian","Neural Network"),
                   choiceValues=c("A","E","B","N"),selected="A", inline = TRUE),
      splitLayout(cellWidths = c("70%", "40%"), plotOutput("tsplot", height=400, width = 850 ), 
                  div(DT::dataTableOutput("table1"), style="width:60%")),
     splitLayout(cellWidths = c("70%", "40%"),  plotOutput("ggplot", height=400, width = 800), 
                 div(DT::dataTableOutput("table2"), style="width:50%"))
     
    )
      
    
  )
  )
)



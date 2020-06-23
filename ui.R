library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("R0"),
    
    uiOutput("pref"), 
    

     #Sidebar with a slider input for number of bins
    sidebarLayout(
         sidebarPanel(
            sliderInput("lp",
                         "潜伏期間(日)",
                         min = 5,
                         max = 14,
                         value = 10),
            sliderInput("ip",
                        "感染期間(日)",
                        min = 8,
                        max = 28,
                        value = 21)
            
         ),
         
    
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plot")
        )
    )
))


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("cri1", "Criteria 1", min = 0, max = 100, value = 30),
       sliderInput("cri2", "Criteria 2", min = 0, max = 100, value = 20),
       sliderInput("cri3", "Criteria 3", min = 0, max = 100, value = 15),
       sliderInput("cri4", "Criteria 4", min = 0, max = 100, value = 5),
       sliderInput("cri5", "Criteria 5", min = 0, max = 100, value = 15),
       sliderInput("cri6", "Criteria 6", min = 0, max = 100, value = 5),
       sliderInput("cri7", "Criteria 7", min = 0, max = 100, value = 10),
       actionButton("reset", label = "Reset the weights")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))

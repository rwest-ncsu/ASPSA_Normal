#
# This is an app designed to be used for ASPSA students enrolled in
# STOR 151-155 at UNC
#
# Designed by: Robert West
# Date: 6/15/2021

library(shiny)
library(shinydashboard)

problemTypes = c("P{X < a}", 
                 "P{X > a}", 
                 "P{|X| < a}", 
                 "P{|X| > a}", 
                 "P{a < X < b}")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Visualize area under a Normal Distribution"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "mean",
        "Mean of the Normal Distribution",
        min = -Inf,
        max = Inf,
        value = 0, 
        step=0.1
      ),
      numericInput(
        "sd",
        "Standard Deviation of the Normal Distribution",
        min=0.1, 
        max=Inf,
        value=1, 
        step=0.1
      ),
      selectInput(
        "problemType",
        "Problem Type",
        choices = problemTypes,
        selected = "P{X < a}"
      ), 
      conditionalPanel(condition = "input.problemType == 'P{X > a}'",
                       numericInput("x>a", 
                                    "Select a:", 
                                    min=-Inf,
                                    max=Inf,
                                    value=1,
                                    step=0.01)),
      conditionalPanel(condition = "input.problemType == 'P{X < a}'", 
                       numericInput("x<a", 
                                    "Select a:", 
                                    min=-Inf,
                                    max=Inf,
                                    value=1,
                                    step=0.01)), 
      conditionalPanel(condition = "input.problemType == 'P{a < X < b}'", 
                       numericInput("lowerBound",
                                    "Select a lower bound:",
                                    min=-Inf,
                                    max=Inf,
                                    value=0,
                                    step=0.1),
                       numericInput("bValue", 
                                    "Select an upper bound:",
                                    min = -Inf,
                                    max=Inf,
                                    value=1,
                                    step=0.1)),
                       # uiOutput("lowerBound"),
                       # uiOutput("bValue")), 
      conditionalPanel(condition = "input.problemType == 'P{|X| > a}'",
                       numericInput("absOuter",
                                    "Select a:",
                                    min=0, 
                                    max=Inf,
                                    value=1,
                                    step=0.1)), 
      conditionalPanel(condition = "input.problemType == 'P{|X| < a}'", 
                       numericInput("absInner",
                                    "Select a:",
                                    min=0, 
                                    max=Inf,
                                    value=1,
                                    step=0.1))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
              box(
                width=12,
                title="Probability = ",
                textOutput("prob"))
      )
  )
))

#
# This is an app designed to be used for ASPSA students enrolled in
# STOR 151-155 at UNC
#
# Designed by: Robert West
# Date: 6/15/2021

library(shiny)

problemTypes = c("X < a", "X > a", "|X| < a", "|X| > a", "a < X < b")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Visualize area under a normal Distribution"),
  
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
        selected = "X > a"
      ), 
      conditionalPanel(condition = "input.problemType == 'X > a'",
                       numericInput("xBiggerA", 
                                    "Select a:", 
                                    min=-Inf,
                                    max=Inf,
                                    value=1,
                                    step=0.01))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("plot"),
              textOutput("TEST"))
  )
))

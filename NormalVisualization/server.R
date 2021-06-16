#
# This is an app designed to be used for ASPSA students enrolled in
# STOR 151-155 at UNC
#
# Designed by: Robert West
# Date: 6/15/2021

library(shiny)
library(ggplot2)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  x = reactive({
    seq(input$mean - 4 * input$sd, input$mean + 4 * input$sd, by = 0.01)
  })
  y = reactive({
    dnorm(x(), mean = input$mean, sd = input$sd)
  })
  test = reactive({
    data.frame(x = x(),
               y = y())
  })
  output$bValue = renderUI({
    numericInput(
      "upperBound",
      "Select an upper bound:",
      min = input$lowerBound,
      max = Inf,
      value = input$lowerBound + 0.5,
      step = 0.1
    )
  })
  
  g = reactive({
    ggplot(test(), mapping = aes(x = x, y = y)) +
      geom_line(color = "black") +
      scale_y_continuous(limits = c(0, dnorm(
        input$mean,
        mean = input$mean,
        sd = input$sd))) +
      theme_bw() +
      theme(panel.grid = element_blank())
  })
  
  output$plot = renderPlot({
    if (input$problemType == "X > a") {
      g() + 
        geom_area(
          data = filter(test(), x > input$'x>a'),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          x = quantile(test()$x, 0.75),
          y = 0.75 * max(test()$y),
          label = "Area to the right of a \n under the curve")
    }
    else if (input$problemType == "X < a") {
      g() +
        geom_area(
          data = filter(test(), x < input$'x<a'),
          fill = "#7BAFD4",
          alpha = 0.5
        ) +
        annotate(
          "text",
          x = quantile(test()$x, 0.25),
          y = 0.75 * max(test()$y),
          label = "Area to the left of a \n under the curve"
        )
    }
    else if (input$problemType == "|X| < a") {
      g() + 
        geom_area(
          data = filter(test(), abs(x) < input$absInner),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          x = quantile(test()$x, 0.75),
          y = 0.75 * max(test()$y),
          label = "Area between the values -a and a \n under the curve")
    }
    else if(input$problemType == "|X| > a") {
      g() + 
        geom_area(
          data = filter(test(),x > input$absOuter),
          fill = "#7BAFD4",
          alpha = 0.5) +
        geom_area(
          data = filter(test(), x < -input$absOuter),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          x = quantile(test()$x, 0.75),
          y = 0.75 * max(test()$y),
          label = "Area to the left of -a \n and to the right of a  \n under the curve")
    }
    else if(input$problemType == "a < X < b"){
      g() + 
        geom_area(
          data=filter(test(), x>input$lowerBound), 
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          x = quantile(test()$x, 0.75),
          y = 0.75 * max(test()$y),
          label = "Area between a and b \n under the curve")
    }
  })
  
  output$TEST = renderText({input$bValue})
  
})

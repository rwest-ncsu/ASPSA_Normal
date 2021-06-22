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
  df = reactive({
    data.frame(x = x(),
               y = y())
  })
  
  #Condition the upper bound on the lower bound
  output$bValue = renderUI({
    numericInput("bValue", 
                "Select an upper bound:",
                min = input$lowerBound,
                max=Inf,
                value=1,
                step=0.1)
  })
  
  #Only allow user to select lower bound for minimum of upper bound
  observeEvent(input$lowerBound, {
      updateNumericInput(inputId = "bValue", min=input$lowerBound)
  })
  
  #Create the base plot
  g = reactive({
    ggplot(df(), mapping = aes(x = x, y = y)) +
      geom_line(color = "black") +
      scale_y_continuous(limits = c(0, dnorm(
        input$mean,
        mean = input$mean,
        sd = input$sd))) +
      theme_bw() +
      theme(panel.grid = element_blank())+
      scale_x_continuous(n.breaks = 10)
  })
  
  #Add layers conditioned on user input
  output$plot = renderPlot({
    if (input$problemType == "P{X > a}") {
      g() + 
        geom_area(
          data = filter(df(), x > input$'x>a'),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          size=6,
          x = quantile(df()$x, 0.75),
          y = 0.75 * max(df()$y),
          label = "Area to the right of a \n under the curve")
    }
    else if (input$problemType == "P{X < a}") {
      g() +
        geom_area(
          data = filter(df(), x < input$'x<a'),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          size=6,
          x = quantile(df()$x, 0.25),
          y = 0.75 * max(df()$y),
          label = "Area to the left of a \n under the curve")
    }
    else if (input$problemType == "P{|X| < a}") {
      g() + 
        geom_area(
          data = filter(df(), abs(x) < input$absInner),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          size=6,
          x = quantile(df()$x, 0.75),
          y = 0.75 * max(df()$y),
          label = "Area between the values -a and a \n under the curve")
    }
    else if(input$problemType == "P{|X| > a}") {
      g() + 
        geom_area(
          data = filter(df(),x > input$absOuter),
          fill = "#7BAFD4",
          alpha = 0.5) +
        geom_area(
          data = filter(df(), x < -input$absOuter),
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          size=6,
          x = quantile(df()$x, 0.75),
          y = 0.75 * max(df()$y),
          label = "Area to the left of -a \n and to the right of a  \n under the curve")
    }
    else if(input$problemType == "P{a < X < b}"){
      g() + 
        geom_area(
          data=filter(df(), x>input$lowerBound & x<input$bValue), 
          fill = "#7BAFD4",
          alpha = 0.5) +
        annotate(
          "text",
          size=6,
          x = quantile(df()$x, 0.75),
          y = 0.75 * max(df()$y),
          label = "Area between a and b \n under the curve")
    }
  })
  
  probability = reactive({
    if(input$problemType=="P{X < a}"){
      pnorm(input$'x<a', mean=input$mean, sd=input$sd)
    } else if(input$problemType=="P{X > a"){
      pnorm(input$'x>a', mean=input$mean, sd=input$sd, lower.tail = F)
    } else if(input$problemType=="P{|X| < a}"){
      pnorm(input$absInner, mean=input$mean, sd=input$sd)-
      pnorm(-input$absInner, mean=input$mean, sd=input$sd)
    } else if(input$problemType=="P{|X| > a}"){
      pnorm(input$absOuter, mean=input$mean, sd=input$sd, lower.tail = F)+
      pnorm(-input$absOuter, mean=input$mean, sd=input$sd)
    } else {
      pnorm(input$bValue, mean=input$mean, sd=input$sd) - 
      pnorm(input$lowerBound, mean=input$mean, sd=input$sd)
    }
  })
  
  output$prob = renderText({
    probability()
  })
  
})

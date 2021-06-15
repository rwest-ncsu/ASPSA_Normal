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
  
  output$plot = renderPlot({
    ggplot(test(), mapping = aes(x = x, y = y)) +
      geom_line(color = "black") +
      geom_area(data = filter(test(), abs(x) < 1),
                fill = "#7BAFD4",
                alpha = 0.5) + 
      #Put the mean here for the max Y value
      scale_y_continuous(limits = c(0, dnorm(input$mean, 
                                             mean=input$mean, 
                                             sd=input$sd))) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      annotate("text",
               x = 0.75 * max(test()$x),
               y = 0.75 * max(test()$y),
               label = "Area between 2 points \n under the curve \n on the X axis")
  })
  
  output$TEST = renderText("Hello")
  
})

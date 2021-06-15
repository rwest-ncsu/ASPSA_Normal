# Playing around with a distribution probability image
library(ggplot2)
library(dplyr)
library(tidyr)

#Input mean, standard dev, problem type

#Problem type looks like: 
#X > a
#a < X < b
#|X| < a
#|X| > a
# X<b AND X>a

#generate a sequence based on the mean and sd.
#X = seq()
x=seq(-4, 4, 0.01)
y=dnorm(x)
a=-1
b=2

myData = data.frame(x=x, y=y)

ggplot(NULL, mapping=aes(x=x, y=y))+
  geom_line(color="black")+
  geom_area(data=filter(myData, abs(x) < b), fill="#7BAFD4", alpha=0.5)+ #Go Heels
  #Put the mean here for the max Y value
  scale_y_continuous(limits=c(0, dnorm(0)))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  annotate("text", x=0.75*max(x), y=0.75*max(y), label="Area between 2 points \n under the curve \n on the X axis")
  

## Include what the commands in Excel are giving you: 
#  Need to have norm.dist(cumulative={T/F})
# -> point to the area or the function height depending on which









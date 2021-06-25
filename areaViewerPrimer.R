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
x=seq(-4, 4, 0.01)
y=dnorm(x)
a=-1.5
b=2

myData = data.frame(x=x, y=y)
filtData = filter(myData, x>a)

ggplot(NULL, mapping=aes(x=x, y=y))+
  geom_area(data=filtData, fill="#7BAFD4", alpha=0.5)+ #Go Heels
  #Put the mean here for the max Y value
  scale_y_continuous(limits=c(0, dnorm(0)))+
  theme_bw()+ 
  geom_line(color="black")+
  theme(panel.grid = element_blank())+
  annotate("text", x=0.75*max(x), y=0.75*max(y), label="text goes here")+
  scale_x_continuous(n.breaks = 10) + 
  theme(axis.line = element_line(size = 0.5, linetype = "solid"), 
        axis.ticks = element_line(linetype = "dashed"),
        panel.grid.major = element_line(colour = "gray99",
        size = 1.3, linetype = "dotted"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10, face = "italic"),
        panel.background = element_rect(colour = "antiquewhite"))+
  labs(y = "Density")










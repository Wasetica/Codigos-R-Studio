##########################################
###################### distribucion de Probabilidad binomial


round(dbinom(0,15,0.4),4)
round(dbinom(5,15,0.4),4)

round(dbinom(3,15,0.4)+dbinom(4,15,0.4)+dbinom(5,15,0.4)+dbinom(6,15,0.4),2)
1-(dbinom(0,15,0.4)+dbinom(1,15,0.4)+dbinom(2,15,0.4))

##################################################################################

X<-seq(0:15)
X
plot(x,dbinom(x,15,0.4),type = "h")

plot(dbinom(x, size = 15, prob = 0.4), type = "h", lwd = 2,
     main = "Función de probabilidad binomial",
     ylab = "probabilidad binomial", xlab = "Número de éxitos" )

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
data.frame(X)
ggplot(data.frame(X), aes(x = X, y=dbinom(x,15,0.4))) + 
geom_point(colour="firebrick",shape="square",size=3)+
theme_set(theme_dark())


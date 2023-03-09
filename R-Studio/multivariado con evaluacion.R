##################################################
# Modelo lineal multivariante#####################
##################################################
#Estimacion de los parametros del modelo##########
##################################################
x<-na.omit(est)
x

model<-lm(x$Admitidos~+x$Primiparos+x$Matriculados+x$Graduados+x$Retirados)
model
round(model,2)
summary(est[4:8])
estimados<-fitted(model)
estimados
round(estimados,2)
plot(estimados)
#Los valores del error del modelo
error<-residuals(model)
round(error,2)
plot(error)
#Evaluzación del modelo
anova(model)
summary(model)
################################################################
#Evaluzación de los supuestos del modelo1########################
#################################################################
#################################################
# Supuesto de Multicolinealidad                 #
#Utilizar la matriz de correlaciones            #
# Es necesario observar que las correlaciones   #
#entre las variables sean pequeñas r<0.3        #
#################################################
matriz.corr<-cor(x[4:8])
matriz.corr<-round(matriz.corr,2)
matriz.corr
library(corrplot)
corrplot(matriz.corr, type = "upper")
#No se observa valores de correlacion lineal mayores
# a 0.30 entonces se puede pensar que el modelo
# cumple con el supuesto de multicolinealidad
library(PerformanceAnalytics)
chart.Correlation(x[4:8], histogram = TRUE,pch=19)
error<-round(as.data.frame(error),2)
round(error,2)
plot(error)
###############################
#Supuesto de autocorrelacion  #
#estadistica de Durbin Watson #
###############################
library(lmtest)
dwtest(model)
#como el pvalor es mayor a 0.05 (0.005457), entonces
#se acepta la Ho.por tanto el modelocumple con
#el supuesto de no autocorrelacion
# Supuesto de Homocedasticidad #
######################################################
# Ho: la varianza del modelo permanece "constante"   #
# H1: la variaza del model no es constante           #
######################################################
plot(estimados)
grupos<-numeric()
grupos[estimados<60]<-1
grupos[estimados>=61& estimados<90]<-2
grupos[estimados>=91]<-3
bartlett.test(estimados~grupos)
#como el pvalor es menor a 0.05 entonces se rechaza la
#hipotesis nula, por tanto no se cumple el supuesto de
#homocedasticidad
############################
# supuesto de normalidad
# Ho: Normalidad multivariante
# Hi; No normalidad multivariante
############################
library(MVN)
mvn(x[4:8])
#
###############################################################
###################Graficos ggplot2#############################
#################################################################
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
# basic histogram
######admitidos#################


ggplot(est, aes(est$Admitidos)) + 
  geom_histogram( binwidth=4,fill="#8B008B",color="#9AC0CD" ,  alpha=0.9) +
  ggtitle("admitidos") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))

ggplot(x, aes(x$Admitidos,x$Primiparos)) + 
  geom_boxplot(fill="purple1", alpha=0.3) + 
  xlab("admitidos")+ ylab("primiparos")

ggplot(x, aes(x$Admitidos,x$Matriculados)) + 
  geom_boxplot(fill="red4", alpha=0.3) + 
  xlab("admitidos")+ ylab("Matriculados")

ggplot(x, aes(x$Admitidos,x$Egresados)) + 
  geom_boxplot(fill="orange", alpha=0.3) + 
  xlab("admitidos")+ ylab("Egresados")

ggplot(x, aes(x$Admitidos,x$Graduados)) + 
  geom_boxplot(fill="mediumpurple4", alpha=0.3) + 
  xlab("admitidos")+ ylab("Matriculados")
  
ggplot(x, aes(x$Admitidos, x$Primiparos)) + 
  geom_point(
    color="olivedrab3",
    fill="#EE0000",
    shape=22,
    alpha=0.7,
    size=3,
    stroke =2
    ) +
  theme_ipsum()

ggplot(x, aes(x$Admitidos, x$Primiparos)) + geom_line(color="red2") 
+geom_point(shape=10, color="black", fill="#69b3a2",)

###########inscritos############
ggplot(est, aes(est$Inscritos)) + 
  geom_histogram( binwidth=12,fill="#CD0000",color="#9AC0CD" ,  alpha=0.9) +
  ggtitle("Inscritos") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))
##############Primiparos#############
ggplot(est, aes(est$Primiparos)) + 
  geom_histogram( binwidth=7,fill="#00868B",color="#8B8B00" ,  alpha=0.9) +
  ggtitle("Primiparos") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))
##############Matriculados#############
ggplot(est, aes(est$Matriculados)) + 
  geom_histogram( binwidth=15,fill="#CD4F39",color="#CDB5CD" ,  alpha=0.9) +
  ggtitle("Matriculados") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))
##############Egresados#############
ggplot(x, aes(x$Egresados)) + 
  geom_histogram( binwidth=10,fill="#CDC9C9",color="#00CD66" ,  alpha=0.9) +
  ggtitle("Egresados") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))
##############Egresados#############
ggplot(x, aes(x$Graduados)) + 
  geom_histogram( binwidth=4,fill="#FFE1FF",color="#00CD66" ,  alpha=0.9) +
  ggtitle("Graduados") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))
##############Retirados#############
ggplot(x, aes(x$Retirados)) + 
  geom_histogram( binwidth=6,fill="#008B45",color="#FF0000" ,  alpha=0.9) +
  ggtitle("Retirados") + theme_ipsum() +
  theme(
    plot.title = element_text(size=15))

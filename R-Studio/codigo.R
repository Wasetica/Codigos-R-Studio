##################### distribucion poisson##########################

x<-seq(0:100)
x
data.frame(x)
library(ggplot2)
library(tidyverse)

ggplot(data.frame(x),aes(x,dpois(x,2)))+geom_point(colour = "slateblue3")

#######################  profesor #########################

x<-(2:12)
data<-sample(x,size=500,replace = TRUE)
data
hist(data)


###################################### como hacer una matriz una talbe de doble entrada  ###########################################################

Datos1<-matrix(c(23,34,56,34,65,72,63,87,84,21,48,45,48,78,27,45),nrow=4)
Datos1
rownames(Datos1)<-(c("bogota","bsas","madrid","santos)"))
colnames(Datos1)<-(c("millos","rover","nacional","santafe"))
Datos1


#################################################################################
##################################################################################


#########################################################################################

anime<-table(serie1$Animes)
sum(serie1$Animes)
n0=27
n1=17
n2=18
n3=24
n4=20 
n5=25
N=131
round((n0/N)+(n1/N)+(n2/N)+(n3/N)+(n4/N)+(n5/N),2)
round(n0/N,2)
round(n1/N,2)
round(n2/N,2)
round(n3/N,2)
round(n4/N,2)
FA<-cumsum(anime)

#############################################################################################
######################## GRAficos de las variables ##########################################
#############################################################################################

hist(serie1$Animes)
hist(serie1$Animes, main = "Anime", col = c("blue","red","green"), xlab = "calificacion", ylab = "frecuencia")
hist(serie1$Adultos)
hist(serie1$Adultos, main = "adultos", col = c("blue","red","green"), xlab = "calificacion", ylab = "frecuencia")
hist(serie1$`Ciencia Ficción`)
hist(serie1$`Ciencia Ficción`, main = "Ciencia Ficción", col = c("blue","red","green"), xlab = "calificacion", ylab = "frecuencia")
hist(serie1$Cocina)
hist(serie1$Cocina, main = "Cocina", col = c("blue","red","green"), xlab = "calificacion", ylab = "frecuencia")
plot(serie1$Adultos,serie1$edad)


#############################################################################################
######################## MEdidas centrales ##################################################
#############################################################################################

library(modeest)
medi<-round(mean(serie1$Adultos))
mi<-round(median(serie1$Adultos),2)
varianza<-var(serie1$Adultos)
moda<-mode(serie1$Adultos)
desvi<-sd(serie1$Adultos, na.rm = TRUE)
coev1<-sd(serie1$Adultos)/mean(serie1$Animes)
library(FinCal)
coev2<-coefficient.variation(sd=sd(serie1$Animes), avg = mean(serie1$Animes))

#############################################################################################
######################## Rango del espectro #################################################
#############################################################################################

range(serie1$Animes, na.rm = TRUE)
max(serie1$Animes, na.rm = TRUE) 
min(serie1$Animes, na.rm = TRUE)

#############################################################################################
################################## simetria,kurtosis #######################################
#############################################################################################

library(psych)
coesim<-skew(serie1$Animes)
kur<-kurtosi(serie1$Animes)


#############################################################################################
################################## Todo en uno #############################################
#############################################################################################

library(ggplot2)
library(Lattice)
library(PASWR2)

anime<-table(serie1$Animes)
eda(serie1$Animes) <-- saca 4 graficos, y todo lo anterior

#############################################################################################
################################## Agrupacion #############################################
#############################################################################################

grupos=numeric()
grupos[serie1$edad<=17]<-1
grupos[serie1$edad>=17 & serie1$edad<25]<-2
grupos[serie1$edad>=25 & serie1$edad<45]<-3
grupos[serie1$edad>=45 & serie1$edad<55]<-4
grupos[serie1$edad>=55 & serie1$edad<65]<-5
grupos[serie1$edad>=65 & serie1$edad<80]<-6
table(grupos)

table(serie1$Horas,grupos)
grupos1=numeric()
grupos1[serie1$Horas<=0]<-1
grupos1[serie1$Horas>=1 & serie1$Horas<5]<-2
grupos1[serie1$Horas>=5 & serie1$Horas<10]<-3
grupos1[serie1$Horas>=10 & serie1$Horas<16]<-4
table(grupos1)

########################### comparacion entre edad y horas ################################

table(grupos,grupos1)

#############################################################################################
############################### Experimentacion ############################################
#############################################################################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggrepel)


###################################################################
# Probabilidad conjunta marginal y condicional   #
##################################################################
# Tabla de frenciencia 

datos1<-matrix(c(23,34,56,34,65,72,43,56,78,65,34,56,89,25,35,45), nrow = 4)
datos1
rownames(datos1)<-c("Bogota","bsas","madrid","santos")
colnames(datos1)<-c("millos","river","real","santos")
datos1

###################################################################
# tabla de contigencia de doble entrada #
#################################################################
tabcon<-ftable(datos1)
rownames(tabcon)<-c("bofota","bsas","madrid","usaquen")
colnames(tabcon)<-c("millos","river","real","santos")


prop.table(tabcon)
# suma para la marginal por columnas
addmargins(tabcon,1)
# suma para la marginal por filas
addmargins(tabcon,2)
# Calcular la probabilidad condicional para la tabla de contigencia fila
prop.table(tabcon,1)
round(prop.table(tabcon,2),2)
# ########################
# prueba de independicia chi-cuadrado
###############################
chisq.test(tabcon)

####################################################################


Def= c(1,2,3,4,5,6)
N=6
r=2
resultado<-combinations(N,r,Def)
resultado

s<-permutations(N, r, Def)
s

#############################################################################################
############d istribucion normal ################################
##################################################################
################## simular una muestra aleatoria normal#####################################################
##edad#
da1<-round(rnorm(10,mean = 18,sd =1.5),1)
plot(da1)
hist(da1)
d2<-round(rnorm(100,mean = 18,sd =1.5),1)
plot(d2)
hist(d2)
d3<-round(rnorm(10000,mean = 18,sd =1.5),1)
hist(d3)
plot(d3)

###########################################################################y ya###############
# para calcular la probabilidad ###############################################
#p(x,a,b)
#dnorm(x.a.b)
dnorm(20,mean = 18,sd =1.5) ############ la probabilidad que una persona tenga 20 años o mas##########################
# en una distribucion normal con media 18 y desviacion 1.5#############


pnorm(20,mean = 18,sd =1.5)########### la probabilidad que una persona tenga menos de 20 años  ###########################

qnorm(0.90,mean = 18,sd =1.5) ###### valor de la variable aleatoria para una distribucion normal estandar #################

d4<-seq(14,22,len=100)   
d4

lines(d4,norm(d4,18,3), type = "1") ############ grafica $$$$$$$$$$$$$$$
hist(d4)
lines(d4, dnorm(d4,18,3), col = "red")



z<-round(dpois(3,2,log = FALSE),2)
p<-round(ppois(33:7,2,log = FALSE),2)

plot(dpois(0:3,2,log = FALSE), type = "h", lwd = 2,
     main = "Función de masa de probabilidad",
     ylab = "P(X = x)", xlab = "Número de eventos")
plot(ppois(0:3,2,log = FALSE), type = "h", lwd = 2,
     main = "Función de masa de probabilidad",
     ylab = "P(X = x)", xlab = "Número de eventos")

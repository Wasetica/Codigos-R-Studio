###############################################
#LA LIBRERIA ggplot, muy ?til para graficos ###
##############################################
poblacion<-c(20,35,42,45,63,18,45,89,52,74)
muertes<-c(2.5,4.2,5.0,5.6,6,1.5,2.6,8,3.9,6)
region<-c("Bogot?","Bogot?","Bogot?","Cali","Cali","Buga","Buga","Chia","Chia","Chia")
model<-lm(poblacion~muertes)
summary(model)
anova(model)
###################################
#Solo funciona con data frame ###
##################################
data<-data.frame(poblacion,muertes,region)
data
##############################
library(dplyr)
library(ggplot2)
###############################################################
# Los graficos se crean por capas la primera solo es el fondo #
##############################################################
ggplot(data = data)
#######################################################
# Un diagrama de dispersi?n lo encadena el s?mbolo + ##
#######################################################
#
ggplot(data = data)+geom_point(aes(x=poblacion,y=muertes),size=3)
###################################################
# Para cambiar el color y tama?o del s?mbolo    ###
###################################################
ggplot(data = data)+geom_point(aes(x=poblacion,y=muertes),color="blue",size=2)
############################################################################
# Otra manera de representar los datos pero difeenciando por colores,     ##
# tambi?n se incluyen los nombres si la variable es categ?rica            ##
############################################################################
############################################################################
# Si se desea colocar solamente el texto asignado a cada uno de los datos###
############################################################################
ggplot(data = data)+geom_point(aes(x=poblacion,y=muertes),color="blue",size=2)+
  geom_text(aes(x=poblacion,y=muertes,label=region), nudge_x = 3)

library(ggthemes)
library(ggrepel)
ggplot(data = data)+geom_point(aes(x=poblacion,y=muertes),color= "blue",size=3)+
   geom_point(aes(x=poblacion,y=muertes,col=region))+
  xlab("Poblaci?n")+ylab("Muertes")+ggtitle("Numero de muertes por ciudad")+
  scale_color_discrete(name = "Regi?n")+
  geom_text(aes(x=poblacion,y=muertes,label=region), nudge_x = 2, nudge_y = .1)+
  geom_abline(intercept=3.103, slope = 9)
















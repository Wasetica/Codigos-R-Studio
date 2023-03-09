###############
#Supuesto de multicolinealidad
#Utilizar la matriz de corelaciones
# Es necesario observar que las correlaciones 
# entre las varibales sean pequeñas, menor r<0.3
Matriz.corr<-cor(factores_afectan_promedio_estudiante[2:6])
Matriz.corr<-round(Matriz.corr,2)
Matriz.corr
library(corrplot)
corr(Matriz.corr)
#No se observa valores de correlación lineal mayores#
# a 0.30 entonces se puede pensar el modelo no tiene problemas de multicolinealidad#
# Cumple con el supuesto de multicolinealidad#
library(perfomanceAnalytics)
Char.Correlation(factores_afectan_promedio_estudiante[2:6],histogram = TRUE,pch=19)
model<-lm(factores_afectan_promedio_estudiante$promedio~factores_afectan_promedio_estudiante$horaest+
            factores_afectan_promedio_estudiante$horsueño+factores_afectan_promedio_estudiante$horastv+factores_afectan_promedio_estudiante$redsocial+
            factores_afectan_promedio_estudiante$novioa)
error<-residuals(model)
estimados<-(fitted(model))
round(error,1)
plot(error)
#supuesto de autocorelación#
# estadistica de Durbin Watson
library(lmtest)
dwtest(model)
#como el pvalor es menor a 0.05(0.005457), entonces
# se rechaza la Hipotesis nula, por tanto el modelo no cumple
#el supuesto de autocorrelación#
#####################################

#Supuesto de Homocedasticidad#
#######################################
# Hipostesis nula del modelo permanece "constante"
#H1: la varianza del modelo no es constante.
##########################################
plot(estimados,error$error)
grupos<-numeric()
grupos[estimados<3.8]<-1
grupos[estimados>3.8 & estimados<4.4]<-2
grupos[estimados>4.4]<-3

bartlett.test(estimados~grupos)
# Como el pvalor es menora 0.05 entonces se rechaza la hipotesis nula
# por tanto no se cumple el supuesto de homocedasticidad
##############################################

#Supuesto de normalidad#
#Ho: normalidad multivariante
#H1: No normalidad multivariante
################################
library(MVN)
mvn(factores_afectan_promedio_estudiante^[1:6])
#solo la variable promedio se comporta de manera normal.
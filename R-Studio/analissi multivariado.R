###########################
# Modelo lineal multivariante
#################################


#Estimacion de los parametros del modelo

model<-lm(factores_afectan_promedio_estudiante$horaest~
            factores_afectan_promedio_estudiante$promedio+
            factores_afectan_promedio_estudiante$horastv+
            factores_afectan_promedio_estudiante$horsueño+
            factores_afectan_promedio_estudiante$redsocial+
            factores_afectan_promedio_estudiante$novioa)


model
round(model,2)
summary(factores_afectan_promedio_estudiante)


estimados<-fitted(model)
round(estimados,2)
plot(estimados)

#Los valores del error del modelo

error<-residuals(model)
rund(error,2)
plot(error)

#Evaluzación del modelo
anova(model)
summary(model)

#Evaluzación de los supuestos del modelo



###########################
# Modelo lineal multiple
#################################
#leer los datos del excel
par(mfrow=c(2,2))
hist(factores_afectan_promedio_estudiante$horaest,
     ylab="Hora de estudio",xlab="Estudiante")
hist(factores_afectan_promedio_estudiante$horsueño,
     ylab="Hora de estudio",xlab="Estudiante")
hist(factores_afectan_promedio_estudiante$horastv,
     ylab="Horaas de televisión",xlab="Estudiante")
hist(factores_afectan_promedio_estudiante$redsocial,
     ylab="Hora de estudio",xlab="Estudiante")
hist(factores_afectan_promedio_estudiante$promedio,
     ylab="promedio",xlab="Estudiante")

par(mfrow=c(1,1))

boxplot(factores_afectan_promedio_estudiante$horsueño
        col="cyan")

plot(factores_afectan_promedio_estudiante$promedio,
     factores_afectan_promedio_estudiante$horaest)

model<-lm(factores_afectan_promedio_estudiante$promedio~factores_afectan_promedio_estudiante+
            factores_afectan_promedio_estudiante$horsueño+
            factores_afectan_promedio_estudiante$horastv+
            factores_afectan_promedio_estudiante$redsocial+
            factores_afectan_promedio_estudiante$novioa)

round(model,2)
summary(model)




plot(factores_afectan_promedio_estudiante$horsueño,
     ylab="Hora de estudio",xlab="Estudiante")
plot(factores_afectan_promedio_estudiante$horaest,
     ylab="Hora de estudio",xlab="Estudiante")
plot(factores_afectan_promedio_estudiante$horastv,
     ylab="Horaas de televisión",xlab="Estudiante")
plot(factores_afectan_promedio_estudiante$redsocial,
     ylab="Hora de estudio",xlab="Estudiante")
plot(factores_afectan_promedio_estudiante$promedio,
     ylab="promedio",xlab="Estudiante")
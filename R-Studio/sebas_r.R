####################################
#Analisis Cluster metodo de Ward#
####################################
library(cluster)
######################################
#eliminar la escala de las variables #
###################################################
# Procedimiento denominado estandarizar los datos #
###################################################
est<-as.data.frame(estudiantes_2)
est
dataestandarizada<-scale(est[1:4], center = T, scale = T)
dataestandarizada
#####################################
# Calculo de la distancia Euclidea  #
#####################################
dist_euclidea<-dist(dataestandarizada, method = "euclidean")

dist_euclidea
round(dist_euclidea,2)
#Metodo de Ward para construir los cluster (conglomerados)#
hc1<-hclust(d=dist_euclidea, method = "ward.D")
#para dibujar el dendograma
plot(hc1, main="Estudiantes", 
     y="distancias Euclideas", xlab = "Metodo de Ward")
rect.hclust(hc1, k=3, border="blue")

###############################
# K means 
#############################
#Necesitan dos paquetes rattle, cluster
# Estandarizacin de los datos # el cielo es azul
data<-scale(estidiantes_1[1:5],center = TRUE,scale = TRUE)
data
data1<-data.frame(data)
set.seed(48)
# Determinar el numero de Cluster
#Necesita la libreria factoextra
library(factoextra)

library(ggplot2)
#El procedimiento del analisis cluster con 3 agrupaciones

kmedias<-kmeans(data,3)
print(kmedias)
attributes(kmedias)
kmedias$centers
#variaci?n totales
kmedias$totss
#variaci?n entre los cluster
kmedias$betweenss
#variaci?n dentro de los cluster
kmedias$withinss
kmedias$cluster
table(kmedias$cluster)
kmedias$size
cl<-cbind(kmedias$cluster)
cl
library(cluster)

clusplot(data,kmedias$cluster,
         main = "cluster",shade=TRUE,labels = 0, lines = 0)
clusplot(data,kmedias$cluster,
         main = "cluster",shade=TRUE,labels = 2, lines = 0)
################################
#Analisis de varianza ANOVA#####
################################

notas<-as.data.frame(Notas_est)
anova3<-aov(notas$`4`~notas$`3`+notas$`6`+notas$`7`+notas$`9`+notas$`11`+notas$`12`+notas$`15`)
summary(anova3)

anova2<-aov(notas$`8`~notas$`10`+notas$`13`+notas$`14`,data = notas)
summary(anova2)

anova1<-aov(notas$`1`~notas$`2`+notas$`5`,data = notas)
summary(anova1)

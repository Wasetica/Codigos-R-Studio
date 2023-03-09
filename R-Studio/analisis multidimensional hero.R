data<-as.data.frame(Superhero)
data
#estandarizan las variables
dataestandarizada<-scale(data[2:7], center = T,scale = T)
dataestandarizada
#calculan las distancias euclideas
distancias<-dist(dataestandarizada,method = "euclidean", diag = T, upper = T)
distancias
#Crea una matriz con las distancias euclideas
matriz<-as.matrix(distancias)
matriz
diseuclidea<-as.dist(matriz)
#algoritmo escalamiento multidimensional
library(smacof)
library(plotrix)
library(e1071)
aem<-mds(delta=diseuclidea, ndim = 2)
plot(aem$conf, pch=1, xlim=range(aem$conf), ylim=c(-1,1))
relatos<-c("Anti-Monitor", "Aquaman","Captain Marvel","Cyclops","Cyborg","Deadpool","Electro",
           "Elektra","	Flash")
text(aem$conf,pos = 1,labels = relatos, col = "blue")
print(aem$stress)

###################################################################################################



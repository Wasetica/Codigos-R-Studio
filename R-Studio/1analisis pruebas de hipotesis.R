#########################################################################
# Escoger tres bebidas diferentes y realizan un an?lisis descriptivo    #
#########################################################################
###################
# Representciones #
##################
data<-as.data.frame(bebidas)

barplot(bebidas$cafe,col=c("blue","magenta","#699CC5"), main = "Valoraciones para Café")


data<-as.data.frame(bebidas)

barplot(bebidas$chocolate,col=c("light blue","green","orange"), main = "Valoraciones para chocolate")


data<-as.data.frame(bebidas)

barplot(bebidas$agua,col=c("light blue","green","orange"), main = "Valoraciones para agua")


data<-as.data.frame(bebidas)
par(mfrow=c(2,2))


barplot(bebidas$cafe,col= c( "Pink","grey","purple"), main = "Valoraciones para Café")
hist(bebidas$cafe)
dotchart(bebidas$cafe)
boxplot(bebidas$cafe)

barplot(bebidas$chocolate,col= c( "Pink","grey","purple"), main = "Valoraciones para Chocolate")
hist(bebidas$chocolate)
dotchart(bebidas$chocolate)
boxplot(bebidas$chocolate)

barplot(bebidas$agua,col= c( "Pink","grey","purple"), main = "Valoraciones para Agua")
hist(bebidas$agua)
dotchart(bebidas$agua)
boxplot(bebidas$agua)

#############################################################################
## CAFÉ################################################
par(mfrow=c(2,2))
barplot(bebidas$cafe,col= c( "blue","magenta","#699CC5"), main = "Valoraciones para café")
hist(bebidas$cafe, col = c("#FF9999","#FF7F50","#333366"), main = "Histograma café")
dotchart(bebidas$cafe, col=c("blue"))
boxplot(bebidas$cafe, col=c("#FF4500"))

#############################################################################
## CHOCOLATE################################################

par(mfrow=c(2,2))
barplot(bebidas$chocolate,col= c( "blue","magenta","#699CC5"), main = "Valoraciones para chocolate")
hist(bebidas$chocolate, col = c("#FF9999","#FF7F50","#333366"), main = "Histograma chocolate")
dotchart(bebidas$chocolate, col=c("blue"))
boxplot(bebidas$chocolate, col=c("#FF4500"))

####################################################
## AGUA#####################################

par(mfrow=c(2,2))
barplot(bebidas$agua,col= c( "blue","magenta","#699CC5"), main = "Valoraciones para agua")
hist(bebidas$agua, col = c("#FF9999","#FF7F50","#333366"), main = "Histograma agua")
dotchart(bebidas$agua, col=c("blue"))
boxplot(bebidas$agua, col=c("#FF4500"))

#Medidas descriptivas café
length(bebidas$cafe)
mediacafe<-round(mean(bebidas$cafe),2)
varianzacafe<-round(var(bebidas$cafe),2)
desvcafe<-round(sd(bebidas$cafe),2)
cvcafe<-round(desvcafe/mediacafe,2)*100
#si cv 0-20 muy poca dispersion
#si cv 20-40 poca dispersion
#si cv 40-60 dispersion moderada
#si cv 60-80 alta dispersion
#si cv 80-100 Muy alta dispersion

#Medidas descriptivas chocolate

length(bebidas$chocolate)
mediachocolate<-round(mean(bebidas$chocolate),2)
varianzachocolate<-round(var(bebidas$chocolate),2)
desvchocolate<-round(sd(bebidas$chocolate),2)
cvchocolate<-round(desvchocolate/mediachocolate,2)*100

#Medidas descriptivas agua
length(bebidas$agua)
mediaagua<-round(mean(bebidas$agua),2)
varianzaagua<-round(var(bebidas$agua),2)
desvagua<-round(sd(bebidas$agua),2)
cvagua<-round(desvagua/mediaagua,2)*100


#boxplot(x~y, data=datos,las=1)

boxplot(bebidas$cafe, col = "6")
boxplot(bebidas$cafe, data=bebidas, las=1)
boxplot(bebidas$agua, data=bebidas, las=1)
boxplot(bebidas$chocolate, data=bebidas, las=1)


stripchart(bebidas, method = "jitter", pch = 19, add = TRUE, col = "blue")

#Prueba para una media poblacional ##########################
#t.test(x, y = NULL,                                        #                                       
#       alternative = c("two.sided", "less", "greater"),    #
#      mu = 0, paired = FALSE, var.equal = FALSE,           #
#      conf.level = 0.95, ...)                              #
#############################################################

#cafe
t.test(bebidas$cafe, alternative = c ("two.sided", "less", "greater"),    
             mu = 0, paired = FALSE, var.equal = FALSE,           
             conf.level = 0.95)
#Chocolate
t.test(bebidas$chocolate, alternative = c ("two.sided", "less", "greater"),    
       mu = 0, paired = FALSE, var.equal = FALSE,           
       conf.level = 0.95)
#Agua
t.test(bebidas$agua, alternative = c ("two.sided", "less", "greater"),    
       mu = 0, paired = FALSE, var.equal = FALSE,           
       conf.level = 0.95)

#Prueba de hipotesis para una proporción ####################
#prop.test(x, n, p = NULL,                                  #
#alternative = c("two.sided", "less", "greater"),           #
#conf.level = 0.95, correct = TRUE)                         #
#############################################################

table(bebidas$cafe)
prop_1<-(29/length(bebidas$cafe))

table(bebidas$chocolate)
prop_2<-(31/length(bebidas$chocolate))

table(bebidas$agua)
prop_3<-(16/length(bebidas$agua))

# Muestras pequeñas #########################################
#binom.test(x, n, p = 0.5,                                  #
#alternative = c("two.sided", "less", "greater"),           #
#conf.level = 0.95)                                         #
#############################################################
table(bebidas$cafe)
binom.test(26, 120, p = 0.3,                                  
           alternative = c("less"),           
           conf.level = 0.95)

table(bebidas$chocolate)
binom.test(31, 120, p = 0.5,                                  
           alternative = c("less"),           
           conf.level = 0.95)


table(bebidas$agua)
binom.test(16, 120, p = 0.4,                                  
           alternative = c("less"),           
           conf.level = 0.95)

#Prueba para dos medias poblacionales #######################
#t.test(x=T1, y=T2, alternative="two.sided", mu=0,          #
#paired=FALSE, var.equal=TRUE, conf.level=0.97)             #
#############################################################

t.test(bebidas$cafe, alternative="two.sided", mu=0,          
       paired=FALSE, var.equal=TRUE, conf.level=0.97)

t.test(bebidas$chocolate, alternative="two.sided", mu=0,          
       paired=FALSE, var.equal=TRUE, conf.level=0.97)

t.test(bebidas$agua, alternative="two.sided", mu=0,          
       paired=FALSE, var.equal=TRUE, conf.level=0.97)

#Prueba de hipotesis para dos proporciones ##################
#prop.test(x=c(x1, x2), n=c(n1, n2),                        #
#alternative='greater', conf.level=0.95)                    #
#############################################################

prop.test(x=c(16, 31), n=c(120, 120),                        #
          alternative='greater', conf.level=0.95)

######Lo tiene que hacer con todas los resultados de la pruebas...

#cuartiles no es necesario

library(psych)
skew(bebidas$cafe)
skew(bebidas$chocolate)
skew(bebidas$agua)


kurtosi(bebidas$cafe)
kurtosi(bebidas$chocolate)
kurtosi(bebidas$agua)

library(moments)
#Percentiles##
quantile(bebidas$cafe, probs = c(0.25, 0.50, 0.75))
quantile(bebidas$chocolate, probs = c(0.25, 0.50, 0.75))
quantile(bebidas$agua, probs = c(0.25, 0.50, 0.75))


skewness(bebidas$cafe)
skewness(bebidas$chocolate)
skewness(bebidas$agua)

kurtosis(bebidas$cafe)
kurtosis(bebidas$chocolate)
kurtosis(bebidas$agua)


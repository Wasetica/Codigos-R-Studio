######################################
######Muestreo Aleatoria estratificado####
###########################################
##data : Kilometraje por auto  #########
################

summary(na.omit(kilometraje_por_tipo_automovil$automoviles))
dsest1<-round(sd(na.omit(kilometraje_por_tipo_automovil$automoviles)),2)
dsest2<-round(sd(na.omit(kilometraje_por_tipo_automovil$suv)),2)
dsest3<-round(sd(na.omit(kilometraje_por_tipo_automovil$camperos)),2)
dsest4<-round(sd(na.omit(kilometraje_por_tipo_automovil$camionetas)),2)
N1=length(na.omit(kilometraje_por_tipo_automovil$automoviles))
N2=length(na.omit(kilometraje_por_tipo_automovil$suv))
N3=length(na.omit(kilometraje_por_tipo_automovil$camperos))
N4=length(na.omit(kilometraje_por_tipo_automovil$camionetas))
N=N1+N2+N3+N4

c1=2000
c2=2500
c3=3000
c4=2800
E=5

num<-((N1*dsest1/sqrt(c1))+(N2*dsest2/sqrt(c2))+(N3*dsest3/sqrt(c3))+(N4*dsest4/sqrt(c4)))*
  ((N1*dsest1*sqrt(c1))+(N2*dsest2*sqrt(c2))+(N3*dsest3*sqrt(c3))+(N4*dsest4*sqrt(c4)))
den<-(N^2*E^2/4)+((N1*dsest1^2)+(N2*dsest2^2)+(N3*dsest3^2)+(N4*dsest4^2))

n=round((num/den),0)

n1=round((N1*dsest1/sqrt(c1))/((N1*dsest1/sqrt(c1))+(N2*dsest2/sqrt(c2))+(N3*dsest3/sqrt(c3))+(N4*dsest4/sqrt(c4)))*n,0)
n2=round((N2*dsest2/sqrt(c2))/((N1*dsest1/sqrt(c1))+(N2*dsest2/sqrt(c2))+(N3*dsest3/sqrt(c3))+(N4*dsest4/sqrt(c4)))*n,0)
n3=round((N3*dsest3/sqrt(c3))/((N1*dsest1/sqrt(c1))+(N2*dsest2/sqrt(c2))+(N3*dsest3/sqrt(c3))+(N4*dsest4/sqrt(c4)))*n,0)
n4=round((N4*dsest4/sqrt(c4))/((N1*dsest1/sqrt(c1))+(N2*dsest2/sqrt(c2))+(N3*dsest3/sqrt(c3))+(N4*dsest4/sqrt(c4)))*n,0)

n1+n2+n3+n4

####################Muestra estratos.#############

MAE1=sample(na.omit(kilometraje_por_tipo_automovil$automoviles),n1,replace = TRUE)
MAE2=sample(na.omit(kilometraje_por_tipo_automovil$suv),n2,replace = TRUE)
MAE3=sample(na.omit(kilometraje_por_tipo_automovil$camperos),n3,replace = TRUE)
MAE4=sample(na.omit(kilometraje_por_tipo_automovil$camionetas),n4,replace = TRUE)



###################################Estimaci?n de la media en el muestreo aleatorio estratificado#
YmediaEST=round((1/N*((N1*mean(MAE1))+(N2*mean(MAE2))+(N3*mean(MAE3))+(N4*mean(MAE4)))),2)

################################Estimacion de la varianza####################

varMAE<-(1/N^2)*(var(MAE1)/n1)*((N1-n1)/N1)+(var(MAE2)/n2)*((N2-n2)/N2)+(var(MAE3)/n3)*((N3-n3)/N3)+(var(MAE4)/n4)*((N4-n4)/N4)

#####################################Error de estimacion#######################
errormediaMAE<-round(2*sqrt(varMAE),2)

########################Limite superior de confianza##########################
LSC<-round(YmediaEST+errormediaMAE,2) 
########################Limite inferior de confianza##########################
LIC<-round(YmediaEST-errormediaMAE,2) 

###########Total Poblacional######
total<-N*YmediaEST

########### Varianza total######
vartotal<-N*var

#########error de estimaci?n total#####
errormastotal<-round(2*sqrt(vartotal),2)

#################Intervalo de confiaza para el total############
LSCTOTAL<-round(total+2*sqrt(var),2)
LICTOTAL<-round(total-2*sqrt(var),2)


###########################estimacion de la proporcion ########################
###############################################################################

#########PROPORCION AUTOMOVILES QUE RECORREN MAS DE 80 KILOMETROS############## 

table(MA1)
P1=52/62
table(MA2)
P2=23/55
table(MA3)
P3=23/36
table(MA4)
P4=34/82

p=(1/N)*((N1*P1)+(N2*P2)+(N3*P3)+(N4*P4))

######### VARIANZA DE LA PROPORCION##########

PVAR=(1/N^2)*((N1^2)*(P1*(1-P1)/(n1-1))*((N1-n1)/N1)+(N2^2)*(P2*(1-P2)/(n2-1))*(N2-n2/N2)+(N3^2)*(P3*(1-P3)/(n3-1))*(N3-n3/N3)+(N4^2)*(P4*(1-P4)/(n4-1))*(N4-n4/N4))
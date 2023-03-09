summary(Horas_que_los_ninos_practican_Video_juegos$Bogotá)
sdbogota<-na.omit(sd(Horas_que_los_ninos_practican_Video_juegos$Bogotá,4))  

summary(Horas_que_los_ninos_practican_Video_juegos$Bucaramanga)
sdbucaramanga<-na.omit(sd(Horas_que_los_ninos_practican_Video_juegos$Bucaramanga,4))

summary(Horas_que_los_ninos_practican_Video_juegos$Cartagena)
sdcartagena<-na.omit(sd(Horas_que_los_ninos_practican_Video_juegos$Cartagena,4))

summary(Horas_que_los_ninos_practican_Video_juegos$Pasto)
sdpasto<-na.omit(sd(Horas_que_los_ninos_practican_Video_juegos$Pasto,4))

sum(table(Horas_que_los_ninos_practican_Video_juegos$Bogotá))
sum(table(Horas_que_los_ninos_practican_Video_juegos$Cartagena))
sum(table(Horas_que_los_ninos_practican_Video_juegos$Bucaramanga))
sum(table(Horas_que_los_ninos_practican_Video_juegos$Pasto))

N1=length(na.omit(Horas_que_los_ninos_practican_Video_juegos$Bogotá))
N2=length(na.omit(Horas_que_los_ninos_practican_Video_juegos$Bucaramanga))
N3=length(na.omit(Horas_que_los_ninos_practican_Video_juegos$Cartagena))
N4=length(na.omit(Horas_que_los_ninos_practican_Video_juegos$Pasto))
N<-6640

c1<-122
c2<-2350
c3<-145
c4<-555
E = 0.05

num<-((N1*sdbogota/sqrt(c1))+(N2*sdbucaramanga/sqrt(c2))+(N3*sdcartagena/sqrt(c3))+(N4*sdpasto/sqrt(c4)))*
  ((N1*sdbogota*sqrt(c1))+(N2*sdbucaramanga*sqrt(c2))+(N3*sdcartagena*sqrt(c3))+(N4*sdpasto*sqrt(c4)))

den<-(N^2*E^2/4)+((N1*sdbogota^2)+(N2*sdbucaramanga^2)+(N3*sdcartagena^2)+(N4*sdpasto^2))

n=round((num/den),0)



n1=round((N1*sdbogota/sqrt(c1))/((N1*sdbogota/sqrt(c1))+(N2*sdbucaramanga/sqrt(c2))+(N3*sdcartagena/sqrt(c3))+(N4*sdpasto/sqrt(c4)))*n,0)
n2=round((N2*sdbucaramanga/sqrt(c2))/((N1*sdbogota/sqrt(c1))+(N2*sdbucaramanga/sqrt(c2))+(N3*sdcartagena/sqrt(c3))+(N4*sdpasto/sqrt(c4)))*n,0)
n3=round((N3*sdcartagena/sqrt(c3))/((N1*sdbogota/sqrt(c1))+(N2*sdbucaramanga/sqrt(c2))+(N3*sdcartagena/sqrt(c3))+(N4*sdpasto/sqrt(c4)))*n,0)
n4=round((N4*sdpasto/sqrt(c4))/((N1*sdbogota/sqrt(c1))+(N2*sdbucaramanga/sqrt(c2))+(N3*sdcartagena/sqrt(c3))+(N4*sdpasto/sqrt(c4)))*n,0)
Nn<-n1+n2+n3+n4


MAE1=sample(na.omit(Horas_que_los_ninos_practican_Video_juegos$Bogotá),n1,replace = TRUE)
MAE2=sample(na.omit(Horas_que_los_ninos_practican_Video_juegos$Bucaramanga),n2,replace = TRUE)
MAE3=sample(na.omit(Horas_que_los_ninos_practican_Video_juegos$Cartagena),n3,replace = TRUE)
MAE4=sample(na.omit(Horas_que_los_ninos_practican_Video_juegos$Pasto),n4,replace = TRUE)


YmediaEST=round((1/N*((N1*mean(MAE1))+(N2*mean(MAE2))+(N3*mean(MAE3))+(N4*mean(MAE4)))),2)
varMAE<-(1/N^2)*(var(MAE1)/n1)*((N1-n1)/N1)+(var(MAE2)/n2)*((N2-n2)/N2)+(var(MAE3)/n3)*((N3-n3)/N3)+(var(MAE4)/n4)*((N4-n4)/N4)

errormediaMAE<-round(2*sqrt(varMAE),2)

LSC<-round(YmediaEST+errormediaMAE,2) 
LIC<-round(YmediaEST-errormediaMAE,2) 


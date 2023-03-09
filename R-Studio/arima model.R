install.packages("tseries")
install.packages("astsa")
install.packages("forecast")
install.packages("foreign")
install.packages("quantmod")
install.packages("lubridate")
install.packages("tidyverse")


library(tseries)
library(astsa)
library(forecast)
library(tidyverse)
library(lubridate)
library(foreign)
library(quantmod)
Datos<-data
Datos;
head(Datos)

d.ts <- ts(Datos)
class(d.ts)
head(d.ts)
plot(d.ts, ylab="PIB", main="datos PIB Colombia")

d.ts.i <- ts(Datos, start=c(2005,2), frequency=4)
d.ts.i;
plot(d.ts.i,ylab="PIB", main="datos PIB Colombia")
end(d.ts.i)


ndiffs(d.ts.i)

acf(d.ts.i,ylab="Autocorrelaci�n",main="Correlaci�n")
pacf(d.ts.i)
adf.test(d.ts.i,alternative="stationary")

seasonplot(d.ts.i, col=rainbow(12),year.labels=TRUE)


seriedif=diff(d.ts.i)
seriedif
plot(seriedif)
adf.test(seriedif,alternative="stationary")

serielog=log(d.ts.i)
plot(serielog)
adf.test(serielog,alternative="stationary")

acf(seriedif)
pacf(seriedif)

acf(seriedif,frequency=1)
pacf(seriedif,frequency=1)

acf(serielog)
pacf(serielog)

par(mfrow=c(2,2),mar=c(4,4,4,1)+.1)

seriedif2=diff(d.ts.i,differences=2)
acf(seriedif2)

par(mfrow=c(1,1),mar=c(4,4,4,1)+.1)



modelo1=arima(d.ts.i,order=c(4,1,5))
tsdiag(modelo1)
Box.test(residuals(modelo1),type="Ljung-Box")
error=residuals(modelo1)
plot(error)

pronostico <- forecast::forecast(modelo1,h=4)
pronostico
plot(pronostico)


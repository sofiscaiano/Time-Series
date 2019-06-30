library(urca)
library(moments)
library(forecast)
library(tseries)

serie <- read.csv("serie20.csv", header=T)
ts(serie)
x<-serie$x
st <- ts(x)
par(mfrow=c(1,1))
plot(st,main=("Serie de tiempo -ST*-"))

#Analisis Descriptivo
Quantiles <- quantile(st,na.rm=T)
Promedio <- sapply(st,mean,na.rm=T)
Momentos <- all.moments(st)
Kurtosis <- kurtosis(st)
Asimetria <- skewness(st)
summary(st)
boxplot(st,main="BoxPlot ST*")

#Grafico FAS FAC FACP
par(mfrow=c(1,3))
layout(matrix(1:4,2,2))
acf(st,lag.max=10,type="covariance",plot=T,main="FAS")
acf(st,main="FAC")
pacf(st,main="FACP")

#Comparacion de modelos

none.df<-ur.df(x,type="none",lags=5,selectlags=c("AIC"))
drift.df<-ur.df(x,type="drift",lags=5,selectlags=c("AIC"))
trend.df<-ur.df(x,type="trend",lags=5,selectlags=c("AIC"))

summary(none.df) #Rechazo Ho
summary(drift.df) #Rechazo Ho
summary(trend.df)  #Rechazo Ho 

ar2 <- arima(x,order=c(2,0,0),include.mean=FALSE)
ar1 <- arima(x,order=c(1,0,0),include.mean=FALSE)
arma11 <- arima(x,order=c(1,0,1),include.mean=FALSE)

ar2
ar1
arma11

AICar2 <- AIC(ar2)
AICar1 <- AIC(ar1)
AICarma11 <-AIC (arma11)
BICar2 <-BIC(ar2)
BICar1 <- BIC(ar1)
BICarma11<- BIC(arma11)

CI <- data.frame(AICar2,AICar1,AICarma11,BICar2,BICar1,BICarma11)
CIm <- matrix(CI,nrow=2,ncol=3,byrow=T)
m <- c("AR(2)","AR(1)","ARMA(1,1)")
ci <- c("AIC","BIC")
rownames(CIm) <- ci
colnames(CIm) <- m
CIm #Elijo menor AIC y menor BIC -> AR(2)

#Prediccion
Prediccion.1 <- predict(ar2,n.ahead=1)$pred
Error.1 <- predict(ar2,n.ahead=1)$se

Prediccion.2 <- predict(ar2,n.ahead=2)$pred
Error.2 <- predict(ar2,n.ahead=2)$se

Prediccion.3 <- predict(ar2,n.ahead=3)$pred
Error.3 <- predict(ar2,n.ahead=3)$se

Prediccion.20 <- predict(ar2,n.ahead=20)$pred
Error.20 <- predict(ar2,n.ahead=20)$se

par(mfrow=c(2,2))
layout(matrix(1:2,1,2))
plot(Prediccion.20)
plot(Error.20)


#Intervalos de Confianza

IC.P1 <- forecast(ar2,h=1,level= c(0.94,0.95,0.99))
IC.P2 <- forecast(ar2,h=2,level= c(0.94,0.95,0.99))
IC.P3 <- forecast(ar2,h=3,level= c(0.94,0.95,0.99))
IC.P20 <- forecast(ar2,h=20,level= c(0.94,0.95,0.99))

#DataFrame

IC.DF <- data.frame(IC.P20)
IC.DF <- as.data.frame (IC.DF[,c(2,4,6,1,3,5,7)])
View(IC.DF)

#Analisis de la serie

Box.test(residuals(ar2)) #No Rechazo Ho -> Incorrelacion/Independencia
jarque.bera.test(residuals(ar2)) #No Rechazo Ho -> Residuos normales


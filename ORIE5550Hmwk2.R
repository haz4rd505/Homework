#Homework 2 ORIE 5550

#install.packages("forecast")
library(forecast)
library(CADFtest)
data("AirPassengers")

air.ts<-ts(log(AirPassengers),frequency=12,start=c(1949,1))

q <- sqrt(length(air.ts))
CADFtest(air.ts, type= "trend", criterion= "BIC", max.lag.y=q) 
#Does not reject, therefore it has a stochastic trend.

difflogair.ts<-ts(diff(log(AirPassengers)),frequency=12,start=c(1949,1))
monthplot(difflogair.ts)
#Yes there is a strong seasonal affect

dslogy <-  diff(diff(air.ts,lag=12))
CADFtest(dslogy, type= "drift", criterion= "BIC", max.lag.y=q)
acf(dslogy,lag.max=50)
pacf(dslogy,lag.max=50)


ma_model<-Arima(air.ts,order=c(0,1,1),seasonal=c(0,1,1))


ar_model<-Arima(air.ts,order=c(1,1,0),seasonal=c(1,1,0))
acf(ar_model$residuals,main="Residuals of AR/SAR Model")
Box.test(ar_model$residuals,lag = q, type=c("Ljung-Box"))

ma_pred <- predict(ma_model,n.ahead=12)
lower <- ma_pred$pred-qnorm(0.975)*ma_pred$se
upper <- ma_pred$pred+qnorm(0.975)*ma_pred$se
plot.ts(air.ts,xlim=c(1960,1962),ylim=c(5.5,6.8),main="Forecast MA/SMA",xlab="Year")
lines(ma_pred$pred,col="red")
lines(lower,col="blue")
lines(upper,col="blue")


y <- air.ts
S <- round(0.75*length(y)); h=1; 

#Sampling MA/SMA Model
e.ma<-c()
for (i in S:(length(y)-h)) # Expanding Window Forecast (SAR)
{
  ma.mod<-Arima(y[1:i], order = c(0,1,1),seasonal=c(0,1,1))
  predict.h<-predict(ma.mod, n.ahead = h)$pred[h]
  e.ma<-c(e.ma, y[i+h] - predict.h)
}

#Sampling AR/SAR Model
e.ar<-c()
for (i in S:(length(y)-h)) # Expanding Window Forecast (SAR)
{
  ar.mod<-Arima(y[1:i], order = c(1,1,0),seasonal=c(1,1,0))
  predict.h<-predict(ar.mod, n.ahead = h)$pred[h]
  e.ar<-c(e.ar, y[i+h] - predict.h)
}

mae.ma<-mean(abs(e.ma))
mae.ar<-mean(abs(e.ar))
dm.test(e.ma,e.ar,h=h,power=1)

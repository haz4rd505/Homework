
flow <- readRDS("C:/Users/kmd266/Documents/R/Hmwk/qflow_parsed.RDS")
prcp <- readRDS("C:/Users/kmd266/Documents/R/Hmwk/prcp_parsed.RDS")

p.ts<-ts(prcp$prcp,start=c(1986,1),frequency =12)

#Precipitation data is often log-normally distributed
plog<-log(p.ts)
CADFtest(plog,criterion="BIC",max.lag.y=round(sqrt(length(plog))),type='drift')
#From the ADF test we can see that our data is stationary

#However if we look at the monthplot we can see clear seasonality, therefore we use the seasonal difference operator
#Taking the Correlogram we see that We have significants lags at 1 and near the order of seasonality
dplog <- diff(plog,lag=12)
acf(dplog)
pacf(dplog)
#From our ACF we decide to use a SARIMA(0,0)(1,0)

library(forecast)
fit_sar1 <- Arima(dplog[1:280],order=c(0,0,0),seasonal=c(1,0,0)) #BIC626.85
summary(fit_sar1)
fit_sma1 <- Arima(dplog,order=c(0,0,0),seasonal=c(0,0,1)) #BIC535.14
summary(fit_sma1)
fit_sma2 <- Arima(dplog,order=c(0,0,0),seasonal=c(0,0,2)) #BIC535.74
summary(fit_sma2)
#Although SMA1 and SMA2 models are similar SMA1 is better with respect to the BIC.

fit_sma1$coef/(sqrt(diag(fit_sma1$var.coef)))
#Both our coefficients are significant

Box.test(fit_sma1$residuals,lag = round(sqrt(length(fit_sma1$residuals))),type="Ljung-Box")
#We prove that our residuals are white noise as the null is not rejected.



# Out-of-sample Criteria --------------------------------------------------

#TESTING BETWEEN SMA1 and SMA2
y <- dplog
S <- round(0.75*length(y)); h=1; # using 75% of data as training set, forecast window of 1
errorSAR.h<-c()
for (i in S:(length(y)-h)) # Expanding Window Forecast (SAR)
{
  dat<-subset(y,start=1,end=i)
  mymodel.sub<-Arima(dat, order = c(0,0,0),seasonal=c(0,0,2))
  predict.h<-predict(mymodel.sub, n.ahead = h)$pred[h]
  errorSAR.h<-c(errorSAR.h, y[i+h] - predict.h)
}

errorSMA.h<-c()
for (i in S:(length(y)-h)) # Expanding Window Forecast (SMA)
{
  dat <- subset(y,start=1,end=i)  
  mymodel.sub<-Arima(dat, order = c(0,0,0), seasonal=c(0,0,1))
  predict.h<-predict(mymodel.sub, n.ahead = h)$pred[h]
  errorSMA.h<-c(errorSMA.h,y[i+h] - predict.h)
}

mean(abs(errorSAR.h));mean(abs(errorSMA.h)) # Mean Absolute Forecast Error
dm.test(errorSAR.h, errorSMA.h, h = h, power = 1)

mean(errorSAR.h^2);mean(errorSMA.h^2) # Mean Squared Forecast Error
dm.test(errorSAR.h, errorSMA.h, h=h, power=2)
#We see no significant difference on both metrics


#TESTING BETWEEN SAR1 and SMA1
y <- dplog
S <- round(0.75*length(y)); h=1; # using 75% of data as training set, forecast window of 1
errorSAR.h<-c()
for (i in S:(length(y)-h)) # Expanding Window Forecast (SAR)
{
  dat<-subset(y,start=1,end=i)
  mymodel.sub<-Arima(dat, order = c(0,0,0),seasonal=c(1,0,0))
  predict.h<-predict(mymodel.sub, n.ahead = h)$pred[h]
  errorSAR.h<-c(errorSAR.h, y[i+h] - predict.h)
}

errorSMA.h<-c()
for (i in S:(length(y)-h)) # Expanding Window Forecast (SMA)
{
  dat <- subset(y,start=1,end=i)  
  mymodel.sub<-Arima(dat, order = c(0,0,0), seasonal=c(0,0,1))
  predict.h<-predict(mymodel.sub, n.ahead = h)$pred[h]
  errorSMA.h<-c(errorSMA.h,y[i+h] - predict.h)
}

mean(abs(errorSAR.h));mean(abs(errorSMA.h)) # Mean Absolute Forecast Error
dm.test(errorSAR.h, errorSMA.h, h = h, power = 1)

mean(errorSAR.h^2);mean(errorSMA.h^2) # Mean Squared Forecast Error
dm.test(errorSAR.h, errorSMA.h, h=h, power=2)

#SMA1 model is significant better based on Diabold Mariano test






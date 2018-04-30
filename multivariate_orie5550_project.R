
# Precip Management -------------------------------------------------------


prcp <- readRDS("C:/Users/kmd266/Documents/R/Hmwk/prcp_parsed.RDS")

p.ts<-ts(prcp$prcp,start=c(1986,1),frequency =12)

#Precipitation data is often log-normally distributed
plog<-log(p.ts)
CADFtest(plog,criterion="BIC",max.lag.y=round(sqrt(length(plog))),type='drift')
#From the ADF test we can see that our data is stationary

#However if we look at the monthplot we can see clear seasonality, therefore we use the seasonal difference operator
#Taking the Correlogram we see that We have significants lags at 1 and near the order of seasonality
dplog <- diff(plog,lag=12)


# Streamflow Management ---------------------------------------------------


flow <- readRDS("C:/Users/kmd266/Documents/R/Hmwk/qflow_parsed.RDS")

q.ts<-ts(flow$qflow,start=c(1986,1),frequency =12)
qlog<-log(q.ts)
CADFtest(plog,criterion="BIC",max.lag.y=round(sqrt(length(plog))),type='drift')
#similarly stationary,but given monthplot we see seasonality
monthplot(qlog)

dqlog <- diff(qlog,lag=12)
CADFtest(dqlog,criterion="BIC",max.lag.y=round(sqrt(length(dqlog))),type='drift')
#still stationary

# Multivariate Analysis ---------------------------------------------------


#Multivariate Analysis Between P and Q
ccf(dqlog,dplog)
#Based on the CCF I would specify a lag 2 DL

#Specifying distributed lag models
lag = 12
qlogdata <- embed(dqlog,dimension = lag+1)
plogdata <- embed(dplog,dimension = lag+1)


#Fitting Distributed Lag Model
fit_dl <- lm(qlogdata[,1] ~ qlogdata[,c(2,3,13)])
acf(fit_dl$residuals)
Box.test(fit_dl$residuals,type = "Ljung-Box",lag = round(sqrt(length(fit_dl$residuals))))

#Fitting Autoregress Distributed Lag Model
fit_adl <- lm(qlogdata[,1] ~qlogdata[,c(2,3,13)] + plogdata[,c(2)])
acf(fit_adl$residuals)
Box.test(fit_adl$residuals,type = "Ljung-Box",lag = round(sqrt(length(fit_adl$residuals))))
#Box test shows that we have a white noise process for the residuals and thus the model is validated

anova(fit_dl,fit_adl)

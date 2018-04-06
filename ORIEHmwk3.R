#ORIE Homework 3

# USING variables SLS and PRICE

#1 Loading RData
setwd("C:/Users/Kenji/Documents/ORIE5550/")
load("SLS.RData")
load("PRICE.RData")

library(CADFtest)

#2 Preliminary Results
price.ts <- ts(PRICE, start = c(1990, 1), frequency = 12)
sls.ts <- ts(SLS, start =c(1990,1),frequency = 12)

#Checking for Stationarity
q1 = sqrt(length(price.ts))
CADFtest(price.ts,type = 'drift', criterion = "BIC",max.lag.y = q1)
acf(price.ts)
q2 <- sqrt(length(sls.ts))
CADFtest(sls.ts,type = 'drift',criterion = 'BIC',max.lag.y = q2) 
acf(sls.ts)
#Both have no unit roots and are stationary, autocorrelation shows significance at lag 1 for both.

ccf(sls.ts,price.ts)
#In the CCF Price predicts the SLS up to lag 2. Significant instantaneous correlation?

#3. Granger Causality
#a 2 lags
lag <- 2 # based on cross-correlogram
price_l2 <- embed(price.ts, dimension = lag + 1)
sls_l2 <- embed(sls.ts, dimension = lag + 1)

adl2_sls<-lm(sls_l2[,3]~sls_l2[,1:2]+price_l2[,1:2])
#VALIDATING
acf(adl2_sls$residuals)
#c 3 lags
lag <- 3 # based on cross-correlogram
price_l3 <- embed(price.ts, dimension = lag + 1)
sls_l3 <- embed(sls.ts, dimension = lag + 1)

adl3_sls<-lm(sls_l3[,4]~sls_l3[,1:3]+price_l3[,1:3])

#VALIDATING
acf(adl3_sls$residuals)

#Testing for Granger Causality
fitsmall2 <- lm(sls_l2[,3]~sls_l2[,1:2])
fitsmall3 <- lm(sls_l3[,4]~sls_l3[,1:3])
anova(adl2_sls,fitsmall2)
anova(adl3_sls,fitsmall3)
#In both cases it doesn't appear that Price Granger causes SLS

#4. VAR models

library(vars)
hmwkdata <- cbind(sls.ts, price.ts)
VARselect(hmwkdata)
#AIC selects lag order 4; BIC selects lag order 1

varfit <- vars:::VAR(hmwkdata, p = 1)

fit.resid<-resid(varfit)

par(mfrow=c(2,2))
acf(fit.resid[,1])#First set of residuals
acf(fit.resid[,2])#Second set of residuals
ccf(fit.resid[,1],fit.resid[,2])
dev.off()

#Testing for multivariate noise
library(MTS)
mq(fit.resid, lag = floor(sqrt(length(sls.ts))))
plot(irf(varfit,ortho=FALSE,boot=TRUE)) #These plots don't really make sense to me.

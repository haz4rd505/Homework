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


#4. VAR models
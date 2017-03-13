##BEE 6940 Hmwk 3
library(xlsx)
OTprcp <- read.xlsx('C:/Users/kmd266/Documents/BEE6940/Ontario.prcp.xlsx',sheetName = 'Ontario.prcp.txt')
#OTprcp <- read.xlsx('c:/Users/Kenji/Documents/R/Homework/Ontario.prcp.xlsx',sheetName= 'Ontario.prcp.txt')
##STANDARDIZE THE DATA
gage1<-scale(as.numeric(as.character(OTprcp$Gage1)),center=TRUE,scale=TRUE)
gage2<-scale(as.numeric(as.character(OTprcp$Gage2)),center=TRUE,scale=TRUE)
gage3<-scale(as.numeric(as.character(OTprcp$Gage3)),center=TRUE,scale=TRUE)
gage4<-scale(as.numeric(as.character(OTprcp$Gage4)),center=TRUE,scale=TRUE)


# Plotting Hist and QQplot ------------------------------------------------



#PLOT HISTOGRAM of data for all four gauges
par(mfrow=c(2,2))
hist(gage1,xlim = c(-3,3),ylim = c(0,15))
hist(gage2,xlim = c(-3,3),ylim = c(0,15))
hist(gage3,xlim = c(-3,3),ylim = c(0,15))
hist(gage4,xlim = c(-3,3),ylim = c(0,15))

qqnorm(gage1,main='Gage1 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(gage1)

qqnorm(gage2,main='Gage2 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(gage2)

qqnorm(gage3,main='Gage3 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(gage3)

qqnorm(gage4,main='Gage4 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(gage4)
#PLOT HISTOGRAM AND Q-Q plot for log-transformed data.
ln_gage1 <-scale(log(as.numeric(as.character(OTprcp$Gage1))),center=TRUE,scale=TRUE)
ln_gage2 <-scale(log(as.numeric(as.character(OTprcp$Gage2))),center=TRUE,scale=TRUE)
ln_gage3 <-scale(log(as.numeric(as.character(OTprcp$Gage3))),center=TRUE,scale=TRUE)
ln_gage4 <-scale(log(as.numeric(as.character(OTprcp$Gage4))),center=TRUE,scale=TRUE)

windows()
par(mfrow=c(2,2))
hist(ln_gage1,xlim = c(-3,3),ylim = c(0,15))
hist(ln_gage2,xlim = c(-3,3),ylim = c(0,15))
hist(ln_gage3,xlim = c(-3,3),ylim = c(0,15))
hist(ln_gage4,xlim = c(-3,3),ylim = c(0,15))

qqnorm(ln_gage1,main='Log Gage1 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(ln_gage1)

qqnorm(ln_gage2,main='Log Gage2 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(ln_gage2)

qqnorm(ln_gage3,main='Log Gage3 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(ln_gage3)

qqnorm(ln_gage4,main='Log Gage4 QQplot',xlim = c(-2.5,2.5),ylim=c(-2.5,2.5))
qqline(ln_gage4)


# Shapiro-Wilktest --------------------------------------------------------

shapiro.test(gage1)
shapiro.test(gage2)
shapiro.test(gage3)
shapiro.test(gage4)

shapiro.test(ln_gage1)
shapiro.test(ln_gage2)
shapiro.test(ln_gage3)
shapiro.test(ln_gage4)




 

# Calculating the Covariance of the log-transformed data ------------------
loggages <- cbind(ln_gage1,ln_gage2,ln_gage3,ln_gage4)
S <- cov(loggages,use='pairwise.complete')
invS <- solve(S)

D.sq<- c()
for (i in 1:31) {
  
  D.sq[i] <- t(loggages[i,])%*%invS%*%loggages[i,]
}


rchisq(n=31,4)

qqplot(D.sq,rchisq(31,4), main = "Q-Q plot for D^2")
qqline(D.sq,distribution=function(p) qchisq(p,df=4))


ks.test(D.sq,"pchisq",df=4)



# Gap Filling missing data ------------------------------------------------

g1 <- as.numeric(as.character(OTprcp$Gage1))
g2 <- as.numeric(as.character(OTprcp$Gage2))
g3 <- as.numeric(as.character(OTprcp$Gage3))
g4 <- as.numeric(as.character(OTprcp$Gage4))

datmat <- cbind(g1,g2,g3,g4)
pick <- 4
mu1 <- mean(datmat[,pick],na.rm = T)
dat2<- datmat[,-pick]
datmeans <- colMeans(dat2,na.rm=T)
dat<-cbind(datmat[,pick],dat2)
covy <- cov(dat,use='pairwise.complete')
munew <- mu1 + covy[2:4,1]%*%solve(covy[2:4,2:4])%*%(dat[9,2:4]-datmeans)

match(NA,g4)

gap_filler(x,cov,)











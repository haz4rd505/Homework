##BEE 6940 Hmwk 3
library(xlsx)
OTprcp <- read.xlsx('C:/Users/kmd266/Documents/BEE6940/Ontario.prcp.xlsx',sheetName = 'Ontario.prcp.txt')

##STANDARDIZE THE DATA
gage1<-scale(as.numeric(as.character(OTprcp$Gage1)),center=TRUE,scale=TRUE)
gage2<-scale(as.numeric(as.character(OTprcp$Gage2)),center=TRUE,scale=TRUE)
gage3<-scale(as.numeric(as.character(OTprcp$Gage3)),center=TRUE,scale=TRUE)
gage4<-scale(as.numeric(as.character(OTprcp$Gage4)),center=TRUE,scale=TRUE)



#PLOT HISTOGRAM of data for all four gauges
par(mfrow=c(2,2))
hist(gage1)
hist(gage2)
hist(gage3)
hist(gage4)

qqnorm(gage1,main='Gage1 QQplot')
qqline(gage1)

qqnorm(gage2,main='Gage1 QQplot')
qqline(gage2)

qqnorm(gage3,main='Gage1 QQplot')
qqline(gage3)

qqnorm(gage4,main='Gage1 QQplot')
qqline(gage4)
#PLOT HISTOGRAM AND Q-Q plot for log-transformed data.



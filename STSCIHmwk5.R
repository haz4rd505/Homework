#HMWK 5 script for STSCI

cps=read.csv("C:/Users/kmd266/Downloads/cps92_08.csv",header=TRUE)

cps1992ahe <- cps$ahe[which(cps$year==1992)]
cps1992bach
cps2008ahe <- cps$ahe[which(cps$year==2008)]
cps2008bach<- cps$ahe[which(cps$bachelor==1 & cps$year==2008)]
cps2008nobach<-cps$ahe[which(cps$bachelor!=1 & cps$year==2008)]
mean92<-mean(cps1992ahe)
Z92 <-1.94
mean08<-mean(cps2008ahe)
sd(cps1992ahe)/length(cps1992ahe)
sd(cps2008ahe)/length(cps2008ahe)

CPI2008<-215.2
CPI1992<-140.3
cpiratio<-CPI2008/CPI1992 
adjcps1992ahe<-cpiratio*cps1992ahe
mean(adjcps1992ahe)

m1<-mean(cps2008bach)
m2<-mean(cps2008nobach)
s1<-sd(cps2008bach)
s2<-sd(cps2008nobach)
nbach<-length(cps2008bach)
nobach<-length(cps2008nobach)

sqrt(s1^2/nbach+s2^2/nobach)
marginHS <- 1.94*s2/sqrt(nobach)
marginCOL <- 1.94*s1/sqrt(nbach)

cps1992bach<- cps$ahe[which(cps$bachelor==1 & cps$year==1992)]
cps1992nobach<-cps$ahe[which(cps$bachelor!=1 & cps$year==1992)]

cps1992bach<-cps1992bach*cpiratio
cps1992nobach<-cps1992nobach*cpiratio

m1<-mean(cps1992bach)
m2<-mean(cps1992nobach)
s1<-sd(cps1992bach)
s2<-sd(cps1992nobach)
nbach<-length(cps1992bach)
nobach<-length(cps1992nobach)

sqrt(s1^2/nbach+s2^2/nobach)
marginHS <- 1.94*s2/sqrt(nobach)
marginCOL <- 1.94*s1/sqrt(nbach)

m2<-mean(cps1992nobach)
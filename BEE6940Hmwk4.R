#BEE 6940 Hmwk

load('C:/Users/kmd266/Documents/BEE6940/Hmwk4/mylat.RData')
load('C:/Users/kmd266/Documents/BEE6940/Hmwk4/mylon.RData')
load('C:/Users/kmd266/Documents/BEE6940/Hmwk4/PDSI.final.RData')


pdsi.coords <- expand.grid('lon'=mylon,'lat'=mylat)


PDSI.final.nonan <- na.omit(t(PDSI.final))


pc.out<-prcomp(t(PDSI.final.nonan))
summary(pc.out)


eigs <- pc.out$sdev^2
eigs[1]/sum(eigs)
#Determining Proportion of Variance
prop.var <- eigs/sum(eigs)
first10<-sum(prop.var[1:10])
#The first 10 explain 66.7% of the variance.
plot(seq(eigs[1:50]), eigs[1:50],ylab='Eigenvalue',xlab='Index',main='Eigenvalue Scree Plot',
     ylim=range(c(eigs[1:50]-eigerror[1:50], eigs[1:50]+eigerror[1:50]))
     ,pch=19
     
)
arrows(seq(eigs[1:50]), eigs[1:50]-eigerror[1:50], seq(eigs[1:50]), eigs[1:50]+eigerror[1:50], length=0.05, angle=90, code=3)
points(eigs[1:5],pch=19,col='red')
abline(v=c(5.5),col='red')


#USE North's Rule to decide which PC's to keep (My version)
n<-533

eigerror<-sqrt(2/n)*eigs
upperr <- eigs+eigerror
lowerr <- eigs-eigerror
north<-eigs*0
for (i in 2:20) {
  if (lowerr[i]>upperr[i+1] & upperr[i]<lowerr[i-1]) {
    north[i] <- 1 
  }
  else {
    north[i]<-0
  }
}


#Different coding style for North's rule.
Lambda_err <- sqrt(2/n)*eigs
upper.lim <- eigs+Lambda_err
lower.lim <- eigs-Lambda_err
NORTHok=0*eigs
for(i in seq(eigs)){
  Lambdas <- eigs
  Lambdas[i] <- NaN
  nearest <- which.min(abs(eigs[i]-Lambdas))
  if(nearest > i){
    if(lower.lim[i] > upper.lim[nearest]) NORTHok[i] <- 1
  }
  if(nearest < i){
    if(upper.lim[i] < lower.lim[nearest]) NORTHok[i] <- 1
  }
}
n_sig <- min(which(NORTHok==0))-1




#cur_PC is the current PC under consideration. We save the slope coefficients in
#reg.coef, and insert an NA if there is no data for a particular column of PDSI.final
reg.coef<-apply(PDSI.final,2,function(x) {
  mycoef<-NA
  if(!is.na(x[1])){mycoef<-lm(x~pc.out$x[,5])$coef[2]}
  return(mycoef)
  }
  )


reg.coef.PC <- matrix(reg.coef,byrow=FALSE,ncol=length(mylat))

mylevel<-seq(-0.05,0.05,length.out=15)
mycolor <- colorRampPalette(c("red","orange","white","green","blue"))(length(mylevel)-1)

filled.contour(
  mylon,mylat,reg.coef.PC,zlim=c(-max(abs(reg.coef.PC),na.rm=T),max(abs(reg.coef.PC),na.rm=T)),main="PC5 Regression",
  plot.axes={
    map("world",add=TRUE);
    map("state",add=TRUE,col='grey',lwd=1.5);
  },
  col=mycolor
) 






# Part 3 Forecasting ------------------------------------------------------
library(forecast)
forkast<-c()
for (ii in 1:dim(pc.out$x[,1:5])[2]){
  mymodel <- arima(pc.out$x[,ii],order=c(1,0,0),include.mean=FALSE)
  pc.forecast <- forecast(mymodel,h=1)$mean[1]
  forkast<-append(forkast,pc.forecast)
  
}

x_prediction <-forkast%*%t(pc.out$rotation[,1:5])




# Rotating EOF's ----------------------------------------------------------

my.varimax <- varimax(pc.out$rotation[,1:5])


library(corrplot)
library(maps)
newPC <- t(PDSI.final.nonan)%*%my.varimax$loadings
rPC <- newPC
corrplot(cor(pc.out$x[,1:5]))

colnames(rPC) <- c("rPC1","rPC2","rPC3","rPC4","rPC5")
corrplot(cor(rPC,pc.out$x[,1:5]),method='number',ylab="rPC")
corrplot(cor(rPC))




reg.coef<-apply(PDSI.final,2,function(x) {
  mycoef<-NA
  if(!is.na(x[1])){mycoef<-lm(x~newPC[,5])$coef[2]}
  return(mycoef)
}
)


reg.coef.PC <- matrix(reg.coef,byrow=FALSE,ncol=length(mylat))

mylevel<-seq(-0.05,0.05,length.out=19)
mycolor <- colorRampPalette(c("red","orange","white","green","blue"))(length(mylevel)-1)

filled.contour(
  mylon,mylat,reg.coef.PC,zlim=c(-max(abs(reg.coef.PC),na.rm=T),max(abs(reg.coef.PC),na.rm=T)),main="rPC5 Regression",
  plot.axes={
    map("world",add=TRUE);
    map("state",add=TRUE,col='grey',lwd=1.5);
  },
  col=mycolor
) 


cor(c(rPC=newPC[,2],pc.out$x[,2]))













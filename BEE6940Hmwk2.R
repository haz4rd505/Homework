library(xlsx)
library(corrplot)
library(car)
library(glmnet)
gages2<-read.xlsx('C:/Users/kmd266/Documents/BEE6940/gages2.data.xlsx',sheetName='gages2.data.txt')
vars2<-read.xlsx('C:/Users/kmd266/Documents/BEE6940/gagesII_sept30_2011_var_desc.xlsx',sheetName='variable_descriptions')

#STEP 2 Scale and calculation covariates
#gages2.sc<-scale(as.matrix(gages2),center=TRUE,scale=TRUE)
gs <- apply(as.matrix(gages2),2,FUN = function(x) ((x-mean(x,na.rm =T))/sd(x,na.rm=T)))
df<-as.data.frame(gs)

vifdf<-names(df)
vifdf<-as.data.frame(vifdf)
vifdf$vif<-rep(NA)
for(i in 1:length(vifdf[,1])){
  y<-as.data.frame(df[,i])
  names(y)<-names(df)[i]
  x<-df[,-i]
  x<-cbind(y, x)
  m<-lm(x[,1]~x[,2]+x[,3]+x[,4]+x[,5]+x[,6]+
          x[,7]+x[,8]+x[,9]+x[,10]+x[,11]+x[,12]+
          x[,13]+x[,14]+x[,15]+x[,16]+x[,17]+x[,18]+
          x[,19]+x[,20]+x[,21]+x[,22]+x[,23]+x[,24]+
          x[,25]+x[,26]+x[,27]+x[,28]+x[,29]+x[,30]+
          x[,31]+x[,32]+x[,33]+x[,34]+x[,35]+x[,36]-1)
  vifdf$vif[i]<-1/(1-summary(m)$r.squared)
}


lasso<-cv.glmnet(gs[,2:37],gs[,1],alpha=1)
ridge<-cv.glmnet(gs[,2:37],gs[,1],alpha=0)
model1 <- lm(gs[,1] ~ gs[,2])






gagescorr <- cor(gages2.sc)
vif <- 1/(1-gagescorr^2)
corrplot(gagescorr,method='square')

#As you would expect the R-factor has a large positive correlation with average precipitation, as it is essentially calculating the erosion force of the annual precipiation.


df <- apply(gages2.sc,2,function(x) lm(gages2.sc[,1] ~ x))




##STEP 3
pvals<-c()
for (i in 1:36) {
  model1<-lm(gs[,1]~gs[,i+1]-1)
  pvals<-cbind(pvals,summary(model1)$coefficients[,4])
  model1
  
}
pvals<-cbind(0,pvals)
checkthis <- cbind(as.matrix(vifdf),t(pvals))
runoff<-gs[,1]
model1<-lm(runoff~gs)

x<-gs
m<-lm(x[,1]~x[,2]+x[,3]+x[,4]+x[,5]+x[,6]+
        x[,7]+x[,8]+x[,9]+x[,10]+x[,11]+x[,12]+
        x[,13]+x[,14]+x[,15]+x[,16]+x[,17]+x[,18]+
        x[,19]+x[,20]+x[,21]+x[,22]+x[,23]+x[,24]+
        x[,25]+x[,26]+x[,27]+x[,28]+x[,29]+x[,30]+
        x[,31]+x[,32]+x[,33]+x[,34]+x[,35]+x[,36]+x[,37]-1)




##STEP 4

lasso<-cv.glmnet(gs[,2:37],gs[,1],alpha=1)
ridge<-cv.glmnet(gs[,2:37],gs[,1],alpha=0)

plot(lasso$lambda,lasso$cvm,main='CVM vs Lambda Lasso')
plot(ridge$lambda,ridge$cvm,main='CVM vs Lambda Ridge')

lassoed_into_it <- lasso$lambda[lasso$cvm==min(lasso$cvm)]
the_chosen_one <- ridge$lambda[ridge$cvm==min(ridge$cvm)]

lassofinal<-glmnet(gs[,2:37],gs[,1],alpha=1,lambda = lassoed_into_it)
ridgefinal<-glmnet(gs[,2:37],gs[,1],alpha=0,lambda = the_chosen_one)

comp<-cbind(m$coefficients,lassofinal$beta,ridgefinal$beta,vifdf$vif)
colnames(comp)<-c("OLS","LASSO","RIDGE","VIF")
write.table(as.data.frame(as.matrix(comp)),file="C:/Users/kmd266/Documents/BEE6940/comp.csv",sep=',')



### SUBSETTING THE DATA AND DoiNG raNDOM STuFF TO IT 100 TIMES

OLS_mse <- c()
ridge_mse <- c()
lasso_mse <- c()
for (i in 1:100) {

  randrows <- sample(c(1:211),size=105)

  sbset1 <- gs[randrows,]
  sbset2 <- gs[-randrows,]

  model1 <- lm(sbset1[,1] ~ sbset1[,2]+sbset1[,3]+sbset1[,4]+sbset1[,5]+sbset1[,6]+
       sbset1[,7]+sbset1[,8]+sbset1[,9]+sbset1[,10]+sbset1[,11]+sbset1[,12]+
       sbset1[,13]+sbset1[,14]+sbset1[,15]+sbset1[,16]+sbset1[,17]+sbset1[,18]+
       sbset1[,19]+sbset1[,20]+sbset1[,21]+sbset1[,22]+sbset1[,23]+sbset1[,24]+
       sbset1[,25]+sbset1[,26]+sbset1[,27]+sbset1[,28]+sbset1[,29]+sbset1[,30]+
       sbset1[,31]+sbset1[,32]+sbset1[,33]+sbset1[,34]+sbset1[,35]+sbset1[,36]+sbset1[,37]-1)
  
  
  ##OLS_mse
  betaOLS <- as.matrix(model1$coefficients)
  OLS_mse <- cbind(OLS_mse,mean((sbset2[,1]-sbset2[,2:37]%*%betaOLS)^2))

  lasso<-cv.glmnet(sbset1[,2:37],sbset1[,1],alpha=1)
  ridge<-cv.glmnet(sbset1[,2:37],sbset1[,1],alpha=0)
  
  laslambmin <- lasso$lambda[lasso$cvm==min(lasso$cvm)]
  riglambmin <- ridge$lambda[ridge$cvm==min(ridge$cvm)]
  
  lassofinal<-glmnet(sbset2[,2:37],sbset2[,1],alpha=1,lambda = laslambmin)
  ridgefinal<-glmnet(sbset2[,2:37],sbset2[,1],alpha=0,lambda = riglambmin)
  
  ridge_mse <- cbind(ridge_mse,mean((sbset2[,1]-sbset2[,2:37]%*%ridgefinal$beta)^2))
  lasso_mse <- cbind(lasso_mse,mean((sbset2[,1]-sbset2[,2:37]%*%lassofinal$beta)^2))
  
  
  
}



boxdat<-cbind(t(OLS_mse),t(ridge_mse),t(lasso_mse))
colnames(boxdat)<-c("OLS","ridge","lasso")
boxplot(boxdat,main="Mean Squared Error by Regression Type",ylab="MSE")



















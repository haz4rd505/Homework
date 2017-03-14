### Gap-filling

gap_filling <- function(x){ 
  
  for (i in 1:nrow(datmat)){
    if (is.na(sum(x[i,]))){
      nacols <- which(is.na(x[i,])==TRUE)
      sub1 <-x[,nacols]
      sub2 <-x[,-nacols]
      newx <- cbind(sub1,sub2)
      covy <- cov(newx,use='pairwise.complete')
      
      if (length(nacols) > 1){
        mu1 <- colMeans(sub1,na.rm=T)
        mu2 <- colMeans(sub2,na.rm=T)
      }
  
      else {
        mu1 <- mean(sub1,na.rm=T)
        mu2 <-colMeans(sub2,na.rm=T)
      }
      
      sigma12 <- covy[(length(nacols)+1):ncol(x),1:length(nacols)]
      sigma22 <- covy[(length(nacols)+1):ncol(x),(length(nacols)+1):ncol(x)]
      
      munew <- mu1 + sigma12%*%solve(sigma22)%*%(sub2[i,]-mu2)
      #nval<-c()
      #xrest<-c()
      #for (j in 1:length(nacols)) {
      #  temp <- length(na.omit(sub1[,j]))
      #  temp2<-sum(na.omit(sub1[,j]))
      #  nval <- append(nval,temp)
      #  xrest<-append(xrest,temp2)
      #}
      #newdata<- munew*(nval+1)-xrest
      #
      x[i,nacols] <- munew
    }

  }
  
  return(x)
  
  }
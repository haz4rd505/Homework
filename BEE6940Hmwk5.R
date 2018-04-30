##BEE 6940 HMWK 5 
library(xlsx)
md_t<-read.xlsx('C:/Users/kmd266/Documents/BEE6940/Maryland.Trout.xlsx',sheetName = 'Maryland.Trout.csv')

md.sc <- scale(md_t[3:7],center=T,scale=T)

#Creating Distance Matrix
d<-dist(md.sc)


#PLOTTING DENDROGRAMS
myclust1 <- hclust(d,method='single')
plot(myclust1, main="Single Linkage")

myclust2 <- hclust(d,method='complete')
plot(myclust2, main="Complete Linkage")

myclust3 <- hclust(d,method='centroid')
plot(myclust3, main="Centroid Linkage")
     


num_cluster<-2    


gmean <- colMeans(md.sc)
mycut <- cutree(myclust2,k=num_cluster)
clust1 <- md.sc[mycut==1,]
clust2 <- md.sc[mycut==2,]


clust1 <- md_t[mycut==1,]
clust2 <- md_t[mycut==2,]


mea1<-colMeans(clust1[3:7])
mea2<-colMeans(clust2[3:7])



betw1<- sum(colMeans(clust1)^2)
betw2<- sum(colMeans(clust2)^2)
betw3<- sum(colMeans(clust3)^2)

  ## Part 2 K-means Clustering
vark<-c(0)
for (ii in 2:10) {
  clust.k <- kmeans(md.sc,center=ii,nstart=10)
  vark <- append(vark,clust.k$betweenss/clust.k$totss)
}

plot(vark, ylab = 'Variance Explained',xlab='k',main='Variance Explained by Cluster')
lines(vark)




clust.k <- kmeans(md.sc,center=2,nstart=10)

clust1 <- md_t[clust.k$cluster==1,]
clust2 <- md_t[clust.k$cluster==2,]

mea1<-colMeans(clust1[3:7])
mea2<-colMeans(clust2[3:7])
##Part 3 

train.set <- md_t[1:60,]
test.set <- md_t[61:84,]

train.set[3:7]<-scale(train.set[3:7],center=T,scale=T)
notrt.train<-train.set[train.set[2]==0,]
yestrt.train<-train.set[train.set[2]==1,]




S1 <- cov(notrt.train[3:7])
S2 <- cov(yestrt.train[3:7])

gmean1<-colMeans(notrt.train[3:7])
gmean2<-colMeans(yestrt.train[3:7])

Spool <- (25-1)/(58)*S1 + (35-1)/58*S2
alpha <- solve(Spool)%*%(gmean1-gmean2)


delta <- t(alpha)%*%t(scale(test.set[3:7],scale=T,center=T))

M <- .5*t(gmean1-gmean2)%*%solve(Spool)%*%(gmean1+gmean2)
M <- as.numeric(M)
g1 <- test.set[delta[1,]>=M,]
g2 <- test.set[delta[1,]<M,]

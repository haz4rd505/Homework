#Stats homework
set.seed(1976)
mew<- 20
sig<- sqrt(380)
upbound1 <- mew+2*sig
lowbound1 <- mew-2*sig
upbound2 <- mew+3*sig
lowbound2 <- mew-3*sig

numset <- rgeom(10000, 1/20)

outliers2sigma<- numset[which(numset>upbound1 | numset<lowbound1)]
outliers3sigma<- numset[which(numset>upbound2 | numset<lowbound2)]

##
set.seed(1976)
mew<- .5
sig<- sqrt(1/12)
numset <- runif(10000, min= 0, max = 1)
mean(numset)
sd(numset)

##
#Stats homework
set.seed(1976)
mew<- 10
sig<- sqrt(10)
upbound1 <- mew+2*sig
lowbound1 <- mew-2*sig
upbound2 <- mew+3*sig
lowbound2 <- mew-3*sig

numset <- rpois(10000, 10)

outliers2sigma<- numset[which(numset>upbound1 | numset<lowbound1)]
outliers3sigma<- numset[which(numset>upbound2 | numset<lowbound2)]
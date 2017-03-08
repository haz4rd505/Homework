west<-read.table(file='C:/Users/kmd266/Downloads/annualwestfield')
hunt<-read.table(file='C:/Users/kmd266/Downloads/annualhuntington')


wtr_avgW<-west$V6[3:82]
wtr_avgH<-hunt$V6[3:82]

hunt.sc<-scale(as.numeric(as.character(wtr_avgH)),center=TRUE,scale=TRUE)
west.sc<-scale(as.numeric(as.character(wtr_avgW)),center=TRUE,scale=TRUE)

mix.sc<-cbind(hunt.sc,west.sc)

#Problem 2
transformmatrix <-matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)),nrow=2)

rot_mix.sc<-mix.sc%*%transformmatrix
plot(mix.sc, xaxt="n", yaxt="n",xlab='huntington(scaled) mm^3/s',ylab='westfield(scaled) mm^3/s')
par(new=TRUE)
plot(rot_mix.sc,main='Problem2 ',col='red',ylim=c(-4,4),xlim=c(-4,4),xlab='huntington(scaled) mm^3/s',ylab='westfield(scaled) mm^3/s')
abline(h=0,v=0, col = "gray60")


#Problem 3
vec1x <- c(5,0)
vec2x <- c(-5,0)
vec3y <- c(0,5)
vec4y<- c(0,-5)

t1 <- vec1x %*% transformmatrix 
t2 <-vec2x %*% transformmatrix
t3 <-vec3y %*% transformmatrix
t4 <-vec4y %*% transformmatrix


#plot(rot_mix.sc,col='red',ylim=c(-4,4),xlim=c(-4,4),xlab='huntington(scaled) mm^3/s',ylab='westfield(scaled) mm^3/s')
#abline(h=0,v=0, col = "gray60")
par(new=TRUE)
segments(t1[1],t1[2],t2[1],t2[2],col='red')
par(new=TRUE)
segments(t3[1],t3[2],t4[1],t4[2],col='red')

norm_vec <- function(x) sqrt(sum(x^2))

P <- 1/norm_vec(t3)^2*t(t3)%*%t3
projdata<-mix.sc%*%P
plot(mix.sc, xaxt="n", yaxt="n",xlab='huntington(scaled) mm^3/s',ylab='westfield(scaled) mm^3/s')
par(new=TRUE)
plot(projdata,xaxt="n", main='Problem 3',yaxt="n",xlab='huntington(scaled) mm^3/s',ylab='westfield(scaled) mm^3/s', col = 'blue')

par(new=TRUE)
segments(t1[1],t1[2],t2[1],t2[2],col='red')
par(new=TRUE)
segments(t3[1],t3[2],t4[1],t4[2],col='red')
#t3 and t4 are the projected axis along the data


#PROBLEM 4
#magnitude from problem 2
magp2<-rot_mix.sc[,1]

#magnitude from problem 3
magp3<-apply(projdata,1,function(x) x[2]/abs(x[2])*norm_vec(x))

plot(magp3,main='Problem 4',ylab='Distance to Origin',type='l')
points(magp3_pos,main='Problem 4')
points(magp3_pos,type='l')
points(magp2,col='red')


#Problem 5
west5<-as.numeric(as.character(wtr_avgW<-west$V6[3:82]))
hunt5<-as.numeric(as.character(wtr_avgH<-hunt$V6[3:82]))
x.tr<-magp3

plot(x.tr,west5,ylab='Westfield data')
plot(x.tr,hunt5,ylab='Huntington data')


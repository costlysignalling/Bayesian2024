library(abc)

# Normal distribution demo
set.seed(13)
abctargetdata=rnorm(20, 52, 4)
abctargetdata

abctarget=c(median(abctargetdata), range(abctargetdata)[1]-range(abctargetdata)[2])

abcmodel=function(mu, sigma){
  temp=rnorm(20, mu, sigma)
  ret=c(median(temp),range(temp)[1]-range(temp)[2])
  return(ret)
}

abcprior=data.frame(mu=runif(10000,0,100), sigma=runif(10000,0,10))
hist(abcprior$mu, breaks = seq(0,100,1))

hist(abcprior$sigma, breaks = seq(0,10,0.1))

abcsummarystats=t(mapply(abcmodel, mu=abcprior$mu, sigma=abcprior$sigma))
abcsummarystats[1:10,]

abcposter=abc(target=abctarget, param=abcprior, sumstat=abcsummarystats, tol=.1, method =
                "rejection")

abcposter$unadj.values[1:10,]


hist(abcprior$mu, breaks = seq(0,100,1), main="",xlab="mu")
hist(abcposter$unadj.values[,1], add=T, col="red", breaks=seq(0,100,1))
abline(v=52,lwd=3, lty=2)



hist(abcprior$sigma, breaks = seq(0,10,0.1), main="",xlab="sigma")
hist(abcposter$unadj.values[,2], add=T, col="red", breaks=seq(0,10,0.1))
abline(v=4,lwd=3, lty=2)


# Neutral theory demo
library(untb)

set.seed(13)
target <- untb(start=rep(1,100),prob=0.8, gens=50,keep=TRUE)
target[50,]

target.curve <- species.count(target)

plot(1:length(target.curve),target.curve, type="l",lwd=3,xlab="t", ylab="richness")

untbprior=runif(10000, 0,1)

untb.streamlinedcurve=function(p){
  a=untb(start=rep(1,100),prob=p, gens=50,keep=TRUE)
  return(species.count(a))
}

untbsummarystatscurve=t(mapply(untb.streamlinedcurve, p=untbprior))

untbpostercurve=abc(target=target.curve, param=untbprior, sumstat=untbsummarystatscurve, tol=.1, method =
                      "rejection")



hist(untbprior, seq(0,1,0.01), main="",xlab="p")
hist(untbpostercurve$unadj.values, seq(0,1,0.01), col="red", add=T)
abline(v=0.8,lwd=3, lty=2)

library(scales)
plot(1:length(target.curve),target.curve, type="l",lwd=3, xlab="t", ylab="richness")
for (i in 1:length(untbsummarystatscurve[,1])){
  lines(1:length(target.curve),untbsummarystatscurve[i,], type="l",col=alpha("blue",0.01))
}
for (i in 1:length(untbpostercurve$ss[,1])){
  lines(1:length(target.curve),untbpostercurve$ss[i,], type="l",col=alpha("black",0.01))
}

# Exercise 1

target.fin=target.curve[length(target.curve)]
untbsummarystatsfin=untbsummarystatscurve[,length(target.curve)]

untbposterfin=abc(target=target.fin, param=untbprior, sumstat=untbsummarystatsfin, tol=.1, method =
                      "rejection")

hist(untbprior, seq(0,1,0.01), main="",xlab="p")
hist(untbposterfin$unadj.values, seq(0,1,0.01), col="red", add=T)
abline(v=0.8,lwd=3, lty=2)

plot(1:length(target.curve),target.curve, type="l",lwd=3, xlab="t", ylab="richness")
for (i in 1:length(untbsummarystatscurve[,1])){
  lines(1:length(target.curve),untbsummarystatscurve[i,], type="l",col=alpha("blue",0.01))
}
for (i in which(untbposterfin$region)){
  lines(1:length(target.curve),untbsummarystatscurve[i,], type="l",col=alpha("black",0.01))
}



# Exercise 2

target.beg=target.curve[3]
untbsummarystatsbeg=untbsummarystatscurve[,3]

untbposterbeg=abc(target=target.beg, param=untbprior, sumstat=untbsummarystatsbeg, tol=.1, method =
                    "rejection")

hist(untbprior, seq(0,1,0.01), main="",xlab="p")
hist(untbposterbeg$unadj.values, seq(0,1,0.01), col="red", add=T)
abline(v=0.8,lwd=3, lty=2)

plot(1:length(target.curve),target.curve, type="l",lwd=3, xlab="t", ylab="richness")
for (i in 1:length(untbsummarystatscurve[,1])){
  lines(1:length(target.curve),untbsummarystatscurve[i,], type="l",col=alpha("blue",0.01))
}
for (i in which(untbposterbeg$region)){
  lines(1:length(target.curve),untbsummarystatscurve[i,], type="l",col=alpha("black",0.01))
}


# Exercise 3

untbposterfinll=abc(target=target.fin, param=untbprior, sumstat=untbsummarystatsfin, tol=.3, method ="loclinear")

untbposterfinll$weights
untbposterfinll$adj.values
untbposterfinll$unadj.values

plot(untbposterfinll$adj.values~untbposterfinll$unadj.values)
points(untbposterfinll$unadj.values[which(untbposterfinll$weights==1),],untbposterfinll$adj.values[which(untbposterfinll$weights==1),],col="red")

hist(untbprior, seq(0,1.1,0.01), main="",xlab="p")
hist(untbposterfinll$adj.values, seq(0,1.1,0.01), col="red", add=T)
abline(v=0.8,lwd=3, lty=2)

# Exercise 4
untb.streamlinedcurve5=function(p){
  a=untb(start=rep(1,100),prob=p,D=5, gens=50,keep=TRUE)
  return(species.count(a))
}

untbsummarystatscurve5=t(mapply(untb.streamlinedcurve5, p=untbprior))
untbsummarystats5fin=untbsummarystatscurve5[,length(target.curve)]



untbfin1or5=postpr(target=target.fin, 
       index=c(rep("jedna",10000),rep("pet",10000)), 
        sumstat=c(untbsummarystatsfin, untbsummarystatsfin),tol=.1,method="rejection")

untbfin1or5
summary(untbfin1or5)

# Exercise 5

library(rethinking)
normmod=alist(
  y~dnorm(mu,sigma),
  mu~dunif(0,100),
  sigma~dunif(0,10)
)

normpost=ulam(flist = normmod,data=list(y=abctargetdata))

mupost=extract.samples(normpost)$mu
sigmapost=extract.samples(normpost)$sigma

hist(abcprior$mu, breaks = seq(0,100,1), main="",xlab="mu")
hist(abcposter$unadj.values[,1], add=T, col="red", breaks=seq(0,100,1))
hist(mupost,add=T, col="blue", breaks=seq(0,100,1))
abline(v=52,lwd=3, lty=2)

hist(abcprior$sigma, breaks = seq(0,10,0.1), main="",xlab="sigma")
hist(abcposter$unadj.values[,2], add=T, col="red", breaks=seq(0,10,0.1))
hist(sigmapost,add=T, col="blue", breaks=seq(0,10,0.1))
abline(v=4,lwd=3, lty=2)

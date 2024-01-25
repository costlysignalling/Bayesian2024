# library(mvtnorm)
# 
# sigma=diag(2)*1
# sigma[1,2]=-0.5
# sigma[2,1]=-0.5
# sigma
# 
# climate=seq(0.303,0.600,0.003)
# mu1=10+5*climate
# mu2=10+5*climate
# r=matrix(0,100,2)
# 
# for (i in 1:100){
#   r[i,]=rmvnorm(1,c(mu1[i],mu2[i]), sigma)
# }
# r
# 
# save(r,climate,file="crdata.RData")
load("crdata.RData")

cor(r[,1],r[,2])
plot(r[,1],r[,2])
var(r[,2])
var(r[,1])
cov(r[,1],r[,2])

library(rethinking)

cycmodel<-alist(
  #model
  r1~dnorm(mu1, sigma1),
  r1~dnorm(mu2, sigma2),
  mu1<-alpha+beta*climate+gamma1*r2,
  mu2<-alpha+beta*climate+gamma2*r1,
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  gamma1~dnorm(0,100),
  gamma2~dnorm(0,100),
  sigma1~dunif(0,100),
  sigma2~dunif(0,100)
  

)

cycposterior=ulam(cycmodel,
                 data=list(r1=r[,1],r2=r[,2], climate=climate))

precis(cycposterior)
traceplot(cycposterior)

mvmodel<-alist(
  #model
  c(r1,r2)~dmvnorm(c(mu1,mu2), Rho, sigmas),
  mu1<-alpha+beta*climate,
  mu2<-alpha+beta*climate,
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  sigmas~dunif(0,100),
  Rho~dlkjcorr(2)
  
)

mvposterior=ulam(mvmodel,
                     data=list(r1=r[,1],r2=r[,2], climate=climate))

precis(mvposterior,depth = 3)
traceplot(mvposterior)


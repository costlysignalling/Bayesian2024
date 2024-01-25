# library(mvtnorm)
# 
# sigma=diag(2)*1
# sigma[1,2]=-0.5
# sigma[2,1]=-0.5
# sigma
# 
# climate=seq(0.303,0.600,0.003)
# mu1=10+5*climate
# mu2=20+2*climate
# r2=matrix(0,100,2)
# 
# for (i in 1:100){
#   r2[i,]=rmvnorm(1,c(mu1[i],mu2[i]), sigma)
# }
# r
# 
# save(r2,climate,file="crdata2.RData")

# Example 1 
# What if both plants have different response to climate?
load("crdata2.RData")

cor(r2[,1],r2[,2])
plot(r2[,1]~r2[,2])
plot(r2[,1]~climate)
plot(r2[,2]~climate)


library(rethinking)

mvmodel2<-alist(
  #model
  c(r1,r2)~dmvnorm(c(mu1,mu2), Rho, sigmas),
  mu1<-alpha1+beta1*climate,
  mu2<-alpha2+beta2*climate,
  alpha1~dnorm(0,100),
  beta1~dnorm(0,100),
  alpha2~dnorm(0,100),
  beta2~dnorm(0,100),
  sigmas~dunif(0,100),
  Rho~dlkjcorr(2)
  
)

mvposterior2=ulam(mvmodel2,
                     data=list(r1=r[,1],r2=r[,2], climate=climate))

precis(mvposterior2,depth = 3)
traceplot(mvposterior2)

# Example 2 Simulate data with response between 0 and 1 and analyze them

# Example 3 Simulate data where the strength of correlation is changing with climate

# Example 4 Tweak the cyclical model so that it works

## Simple models
# Exercise 1
library(rethinking)
heightgirlsvar=rnorm(10, 140, 20)
heightboysvar=rnorm(10, 160, 5)

t.test(heightboysvar,heightgirlsvar, var.equal = T)
t.test(heightboysvar,heightgirlsvar, var.equal = F)

ttestvarmodel=alist(
  #likelihood
  heightgirlsvar~dnorm(mugirls, sigmagirls),
  heightboysvar~dnorm(muboys, sigmaboys),
  #priors
  mugirls~dunif(100,200),
  muboys~dunif(100,200),
  sigmagirls~dunif(0,1000),
  sigmaboys~dunif(0,1000)
  
  
)

ttestvarposterior=ulam(ttestvarmodel,
                    data=list(heightgirlsvar=heightgirlsvar, heightboysvar=heightboysvar))

precis(ttestvarposterior)
plot(ttestvarposterior)
traceplot(ttestvarposterior)

girlsvarposterior=extract.samples(ttestvarposterior)$mugirls
boysvarposterior=extract.samples(ttestvarposterior)$muboys
par(mfrow=c(1,1))
hist(girlsvarposterior-boysvarposterior)

girlsvarposteriorsd=extract.samples(ttestvarposterior)$sigmagirls
boysvarposteriorsd=extract.samples(ttestvarposterior)$sigmaboys
hist(girlsvarposteriorsd-boysvarposteriorsd)


# Exercise 2

ttestvarprimodel=alist(
  #likelihood
  heightgirlsvar~dnorm(mugirls, sigmagirls),
  heightboysvar~dnorm(muboys, sigmaboys),
  #priors
  mugirls~dunif(180,200),
  muboys~dunif(180,200),
  sigmagirls~dunif(0,1000),
  sigmaboys~dunif(0,1000)
  
  
)

ttestvarpriposterior=ulam(ttestvarprimodel,
                       data=list(heightgirlsvar=heightgirlsvar, heightboysvar=heightboysvar))

precis(ttestvarpriposterior)
plot(ttestvarpriposterior)
traceplot(ttestvarpriposterior)


# Exercise 3
datareg=data.frame(x=seq(0,10,by=1))
alpha=10
beta=2
sigma=3
datareg$y=rnorm(11,alpha+beta*datareg$x,sigma)


heightgirls=rnorm(10, 140, 10)
heightboys=rnorm(10, 160, 10)

datattest=data.frame(x=c(rep(0,10), rep(1,10)),y=c(heightboys, heightgirls))

regresmodel=alist(
  #likelihood
  y~dnorm(yexp, sigma),
  yexp <- alpha+beta*x,
  #priors
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  sigma~dunif(0 , 100)
  
)

regresposterior=ulam(regresmodel,
                     data=list(x=datattest$x, 
                               y=datattest$y))

precis(regresposterior)
plot(regresposterior)
traceplot(regresposterior)

alphaes=extract.samples(regresposterior)$alpha
betaes=extract.samples(regresposterior)$beta

girlmuses=alphaes+betaes
summary(girlmuses)

# Exercise 4

datareghet=data.frame(x=seq(0,10,by=0.5))
alpha=10
beta=5
sigma=3
datareghet$y=rnorm(21,alpha+beta*datareghet$x,sigma*datareghet$x)
plot(datareghet$y~datareghet$x)

regreshetmodel=alist(
  #likelihood
  y~dnorm(yexp, sgexp),
  yexp <- alpha+beta*x,
  sgexp <- sigmaint+sigmasl*x,
  #priors
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  sigmasl~dunif(0,100),
  sigmaint~dunif(0,100)
  
)

regreshetposterior=ulam(regreshetmodel,
                     data=list(x=datareghet$x,
                               y=datareghet$y), iter = 4000)

precis(regreshetposterior)
traceplot(regreshetposterior)

yexpregressionhet=link(regreshetposterior)$yexp

yexpmeanregressionhet=apply(yexpregressionhet,2, mean)
yexpPIregressionhet=apply(yexpregressionhet,2, PI)

plot(datareghet$y~datareghet$x)
lines(datareghet$x , yexpmeanregressionhet)
shade(yexpPIregressionhet , datareghet$x)

# Exercise 5

dataregbipred=data.frame(x1=seq(0,10,by=0.5), x2=sample(seq(0,10,by=0.5),21, replace = F))
alpha=10
beta1=2
beta2=4
sigma=10
dataregbipred$y=rnorm(21,alpha+beta1*dataregbipred$x1+beta2*dataregbipred$x2,sigma)
plot(dataregbipred$y~dataregbipred$x1)
plot(dataregbipred$y~dataregbipred$x2)

plot(dataregbipred$x1~dataregbipred$x2)

regresbipredmodel=alist(
  #likelihood
  y~dnorm(yexp, sigma),
  yexp <- alpha+beta1*x1+beta2*x2,
  #priors
  alpha~dnorm(0,100),
  beta1~dnorm(0,100),
  beta2~dnorm(0,100),
  sigma~dunif(0 , 100)
  
)


regresbipredposterior=ulam(regresbipredmodel,
                     data=list(x1=dataregbipred$x1,x2=dataregbipred$x2, y=dataregbipred$y))

precis(regresbipredposterior)
traceplot(regresbipredposterior)


# Exercise 6

dataregbires=data.frame(x=seq(0,10,by=0.5))
alpha=10
beta1=2
beta2=4
sigma=10
dataregbires$y1=rnorm(21,alpha+beta1*dataregbires$x,sigma)
dataregbires$y2=rnorm(21,alpha+beta2*dataregbires$x,sigma)

plot(dataregbires$y1~dataregbires$x)


regresbiresmodel=alist(
  #likelihood
  y1~dnorm(y1exp, sigma1),
  y2~dnorm(y2exp, sigma2),
  
  y1exp <- alpha+beta*x,
  y2exp <- alpha+beta*x,
  
  #priors
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  sigma1~dunif(0 , 100),
  sigma2~dunif(0 , 100)
  
  
)


regresbiresposterior=ulam(regresbiresmodel,
                           data=list(x=dataregbires$x,y2=dataregbires$y2, y1=dataregbires$y1))

precis(regresbiresposterior)
traceplot(regresbiresposterior)

regresbiresmodel1=alist(
  #likelihood
  y1~dnorm(y1exp, sigma),

  y1exp <- alpha+beta1*x,

  #priors
  alpha~dnorm(0,100),
  beta1~dnorm(0,100),
  sigma~dunif(0 , 100)
  
)

regresbiresmodel2=alist(
  #likelihood
  y2~dnorm(y2exp, sigma),
  
  y2exp <- alpha+beta2*x,
  
  #priors
  alpha~dnorm(0,100),
  beta2~dnorm(0,100),
  sigma~dunif(0 , 100)
  
)

regresbiresposterior1=ulam(regresbiresmodel1,
                          data=list(x=dataregbires$x, 
                                    y1=dataregbires$y1))

regresbiresposterior2=ulam(regresbiresmodel2,
                           data=list(x=dataregbires$x, 
                                     y2=dataregbires$y2))


precis(regresbiresposterior)
precis(regresbiresposterior1)
precis(regresbiresposterior2)



corbires=rmvnorm(21,cbind(10,5),
                 matrix(c(10,8,8,10),2,2))
dcorbires=as.data.frame(corbires)

cov(dcorbires)


# Remark on categorical predictors reparameterisation

plot(alphaes,betaes) #estiamtes are correlated

#contrasts
datattest$xrep=ifelse(datattest$x==1,0.5,-0.5)

regresmodel=alist(
  #likelihood
  y~dnorm(yexp, sigma),
  yexp <- alpha+beta*x,
  #priors
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  sigma~dunif(0 , 100)
  
)

regresposteriorrep=ulam(regresmodel,
                     data=list(x=datattest$xrep, 
                               y=datattest$y))

precis(regresposteriorrep)
plot(regresposteriorrep)
traceplot(regresposteriorrep) #mixing is better

alphaesrep=extract.samples(regresposteriorrep)$alpha
betaesrep=extract.samples(regresposteriorrep)$beta

plot(alphaesrep,betaesrep) #estiamtes are less correlated

#index variables
datattest$xrep2=datattest$x+1

regresmodelrep2=alist(
  #likelihood
  y~dnorm(mu, sigma),
  mu <- a[xrep2],
  #priors
  a[xrep2]~dnorm(0,100),
  sigma~dunif(0,100)
  
)

regresposteriorrep2=ulam(regresmodelrep2,
                        data=list(xrep2=datattest$xrep2, 
                                  y=datattest$y))

precis(regresposteriorrep2,depth=2)
plot(regresposteriorrep2)
traceplot(regresposteriorrep2) #mixing is better

alphaesrep2=extract.samples(regresposteriorrep2)$a[,1]
betaesrep2=extract.samples(regresposteriorrep2)$a[,2]

plot(alphaesrep2,betaesrep2) #estimates are less correlated


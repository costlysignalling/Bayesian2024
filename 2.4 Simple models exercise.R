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


# Exercise 4
library(rethinking)
datareg=data.frame(x=seq(0,10,by=1))
alpha=10
beta=2
sigma=3
datareg$y=rnorm(11,alpha+beta*datareg$x,sigma)


heightgirls=rnorm(10, 140, 10)
heightboys=rnorm(10, 160, 10)

datattest=data.frame(x=c(rep(1,10), rep(2,10)),y=c(heightboys, heightgirls))

# Petr's stuff
regresmodel2=alist(
  #likelihood
  y~dnorm(yexp, sigma),
  yexp <- alpha[x],
  #priors
  alpha[x]~dnorm(0,100),
  sigma~dunif(0 , 100)
)


regresposterior2=ulam(regresmodel2,
                     data=list(x=datattest$x, y=datattest$y))

precis(regresposterior)
post<-extract.samples(regresposterior2)
sd(post$alpha+post$beta)
sd(post$alpha-post$beta)

yexpregression=link(regresposterior)
dim(yexpregression)

yexpmeanregression=apply(yexpregression,2, mean)
yexpPIregression=apply(yexpregression,2, PI)

plot(datattest$y~datattest$x)
lines(datattest$x , yexpmeanregression)
shade(yexpPIregression , datattest$x)


# Exercise 5

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
  sgexp <- sigmasl*x+sigmaint,
  #priors
  alpha~dnorm(0,100),
  beta~dnorm(0,100),
  sigmasl~dunif(0,100),
  sigmaint~dunif(0,100)
  
  
)

regreshetposterior=ulam(regreshetmodel,
                     data=list(x=datareghet$x, y=datareghet$y))

precis(regreshetposterior)
traceplot(regreshetposterior)

yexpregressionhet=link(regreshetposterior)$yexp

yexpmeanregressionhet=apply(yexpregressionhet,2, mean)
yexpPIregressionhet=apply(yexpregressionhet,2, PI)

plot(datareghet$y~datareghet$x)
lines(datareghet$x , yexpmeanregressionhet)
shade(yexpPIregressionhet , datareghet$x)

# Exercise 6

dataregbipred=data.frame(x1=seq(0,10,by=0.5), x2=sample(seq(0,10,by=0.5),21, replace = F))
alpha=10
beta1=2
beta2=4
sigma=10
dataregbipred$y=rnorm(21,alpha+beta1*dataregbipred$x1+beta2*dataregbipred$x2,sigma)
plot(dataregbipred$y~dataregbipred$x1)
plot(dataregbipred$y~dataregbipred$x2)


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


# Exercise 7

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
  y1~dnorm(y1exp, sigma),
  y2~dnorm(y2exp, sigma),
  
  y1exp <- alpha+beta1*x,
  y2exp <- alpha+beta2*x,
  
  #priors
  alpha~dnorm(0,100),
  beta1~dnorm(0,100),
  beta2~dnorm(0,100),
  sigma~dunif(0 , 100)
  
)


regresbiresposterior=ulam(regresbiresmodel,
                           data=list(x=dataregbires$x,y2=dataregbires$y2, y1=dataregbires$y1))

precis(regresbipredposterior)
traceplot(regresbipredposterior)

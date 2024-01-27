library(rethinking)
setwd("~/Desktop/Bayesian Biostats/")
load("scdata.RData")

scmodel=alist(
  observed ~ dnorm(realsize[id], sigmam[id]),
  realsize[id] ~ dnorm(mu, sigma),
  mu <- alpha + beta*humidity,
  sigmam[id] ~ dexp(0.001),
  sigma ~ dexp(0.001),
  alpha ~ dnorm(0, 100),
  beta ~ dnorm(0,100)
)


scposterior=ulam(scmodel,
                 data=list(observed=scdata$observed,
                           id=scdata$id,
                           humidity=scdata$humidity),iter = 10000)


precis(scposterior, depth=2)

yexpsc=link(scposterior)

yexpmeansc=apply(yexpsc,2, mean)
yexpPIsc=apply(yexpsc,2, PI)

plot(observed~humidityex, data=scdata, ylim=c(95,110))
lines(scdata$humidity , yexpmeansc)
shade(yexpPIsc , scdata$humidity)

# Example 1

sc1model=alist(
  observed1 ~ dnorm(realsize[id], sigmam[id]),
  realsize[id] ~ dnorm(mu, sigma),
  mu <- alpha + beta*humidity,
  
  sigma ~ dexp(0.001),
  sigmam[id] ~ dexp(0.001),
  alpha ~ dnorm(0, 100),
  beta ~ dnorm(0,100)
  
)

sc1posterior=ulam(sc1model,
                  data=sc1data,iter = 10000)

precis(sc1posterior, depth=2)

traceplot(sc1posterior)

yexpsc1=link(sc1posterior)

yexpmeansc1=apply(yexpsc1,2, mean)
yexpPIsc1=apply(yexpsc1,2, PI)

plot(observed1~humidity, data=sc1data, ylim=c(70,150))
lines(sc1data$humidity , yexpmeansc1)
shade(yexpPIsc1 , sc1data$humidity)


# Example 2

sc1ssmodel=alist(
  observed1 ~ dnorm(realsize[id], sigmam),
  realsize[id] ~ dnorm(mu, sigma),
  mu <- alpha + beta*humidity,
  
  sigma ~ dexp(0.001),
  sigmam ~ dexp(0.001),
  alpha ~ dnorm(0, 100),
  beta ~ dnorm(0,100)
  
)

sc1ssposterior=ulam(sc1ssmodel,
                    data=sc1data,iter = 10000)

precis(sc1ssposterior, depth=2)

yexpsc1ss=link(sc1ssposterior)

yexpmeansc1ss=apply(yexpsc1ss,2, mean)
yexpPIsc1ss=apply(yexpsc1ss,2, PI)

plot(observed1~humidity, data=sc1data, ylim=c(95,110))
lines(sc1data$humidity , yexpmeansc1ss)
shade(yexpPIsc1ss , sc1data$humidity)

# Example 3

sc1rpmodel=alist(
  observed1 ~ dnorm(realsize[id], sigmam[id]),
  realsize[id] ~ dnorm(mu, sigma),
  mu <- alpha + beta*humidity,
  
  sigma ~ dexp(0.01),
  sigmam[id] ~ dexp(1),
  alpha ~ dnorm(0, 100),
  beta ~ dnorm(0,100)
  
)

sc1rpposterior=ulam(sc1rpmodel,
                    data=sc1data,iter = 10000)

precis(sc1rpposterior, depth=2)

yexpsc1rp=link(sc1rpposterior)

yexpmeansc1rp=apply(yexpsc1rp,2, mean)
yexpPIsc1rp=apply(yexpsc1rp,2, PI)

plot(observed1~humidity, data=sc1data,ylim=c(95,110))
lines(sc1data$humidity , yexpmeansc1rp)
shade(yexpPIsc1rp , sc1data$humidity)

# a person asking 10 people on dates repeatedly for 100 times
sec.dates=rep(NULL, 100)
for(i in 1:100){
  sec.dates[i]=sum(rbinom(10,1,0.3))
}
sec.dates


# 100 different people asking 10 people on dates
sec.dates2=rep(NULL, 100)
for(i in 1:100){
  sec.dates2[i]=sum(rbinom(10,1,rbeta(1, 0.3*3,0.7*3)))
}
sec.dates2

# beta dist
plot(density(rbeta(100,2,4)), xlim=c(0,1),ylim=c(0,5))
curve(dbeta(x,2,4),add=T, col="red")

plot(density(rbeta(100,0.4*4,0.6*4)), xlim=c(0,1),ylim=c(0,5))
curve(dbeta(x,0.4*4,0.6*4),add=T, col="red")


#distribution functions in R
rpois(5,20)
dpois(10,20)
dpois(20,20)

#generate song frequency
sf=1000*rbeta(100,10,2)
sf
hist(sf)
sf2=rgamma(100, 40,0.1)
sf2
hist(sf2)
sf3=rlnorm(100, 400,1)
sf3
hist(sf3)

#amount of shampoo
bottle.size=100
sh=bottle.size*rbeta(100, 1,2)
sh
hist(sh)

logistic=function(x){
  1/(1+exp(-x))
}
sh2=logistic(rnorm(100,0,1))
sh2  
hist(sh2)

#the sampler
# temp <- c(...)
# a <- ?
#   b <- ?
#   sigma <- ?
#   mu = a+b*temp
# profit <- rnorm(length(temp),mu,sigma)
# 
# plot(temp, profit)
# 
# d <- data.frame(temp,profit)

temp=d$temp
profit=d$profit

genH=function(i){
  a=rnorm(1,-200,5000)
  b=rnorm(1,10,9.2)
  sigma=rexp(1,1/20)
  lik.per.point=dnorm(profit, mean=a+b*temp, sd=sigma)
  lik=prod(lik.per.point)
  return(c(a=a,b=b,sigma=sigma,lik=lik))
}
H=sapply(1:100000,genH)

ind=sample(1:100000,1000,replace=T,prob = unlist(H[4,]))
ind

post=H[,ind]

hist(post[1,]) #a
hist(post[2,]) #b
hist(post[3,]) #sigma

summary(t(post))

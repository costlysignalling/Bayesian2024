## CLT
# Exercise 1

library(Rlab)
approxnormbern=rep(0,1000)
for (i in 1:1000){
  approxnormbern[i]=mean(rbern(1000, p=0.5))
}

hist(approxnormbern)

# Exercise 2


approxnormpoissum=rep(0,1000)
for (i in 1:1000){
  approxnormpoissum[i]=sum(rpois(100, lambda=150))
}
hist(approxnormpoissum)

approxnormpoisprod=rep(0,1000)
for (i in 1:1000){
  approxnormpoisprod[i]=prod(rpois(100, lambda=150))
}
hist(approxnormpoisprod)
hist(log(approxnormpoisprod))

#log(150*150...)=log(150)+log(150)...
log(150)*100
log(sqrt(150))*100


par(mfrow=c(1,2))
hist(log(approxnormpoisprod))
hist(rnorm(1000,mean = 501,sd = 1))

hist((approxnormpoisprod))
hist(exp(rnorm(1000,mean = 501,sd = 1)))
hist((rlnorm(1000,meanlog = 501,sdlog = 1)))


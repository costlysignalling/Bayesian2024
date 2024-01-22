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

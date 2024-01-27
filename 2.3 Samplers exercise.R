library(rethinking)

y=rpois(5,lambda=20)
y
y=as.double(y)

poism=alist(
  #model (lh)
  y~dpois(lambda),
  #prior
  #lambda~dexp(0.03)
  lambda~dunif(0,100)
)


post=ulam(flist=poism,
          data=list(y=y))

precis(post)
traceplot(post)
plot(post)
epost=extract.samples(post)
hist(epost$lambda)

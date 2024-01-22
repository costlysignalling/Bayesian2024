#This dataset is part of rethinking package, so it can be loaded with function data()
library(rethinking)
set_ulam_cmdstan(FALSE)

data(Kline)
d<-Kline

#Very short but very interesting dataset
d

#Function scale standardizes data for you
d$P <- scale(log(d$population))
d$contact_id <- ifelse(d$contact=="high",2,1)

#Think about reasonable intercept priors
a <- rnorm(1000,0,10)
lambda <- exp(a)
mean(lambda)
plot(density(lambda))

a <- rnorm(1000,3,0.5)
lambda <- exp(a)
mean(lambda)
plot(density(lambda))

#Plot the probability density functions side-by-side
curve(dlnorm(x,0,10), from=0, to=100, n=200)
curve(dlnorm(x,3,0.5), from=0, to=100, n=200,add=T,col=4)

#Looking for a sensible prior for b in the regression, where we link the linear model to the logarithm of average number of tools
N <- 100
a <- rnorm(N,3,0.5)
b <- rnorm(N,0,0.2)

#Predictions around x=0 (median population)
plot(NULL,xlim=c(-2,2),ylim=c(0,100),xlab="scaled log(pop)",ylab="tools")
for (i in 1:N){
  curve(exp(a[i] + b[i]*x), add=TRUE, col="#80808080")
}

#Without scaling
x_seq <- seq(from=log(100), to=log(200000), length.out=100)
lambda <- sapply(x_seq, function(x){exp(a + b*x)})

plot(NULL, xlim=range(x_seq), ylim=c(0,500), xlab="log population", ylab="total tools")
for(i in 1:N){
  lines(x_seq, lambda[i,], col="#80808080", lwd=1.5)
}

plot(NULL, xlim=range(exp(x_seq)), ylim=c(0,1500), xlab="population", ylab="total tools")
for(i in 1:N){
  lines(exp(x_seq), lambda[i,], col="#80808080", lwd=1.5)
}

#TASK 17: Do you think b ~ dnorm(0,1) is sensible? Do you have a better idea? (Rerun the lines 33 onward with that prior.)
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 394

#Fir the intercept only model and model with the effect of population
d.tools <- list(tools=d$total_tools,
            P=d$P,
            cid=d$contact_id)

# intercept only
m.tools.1 <- ulam(
  alist(
    tools ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 0.5)
  ), data=d.tools, chains=4 , cores=4, log_lik=TRUE, sample=T)

save(m.tools.1,file="sampled/m.tools.1.Rdata")
load("sampled/m.tools.1.Rdata")

precis(m.tools.1,depth=2)
exp(3.54) #expected number of tools for median population

# interaction model
m.tools.PC <- ulam(
  alist(
    T ~ dpois( lambda ),
    log(lambda) <- a[cid] + b[cid]*P,
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ), data=d.tools, chains=4, cores=4, log_lik=TRUE, sample=T)

save(m.tools.PC,file="sampled/m.tools.PC.Rdata")
load("sampled/m.tools.PC.Rdata")

precis(m.tools.PC,depth=2)

#Let us use Pareto-Smoothed Importance Sapling to indicate, which observations are potentially very influential (unlikely according to most )
k <- PSIS(m.tools.PC, pointwise=TRUE)$k 
k
#See the last high value? It is Hawaii. When you see something like that, never drop it. Only discuss it. Outlier is not the data's fault, it is model's fault. You will not improve predictions by dropping your data.

plot( d.tools$P, d.tools$tools , xlab="log population (std)", ylab="total tools" ,
      col=rangi2, pch=ifelse( d.tools$cid==1 , 1 , 16 ), lwd=2 ,
      ylim=c(0,75), cex=1+normalize(k) )

#If you prefer WAIC to PSIS, it is possible to use per-observation penalty (log-likelihood variance) to indicate relative influence of each observation.

# set up the horizontal axis values to compute predictions at
ns <- 100
P_seq <- seq(from=-1.4, to=3, length.out=ns)

# predictions for cid=1 (low contact)
lambda <- link(m.tools.PC, data=data.frame(P=P_seq, cid=1))

lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)

lines( P_seq , lmu , lty=2 , lwd=1.5 )
shade( lci , P_seq)

# predictions for cid=2 (high contact)
lambda <- link( m.tools.PC , data=data.frame( P=P_seq , cid=2 ) )
lmu <- apply( lambda , 2 , mean )
lci <- apply( lambda , 2 , PI )
lines( P_seq , lmu , lty=1 , lwd=1.5 )
shade( lci , P_seq, col=col.alpha(rangi2,0.5))

#On the original scale
plot(d$population, d$total_tools, xlab="population", ylab="total tools",
     col=rangi2, pch=ifelse(d.tools$cid==1, 1, 16), lwd=2,
     ylim=c(0,75), cex=1+normalize(k))

ns <- 100
P_seq <- seq( from=-5 , to=3 , length.out=ns )

(slpop<-sd(log(d$population))) # 1.53 is sd of log(population)
(mlpop<-mean(log(d$population))) # 9 is mean of log(population)

pop_seq <- exp(P_seq*slpop + mlpop) 
lambda <- link(m.tools.PC , data=data.frame(P=P_seq , cid=1)) #Prepare predictions for each population size along pop_seq (low contact) 

lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)

lines(pop_seq, lmu, lty=2, lwd=1.5)
shade(lci, pop_seq)

lambda <- link(m.tools.PC , data=data.frame(P=P_seq , cid=2)) #Prepare predictions for each population size along pop_seq (high contact)

lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(pop_seq, lmu, lty=1, lwd=1.5)
shade(lci, pop_seq, col=col.alpha(rangi2,0.5))

#We can make much better model with some clever thinking.
#Just formulate a sensible equation for delta(T) and solve for delta(T)=0

#Notice that there are original values of population
d.tools2 <- list(
  T = d$total_tools,
  P = d$population,
  cid = d$contact_id )

popscale<-function(x,a=2,b){a*x^b}
popscale(10,b=)

curve(popscale(x,a=2,b=0.2),from=0,to=10000)

m.nice <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- a[cid]*P^b[cid]/g,
    a[cid] ~ dexp(1),
    b[cid] ~ dexp(1),
    g ~ dexp(1)
  ), data=d.tools2, chains=4, cores=4,log_lik=T,sample=T)

save(m.nice,file="sampled/m.nice.Rdata")
load("sampled/m.nice.Rdata")

precis(m.nice,depth=2)

#There are also better ways how to include the contact between the islands - use the distances between them
data(Kline2) # load the data, now with coordinates
d <- Kline2
d$society <- 1:nrow(d) # index observations

data(islandsDistMatrix)
round(islandsDistMatrix,2)

#Decomposition of influence/covariance with distance
# linear
curve(exp(-1*x), from=0, to=4, lty=2)
# squared
curve(exp(-1*x^2), add=TRUE)

d.tools.dist <- list(
  T = d$total_tools,
  P = d$population,
  society = d$society,
  Dmat=islandsDistMatrix )

m.distance <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- (a*P^b/g)*exp(k[society]),
    vector[10]:k ~ multi_normal(0, SIGMA),
    matrix[10,10]:SIGMA <- cov_GPL2(Dmat, etasq, rhosq, 0.01),
    c(a,b,g) ~ dexp(1),
    etasq ~ dexp(2),
    rhosq ~ dexp(0.5)
  ), data=d.tools.dist, chains=4, cores=4, iter=2000, log_lik=T, sample=T)

save(m.distance,file="sampled/m.distance.Rdata")
load("sampled/m.distance.Rdata")

post <- extract.samples(m.distance)

# plot the posterior median covariance function
plot(NULL, xlab="distance (thousand km)", ylab="covariance",
    xlim=c(0,10), ylim=c(0,2))

# compute posterior mean covariance
x_seq <- seq(from=0, to=10, length.out=100)
pmcov <- sapply(x_seq , function(x) post$etasq*exp(-post$rhosq*x^2))
pmcov_mu <- apply(pmcov, 2, mean)
lines(x_seq, pmcov_mu, lwd=2)

# plot 50 functions sampled from posterior
for ( i in 1:50 ){
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE ,
         col=col.alpha("black",0.3) )
}


# compute posterior median covariance among societies
K <- matrix(0,nrow=10,ncol=10)
for (i in 1:10){
  for ( j in 1:10 ){
    K[i,j] <- median(post$etasq)*exp(-median(post$rhosq)*islandsDistMatrix[i,j]^2)
    diag(K) <- median(post$etasq)+0.01
  }
}

round(K,2)
image(round(K,2),ylim=c(1.05,-0.05))

# convert to correlation matrix 
Rho <- round(cov2cor(K), 2)

colnames(Rho) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
rownames(Rho) <- colnames(Rho)
Rho

# scale point size to logpop
psize <- d$logpop / max(d$logpop)
psize <- exp(psize*1.5)-2

# plot raw data and labels
plot(d$lon2, d$lat, xlab="longitude", ylab="latitude",
    col=4, cex=psize, pch=16, xlim=c(-50,30))

labels <- as.character(d$culture)
text(d$lon2, d$lat, labels=labels, cex=0.7, pos=c(4))

# overlay lines shaded by Rho
for( i in 1:10 ){
  for ( j in 1:i ){
      lines( c( d$lon2[i],d$lon2[j] ), c(d$lat[i],d$lat[j]) ,
             lwd=2 , col=col.alpha("black",Rho[i,j]^2))
  }
}

PSIS(m.distance,pointwise = T)$k

# compute posterior median relationship, ignoring distance
logpop.seq <- seq(from=6, to=14, length.out=30)
pop.seq<-exp(logpop.seq)

lambda <- sapply(pop.seq, function(pop){(post$a*pop^post$b)/post$g})
lambda.median <- apply(lambda, 2, median)
lambda.PI80 <- apply(lambda, 2, PI, prob=0.8)

# plot raw data and labels
plot( d$population , d$total_tools , col=rangi2 , cex=psize , pch=16 ,
      xlab="population" , ylab="total tools" )
text( d$logpop , d$total_tools , labels=labels , cex=0.7 ,
      pos=c(4,3,4,2,2,1,4,4,4,2) )

# display posterior predictions
lines(pop.seq, lambda.median  , lty=2)
lines(pop.seq, lambda.PI80[1,], lty=2)
lines(pop.seq, lambda.PI80[2,], lty=2)

str(lambda)
for(i in 1:30){
  lines(pop.seq,lambda[i,],col=col.alpha("black",0.2))
}

#TASK 18: Overlay the plot with correlations again (use log population and number of tools instead of longitude and latitude)
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 399

#Another example where distance-dependent covariance table comes handy as a part of the model is analysis that accounts for the relatedness between the data points (e.g. phylogeny)
#Load the data
data(Primates301)
data(Primates301_nex) #This is a phylogenetic tree

#plot it using ape package - install.packages('ape') if needed
library(ape)
plot( ladderize(Primates301_nex) , type="fan" , font=1 , no.margin=TRUE ,
      label.offset=1 , cex=0.5 )

#Arrange into a neat dataset
d <- Primates301
d$name <- as.character(d$name)

dstan <- d[ complete.cases( d$group_size , d$body , d$brain ) , ]

spp_obs <- dstan$name
head(dstan)

#Make a dataset with placeholder diagonal matrix
diag(5)

dat_list <- list(
  N_spp = nrow(dstan),
  M = standardize(log(dstan$body)),
  B = standardize(log(dstan$brain)),
  G = standardize(log(dstan$group_size)),
  Imat = diag(nrow(dstan)) )

m.simple <- ulam(
  alist(
    B ~ multi_normal( mu , SIGMA ),
    mu <- a + bM*M + bG*G,
    matrix[N_spp,N_spp]: SIGMA <- Imat * sigma_sq,
    a ~ normal( 0 , 1 ),
    c(bM,bG) ~ normal( 0 , 0.5 ),
    sigma_sq ~ exponential( 1 )
  ), data=dat_list, chains=4, cores=4, sample=T)

save(m.simple,file="sampled/m.simple.Rdata")
load("sampled/m.simple.Rdata")

precis(m.simple)

#One of the most conservative phyogenetic regression, the one that just assumes Brownian motion - Gaussian random walk - of traits from common "root".
tree_trimmed <- keep.tip(Primates301_nex, spp_obs)
Rbm <- corBrownian(phy=tree_trimmed)
V <- vcv(Rbm)
Dmat <- cophenetic(tree_trimmed)
plot(Dmat, V , xlab="phylogenetic distance" , ylab="covariance" )
str(V)

#put species in right order
dat_list$V <- V[spp_obs,spp_obs]
#convert to correlation matrix
dat_list$R <- dat_list$V/max(V)

#The model
m.Brown <- ulam(
  alist(
    B ~ multi_normal( mu , SIGMA ),
    mu <- a + bM*M + bG*G,
    matrix[N_spp,N_spp]: SIGMA <- R*sigma_sq,
    a ~ normal( 0 , 1 ),
    c(bM,bG) ~ normal( 0 , 0.5 ),
    sigma_sq ~ exponential( 1 )
  ), data=dat_list , chains=4 , cores=4, sample=T)

save(m.Brown,file="sampled/m.Brown.Rdata")
load("sampled/m.Brown.Rdata")

precis(m.Brown)

#Scaled and reordered distance matrix
dat_list$Dmat <- Dmat[spp_obs,spp_obs]/max(Dmat)

#We will employ exponential kernel (not quadratic) whcih assumes quite quick decay of "just due to common ancestry" similarities. In phylogenetic analysis, it is current standard that goes under the name of Ornstein-Uhlenbeck kernel, since it well approximates differentiation under the assumption of damped Browninan motion of Ornstein-Uhlenbeck process (random walk peppered with reversion towards some mean, possibly with the selection towards some stable optimum).
m.OU <- ulam(
  alist(
    B ~ multi_normal( mu , SIGMA ),
    mu <- a + bM*M + bG*G,
    matrix[N_spp,N_spp]: SIGMA <- cov_GPL1(Dmat, etasq, rhosq, 0.01),
    a ~ normal(0,1),
    c(bM,bG) ~ normal(0,0.5),
    etasq ~ half_normal(1,0.25),
    rhosq ~ half_normal(3,0.25)
  ), data=dat_list , chains=4 , cores=4, sample=F)

stancode(m.OU)

save(m.OU,file="sampled/m.OU.Rdata")
load("sampled/m.OU.Rdata")

precis( m.OU )
plot(coeftab(m.OU),pars=c("a","bG","bM"))

#Illustrate how the phylo-distance decay changed between prior and posterior
post <- extract.samples(m.OU)
plot(NULL, xlim=c(0,max(dat_list$Dmat)), ylim=c(0,1.5),
     xlab="phylogenetic distance" , ylab="covariance")

# posterior
for (i in 1:50){
  curve(post$etasq[i]*exp(-post$rhosq[i]*x), add=TRUE, col=rangi2)
}

#Prior mean and 89% Compatibility Interval
eta <- abs(rnorm(100,1,0.25))
rho <- abs(rnorm(100,3,0.25))
d_seq <- seq(from=0,to=1,length.out=50)

K <- sapply(d_seq, function(x){eta*exp(-rho*x)})
lines(d_seq ,colMeans(K), lwd=2)
shade(apply(K,2,PI), d_seq)
text(0.5, 0.5, "prior")
text(0.2, 0.1, "posterior", col=rangi2)

#The model estimates the importance of phylogeny together with all the other model parameters in joint posterior distribution. This model seems to favor very small covariation at all distances over a model, where covariance stars high and decays rapidly (But the later is, what she simple imposed Brownian motion analysis enforces on the data!)


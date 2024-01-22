library(rethinking)

sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")

brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species=sppnames, brain=brainvolcc, mass=masskg)

# Standardize the variable for easier choice of prior
d$mass_std <- (d$mass - mean(d$mass))/sd(d$mass)
d$brain_std <- d$brain/max(d$brain)

#Single parameter model - intercept only. This model says "brain size is independent of body mass"
m0 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a,
    a ~ dnorm( 0.5 , 1 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d )

#Visualize the predictions
mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )

l <- link( m0 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )
for(i in 1:10){
  lines( mass_seq , l[i,] ,col=4)
}

precis(m0)
exp(-1.40)

# Simple model with one linear slope
m1 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b*mass_std,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d )

precis(m1)
exp(-1.71)

# Calculate R squared
set.seed(12)
s <- sim( m1 )
r <- apply(s,2,mean) - d$brain_std
resid_var <- var2(r)
outcome_var <- var2( d$brain_std )
1 - resid_var/outcome_var

# Function that calculates R squared but nevertheless discourages you from doing so
R2_is_bad <- function( quap_fit ) {
  s <- sim( quap_fit , refresh=0 )
  r <- apply(s,2,mean) - d$brain_std
  1 - var2(r)/var2(d$brain_std)
}

R2_is_bad(m0)
R2_is_bad(m1)

# (Just a quick detour, note that extract samples function works also on simple frequentist models, it is designed that way.)
m1_OLS <- lm( brain_std ~ mass_std , data=d )
summary(m1_OLS)
post <- extract.samples( m1_OLS )
str(post)

# Quadratic model
m2 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b1*mass_std + b2*mass_std^2,
    a ~ dnorm( 0.5 , 1 ),
    c(b1,b2) ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,2)) )

#TASK 11: Create polynomial models of order 3 and 4, Visualize the outcomes of the linear and the most complex (fourth order polynomial) model
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 195

#Do you think the more complex model is better?
R2_is_bad(m0)
R2_is_bad(m1)
R2_is_bad(m2)
R2_is_bad(m3)
R2_is_bad(m4)

#Check what go-to frequentist tool thinks
d$mass_std2<-d$mass_std^2
d$mass_std3<-d$mass_std^3
d$mass_std4<-d$mass_std^4

m4_OLS <- lm( d$brain_std ~ d$mass_std + d$mass_std2 + d$mass_std3 + d$mass_std4)
summary(m1_OLS)
summary(m4_OLS)


#On one side from the narrow region of good models that are useful on out-of-sample data is the Charybdis of underfitting (one gaping mouth, terrible vortex), on the other is the Scylla of overfitting (many dangerous heads)
#To find the path between them, you need to know how to likely are your data under each model

#Start with the linear model
set.seed(1) #You can use seeds for pseudo-random number generation to get same set of values in posterior every time
post<-extract.samples(m1)
str(post) 

S<-10000 #How many samples from posterior do we have
N<-7 #How many observations do we have

#TASK 12: Calculate likelihood of 2nd observation in 10th sample in m1 posterior
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 236

#Calculate likelihood of each observation in each sample
lik<-matrix(NA,nrow=S,ncol=N)

for(i in 1:N){
  for(s in 1:S){
    lik[s,i]<-dnorm(d$brain_std[i],post$a[s]+post$b[s]*d$mass_std[i],exp(post$log_sigma[s]))
  }
}

str(lik)

#Take the logarithms of average likelihoods and sum them
lppd<-log(colSums(lik)/S)
lppd #log pointwise predictive density

sum(lppd)

#There already is a function that can do this for you, by the way
set.seed(1)
lppd(m1,n=10000)

#This makes our lives easier
set.seed(15)
sumlppd<-sapply(list(m0,m1,m2,m3,m4),function(m){sum(lppd(m))})
sumlppd

#more complicated models would steer you towards the Scylla of overfitting - they have higher lppd
#Notice that m4 (model with 5 parameters for 7 observations) has bigger differences in how likely different observations are under different models
#beyond mean likelihood: likelihood variance
str(lik)

log.lik.var<-sapply(1:N,function(i){var2(log(lik)[,i])})
sum(log.lik.var)

#TASK 13: Turn the posterior variance calculation into a  function and apply it on all models as lppd above
#HINT: easy but not very fast way to obtain log-likelihood matrix from a model is running function sim() with log_lik parameter set to TRUE like this:
log_lik<-sim(m1,ll=T, n=10000)
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 242

#The best path between Skylla and Charybdis is marked by the sweet spot between low predictive density and high likelihood variance
#I want to be as close to the "best model" as possible, so I aim for low numbers (therefore i subtract predictive density from likelihood variance)
sumlpv-sumlppd
plot(0:4,sumlpv-sumlppd,type="b")

v<-c(0.2,0.3,0.1)*1
var(v)

var2(log(v))


#Double of this difference is called WAIC (Widely Applicable Information Criterion, also known as Watanabe-Akaike IC)
set.seed(15)
sapply(list(m0,m1,m2,m3,m4),WAIC,n=10000)
(sumlpv-sumlppd)*2 #Excuse the sampling variation

#You should know also the wrap-up function compare() that evaluates information criteria (WAIC by default, know also names LOO: Leave One Out Cross-validation and PSIS: Pareto Smoothed Importance Sampling Cross-Validation, which is just fancy equivalent of LOO without leaving one out) calculates model weights based on them and orders models accordingly
set.seed(15)
compare(m0,m1,m2,m3,m4)

set.seed(15)
compare(m0,m1,m2,m3,m4,func=PSIS)

#Model weights are calculated as
WAIC<- -2*sumlppd + 2*sumlpv #Written in the standard (historical reasons) form
(dWAIC<-WAIC-min(WAIC))

w<-exp(-0.5*dWAIC)/sum(exp(-0.5*dWAIC))
round(w,2)

#Compare to softmax function, which is often used in multinomial models (as a generalization of inverse logit)
mysoftmax<-function(x){
  x<-x-min(x) #This line is here only for "computer reasons"
  return(exp(x)/sum(exp(x)))
}

mysoftmax(c(2,3,1))
softmax(c(2,3,1)) #there is softmax() function in rethinking already, but has too much "safe" appendages for easy inspection

inv_logit(3)
mysoftmax(c(3,0))

round(mysoftmax(-WAIC),2)
round(mysoftmax(-WAIC*0.5),2) #Again taking it from WAIC scale to simple lppd-lpv. Statistics is full of frozen random decisions :)

#And you have probably already heard about AIC (Akaike Information Criterion) which is just: -2lppd + 2p, where p is just a number of parameters in the model. We will now demonstrate, why the number of parameters is just not the best idea, because what matters is not the number of parameters but also the conservativeness of the priors.

#Even more parameters and the tragedy of superfitting
m5 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,5)) )

m6 <- quap(
  alist(
    brain_std ~ dnorm( mu , 0.001 ), #The sigma here is fixed, so there is 7 parameters in total
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5 + b[6]*mass_std^6,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 )
  ), data=d , start=list(b=rep(0,6)) )


par(mfrow=c(1,2))
l <- link( m5 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d ,ylim=c(-0.3,1.3))
abline(h=0,lty=2)
lines( mass_seq , mu )
shade( ci , mass_seq )
for(i in 1:10){
  lines( mass_seq , l[i,] ,col=4)
}

l <- link( m6 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d ,ylim=c(-0.3,1.3))
abline(h=0,lty=2)
lines( mass_seq , mu )
shade( ci , mass_seq )
for(i in 1:10){
  lines( mass_seq , l[i,] ,col=4)
}

#Remember: Fitting is easy, prediction is hard

set.seed(15)
compare(m0,m1,m2,m3,m4,m5,m6)
#There is no other option than a perfect fit if you have as many parameters as data points.
precis(m5,depth=2)
precis(m6,depth=2)

#But there indeed are Bayesian models that have even MORE parameters than data points. How is this possible? Easy: The parameters may be grouped and described by hyperparameters or they are regularised in a different way

#TASK 14: Rerun and plot m5 and m6 with very conservative, skeptical priors
#Remember: Extraordinary claims require extraordinary evidence!
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 253

#This was an important step: One that allowed us to trade great in-sample fit for better out-of-sample prediction
precis(m5,depth=2) #When it cannot do whatever it wants, the important parameters shine more
precis(m6,depth=2)
#But beware - wee simple models like m1 might lead to worse out-of-sample predictions when restricted with very tight priors

set.seed(15)
compare(m0,m1,m2,m3,m4,m5,m6)


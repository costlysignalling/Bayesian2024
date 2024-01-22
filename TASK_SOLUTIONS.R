#TASK 1: Visualize the parameter values from the posterior (object post)

plot(NULL,xlim=c(-2,2),ylim=c(3.5,0.5),xlab="parameter value",ylab="",yaxt="n")
axis(2,at=c(1,2,3),labels=c("a","b","sigma"),las=2)
abline(h=1:3)
drawDist(post$a,y=1)
drawDist(post$b,y=2)
drawDist(post$sigma,y=3)
abline(v=seq(-2,2,1),lty=2)

#TASK 2: Draw the prediction line and the 99% Percentile Compatibility interval around it 
x<-seq(-7,8,0.01)
y<-sapply(1:length(post$a),function(i){post$a[i]+post$b[i]*x})
muy<-apply(y,1,mean)
CIy<-apply(y,1,PI,prob=0.99)

myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
lines(x,muy)
shade(CIy,x)

#TASK 3: See the slides

#Task 4: Think about the prior distribution of a in the light of the assumptions it imposes on the data. Try some alternatives and discuss with a friend.
prior_a<-rnorm(10000,0,1)
plot(density(inv_logit(prior_a)),xlim=c(0,1)) #It seems reasonable
abline(v=0.5,col=2,lty=2)

prior_a<-rnorm(1000,-0.5,0.05)
plot(density(inv_logit(prior_a)),xlim=c(0,1)) #This might be too conservative and presumptious
abline(v=0.5,col=2,lty=2)

plot(density(inv_logit(prior_a)),xlim=c(0,1)) #This is so unskeptical on log-odds scale that it assumes extreme options as most likely
abline(v=0.5,col=2,lty=2)

#TASK 5: Calculate p-value of something like "Two tailed test" (Probability of null hypothesis or more extreme hypothesis)
(pRT<-HR/(HR+HL)) #Right tail size (right from null hypothesis)
(pLT<-HL/(HR+HL)) #Left tail size (right from null hypothesis)

min(c(pRT,pLT))*2 #Probability of null hypothesis or more extreme hypothesis

#TASK 6: Create a model where the observations are grouped not by sex, but by each rater.
#Model with the effect of rater
d.rater<-list(
  rater=as.numeric(as.factor(d$rater)),
  chooser=d$chooser)

m.rater<-ulam(alist(
  chooser~dbinom(1,p),
  logit(p)<-a[rater],
  a[rater]~dnorm(0,1)
),data=d.rater,cores=4,chains=4,log_lik=T,sample=T)

save(m.rater,file="sampled/binom_m.rater.Rdata")
load("sampled/binom_m.rater.Rdata")

precis(m.rater,depth=2)

#TASK 7: Fill in this line (if you struggle the solution is in TASK_target_face_vary_solution)
logit(p)<-mean_a[sex] + za[rater]*sd_rater + zt[IDr]*sd_target - zt[IDl]*sd_target #remember to add comma here

#TASK 8: Make models that use differences in (standardized) height, weight, and age instead of rating difference and a full model with difference in ratings and measured characteristics. Compare parameter estimates from the two models and discuss.
#Model with trait differences
m.traits.vd.sex<-ulam(alist(
  chooser ~ dbinom(1,p),
  logit(p) <- a[sex] + VaVb[rater,1] + bH[sex]*diffH + VaVb[rater,2]*diffH + bW[sex]*diffW + VaVb[rater,3]*diffW + bA[sex]*diffA + VaVb[rater,4]*diffA + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  transpars> matrix[rater,4]:VaVb<-compose_noncentered(sd_rater,L_Rho,zr),
  
  a[sex]~dnorm(0,1),
  bH[sex]~dnorm(0,1),
  bW[sex]~dnorm(0,1),
  bA[sex]~dnorm(0,1),
  
  vector[Nt]:zt ~ dnorm(0,1),
  matrix[4,rater]:zr ~ normal(0,1),
  
  vector[4]:sd_rater~dexp(1),
  cholesky_factor_corr[4]:L_Rho ~ lkj_corr_cholesky(2), 
  
  sd_target~dexp(1),
  
  gq> matrix[4,4]:Rho <<- Chol_to_Corr(L_Rho)
),data=d.traits.v.sex,cores=4,chains=4,log_lik=T,sample=T)

save(m.traits.vd.sex,file="sampled/binom_m.traits.vd.sex.Rdata")
load("sampled/binom_m.traits.vd.sex.Rdata")

precis(m.traits.vd.sex,depth=3,pars=c("a","bH","bW","bA","Rho","sd_rater","sd_target"))

#full model
d.full.v.sex<-list(
  Nt=max(c(d$IDl,d$IDr)),
  sex=as.numeric(as.factor(d$sex)),
  rater=as.numeric(as.factor(d$rater)),
  IDl=d$IDl,
  IDr=d$IDr,
  diffR=d$ratingr-d$ratingl,
  diffH=d$heightsr-d$heightsl,
  diffW=d$weightsr-d$weightsl,
  diffA=d$agesr-d$agesl,
  chooser=d$chooser)

m.full.vd.sex<-ulam(alist(
  chooser ~ dbinom(1,p),
  logit(p) <- a[sex] + VaVb[rater,1] + bR[sex]*diffR + VaVb[rater,2]*diffR + bH[sex]*diffH + VaVb[rater,3]*diffH + bW[sex]*diffW + VaVb[rater,4]*diffW + bA[sex]*diffA + VaVb[rater,5]*diffA + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  transpars> matrix[rater,5]:VaVb<-compose_noncentered(sd_rater,L_Rho,zr),
  
  a[sex]~dnorm(0,1),
  bR[sex]~dnorm(0,1),
  bH[sex]~dnorm(0,1),
  bW[sex]~dnorm(0,1),
  bA[sex]~dnorm(0,1),
  
  vector[Nt]:zt ~ dnorm(0,1),
  matrix[5,rater]:zr ~ normal(0,1),
  
  vector[5]:sd_rater~dexp(1),
  cholesky_factor_corr[5]:L_Rho ~ lkj_corr_cholesky(2), 
  
  sd_target~dexp(1),
  
  gq> matrix[5,5]:Rho <<- Chol_to_Corr(L_Rho)
),data=d.full.v.sex,cores=4,chains=4,log_lik=T,sample=T)

save(m.full.vd.sex,file="sampled/binom_m.full.vd.sex.Rdata")
load("sampled/binom_m.full.vd.sex.Rdata")

precis(m.full.vd.sex,depth=3,pars=c("a","bR","bH","bW","bA","Rho","sd_rater","sd_target"))

plot(coeftab(m.traits.vd.sex,m.full.vd.sex),
     pars=c("a[1]","a[2]","bR[1]","bR[2]",
            "bH[1]","bH[2]","bW[1]","bW[2]","bA[1]","bA[2]",
            "sd_target"))

#TASK 9: Parametrize the model better to get narrow posterior distributions right away and not damage the predictive accuracy
#SOLUTION 1: Just left leg
mL <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )

precis(mL)
plot(precis(mL),pars=c("bl"),xlim=c(-2,2))

#SOLUTION 2: Sum of leg lengths as a predictor (average leg as a predictor works the same)
d$SUM<-d$leg_left+d$leg_right

mSUM <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*SUM,
    a ~ dnorm( 10 , 100 ) ,
    b ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )

precis(mSUM)
plot(precis(mSUM),pars=c("b"),xlim=c(-2,2))

#TASK 10: In light of this knowledge think about posteriors from m.traits.vd.sex again. What could you reliably assess despite the wide CIs of model parameters? Visualize it
load("sampled/binom_m.traits.vd.sex.Rdata")
post<-extract.samples(m.traits.vd.sex)
str(post)

str(post)
da<-post$a[,1]-post$a[,2]
dbH<-post$bH[,1]-post$bH[,2]
dbW<-post$bW[,1]-post$bW[,2]
dbA<-post$bA[,1]-post$bA[,2]

#Remember this function?
drawDist<-function(toplot,y,sc=0.1){
  dens<-density(toplot)
  polygon(dens$x,y-dens$y*sc,col=4)
  lines(PI(toplot,prob=0.95),c(y,y),lwd=3)
  points(mean(toplot),y,lwd=2,cex=1.2,bg=0,pch=21)
}

plot(NULL,xlim=c(-1,1),ylim=c(4.5,0.5),xlab="parameter value",ylab="",yaxt="n")
axis(2,at=c(1,2,3,4),labels=c("da","dbH","dbW","dbA"),las=2)
abline(h=1:4)
drawDist(da,y=1)
drawDist(dbH,y=2)
drawDist(dbW,y=3)
drawDist(dbA,y=4)
abline(v=seq(-2,2,1),lty=2)

#TASK 11: Create polynomial models of order 3 and 4, Visualize the outcomes of the linear and the most complex (fourth order polynomial) model
# Polynomials of order 3 and 4
m3 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,3)) )

m4 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,4)) )

# Visualize the outcomes of the linear and the most complex (fourth order polynomial) model
l <- link( m1 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )
for(i in 1:10){
  lines( mass_seq , l[i,] ,col=4)
}

l <- link( m4 , data=list( mass_std=mass_seq ) )
mu <- apply( l , 2 , mean )
ci <- apply( l , 2 , PI )
plot( brain_std ~ mass_std , data=d )
lines( mass_seq , mu )
shade( ci , mass_seq )
for(i in 1:10){
  lines( mass_seq , l[i,] ,col=4)
}

#TASK 12: Calculate likelihood of 2nd observation in 10th sample in m1 posterior
i<-2
s<-10

dnorm(d$brain_std[i],post$a[s]+post$b[s]*d$mass_std[i],exp(post$log_sigma[s]))

dnorm(1,0.53+0.2*0.73,0.2)


#TASK 13: Turn the posterior variance calculation into a  function and apply it on all models as lppd above
lpv<-function(m,n=10000){
  log_lik<-sim(m,ll=T, n=10000)
  
  log.lik.var<-sapply(1:N,function(i){var2(log_lik[,i])})
  return(log.lik.var)
}

set.seed(15)
sumlpv<-sapply(list(m0,m1,m2,m3,m4),function(m){sum(lpv(m))})

#TASK 14: Rerun and plot m5 and m6 with very conservative, skeptical priors
m5 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 0.1 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,5)) )

m6 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ), #Who got this bit right?
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5 + b[6]*mass_std^6,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 0.1 ),
    log_sigma ~ dnorm( 0 , 1 )
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

#TASK 15: Repeat with linear splines. (degree=1) How do the predictions differ?
mu_PI.cubic.15k<-mu_PI #Save this prediction for later

B <- bs(d2$year, knots=knots[-c(1,num_knots)], degree=1, intercept=TRUE )

plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
points(knots,rep(1,length(knots)),col=2,pch=3)
abline(v=knots,col=2,lty=2)
for (i in 1:ncol(B)){
  lines(d2$year , B[,i])
}

m.spline.linear.15k <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

#Extract posterior and plot the results
post <- extract.samples(m.spline.linear.15k)
w <- apply(post$w ,2 ,mean)

plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

mu <- link(m.spline.linear.15k)
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("red",0.5) )
shade( mu_PI.cubic.15k , d2$year , col=col.alpha("black",0.5) ) #At the original, less spiky predictions

#TASK 16: Repeat with Cubic splines from 10 to 30 knots. Which model has the best predicted out-of-sample fit?
#Simple version without a function (shown only for 10 and 20 knots)
num_knots<-10
knots <- quantile(d2$year, probs=seq(0,1,length.out=num_knots)) 
B <- bs(d2$year, knots=knots[-c(1,num_knots)], degree=3, intercept=TRUE)

m.spline.cubic.10k <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

num_knots<-20
knots <- quantile(d2$year, probs=seq(0,1,length.out=num_knots)) 
B <- bs(d2$year, knots=knots[-c(1,num_knots)], degree=3, intercept=TRUE)

m.spline.cubic.20k <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

compare(m.spline.cubic.10k,m.spline.cubic.15k,m.spline.cubic.20k)

#Using a function
splineModel<-function(num_knots){
  knots <- quantile(d2$year, probs=seq(0,1,length.out=num_knots)) 
  B <- bs(d2$year, knots=knots[-c(1,num_knots)], degree=3, intercept=TRUE)
  
  m <- quap(
    alist(
      D ~ dnorm( mu , sigma ) ,
      mu <- a + B %*% w ,
      a ~ dnorm(100,10),
      w ~ dnorm(0,10),
      sigma ~ dexp(1)
    ), data=list( D=d2$doy , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) ) )
  
  #Parser is useful here
  rename<-paste("spitout<-list(cubic.",num_knots,"k=m)",sep="")
  eval(parse(text=rename))
  return(spitout)
}

models<-lapply(10:30,splineModel)

save(models,file="sampled/spline.models.Rdata")
load("sampled/spline.models.Rdata")

#We need to get rid of one list level
models<-unlist(models,recursive=F)
WAICs<-sapply(models,WAIC)
plot(10:30,WAICs[1,])

#TASK 17: Do you think b ~ dnorm(0,1) is sensible? Do you have a better idea? (Rerun the lines 33 onward with that prior.)
#Smaller standard deviation feels better and does not lead to extreme predictions.
#We will go with b ~ dnorm(0,0.2) in the class, but feel free to use your own prior
b <- rnorm(N,0,0.2) #would be the line 33 in this example

#TASK 18: Overlay the plot with correlations again (use lop population and number of tools instead of longitude and latitude)
for( i in 1:10 ){
  for ( j in 1:i ){
    lines( c( d$logpop[i],d$logpop[j] ), c(d$total_tools[i],d$total_tools[j]) ,
           lwd=2 , col=col.alpha("black",Rho[i,j]^2))
  }
}

#this task is omitted from the 2024 course, because we do not deal with the oceanic tool example
#TASK 19: Fill this line with the function that computes expeceted covariance based on parameters of exponential (GPL1) kernel.
matrix[N_spp,N_spp]: SIGMA <- cov_GPL1(Dmat, etasq, rhosq, 0.01) #do not forget to end the line with comma in the model

#TASK 20: Try to think about how would you do that with what you already know and only later read further
#Is the donation predicted by a single trait?
#Is it a single decision?
#Is the situation in any way similar to the monastery where they either drink or work?

#TASK 21: Now that you know beta-binomial distribution, simulate 1000 observations similar to what you see in vector 
n<-1000
donation<-ifelse(rbinom(n,1,0.10),200,rbetabinom(n,size=400,prob=0.15,theta=3))
simplehist(donation)

#TASK 22: Simulate data from the posterior and check whether it works
post<-extract.samples(m.distribution)
(ns<-length(post$ap))

sim.don<-ifelse(
  rbinom(ns,1,inv_logit(post$ap)),200,
  rbetabinom(ns,400,inv_logit(post$amu),post$theta))

hist(sim.don)

#TASK 23: Try to modify the model from code.m.distribution to include linear effects of testosterone and cortisol and their interaction with sex
code.m.TC<-"
data{
    int N;
    int sex[N];
    vector[N] t;
    vector[N] c;
    int donation[N];
}
parameters{
    real ap[2];
    real amu[2];
    
    real bTp[2];
    real bTmu[2];
    
    real bCp[2];
    real bCmu[2];
    
    real<lower=0> theta;
}
model{
    real p;
    real mu;

    ap ~ normal( 0 , 1 );
    amu ~ normal( 0 , 1 );
    
    bTp ~ normal( 0 , 1 );
    bTmu ~ normal( 0 , 1 );
    
    bCp ~ normal( 0 , 1 );
    bCmu ~ normal( 0 , 1 );
    
    theta ~ exponential( 1 );

    for ( i in 1:N ){
        p = ap[sex[i]]+bTp[sex[i]]*t[i]+bCp[sex[i]]*c[i];
        p = inv_logit(p);
    
        mu = amu[sex[i]]+bTmu[sex[i]]*t[i]+bCmu[sex[i]]*c[i];
        mu = inv_logit(mu);
    
        if ( donation[i] == 200 ) target += log_mix(p, 0, beta_binomial_lpmf(200 | 400, mu * theta, (1 - mu) * theta));
        if ( donation[i] != 200 ) target += log1m(p) + beta_binomial_lpmf(donation[i] | 400, mu * theta, (1 - mu) * theta);
    }
}"


#TASK 24: What does this coefficient table suggests at a first sight? Discuss in small groups.
#It seems that females are more generous. Both intercepts - of p and mu - agree on that. The effects of hormones also possibly differ

#TASK 25: wrap the construction of contrafactual predictions into a  function and draw the same plot for men at low cortisol, and both sexes in the other two other levels
predictDon<-function(sex,c){
  logit.p<-sapply(1:ns,function(i){ap[i,sex]+bTp[i,sex]*seqT+bCp[i,sex]*c+bTCp[i,sex]*seqT*c})
  logit.mu<-sapply(1:ns,function(i){amu[i,sex]+bTmu[i,sex]*seqT+bCmu[i,sex]*c+bTCmu[i,sex]*seqT*c})
  pred.p<-inv_logit(logit.p)
  pred.mu<-inv_logit(logit.mu)
  return(pred.p*200+(1-pred.p)*pred.mu*400)
}

predM1<-predictDon(2,scaleC(quantile(d1$cortisol,0.25)))
lines(descaleT(seqT),apply(predM1,1,mean),col=sexcol[2])
shade(apply(predM1,1,PI),descaleT(seqT),col=col.alpha(sexcol[2],0.2))

#Second panel
plot(d1$testosterone,d1$DG_given,type="n",xlab="Testosterone",ylab="donation",main="Cortisol median",xaxs="i",yaxs="i")
abline(h=seq(50,250,50),lty=2,col="grey")

predF2<-predictDon(1,scaleC(quantile(d1$cortisol,0.50)))
lines(descaleT(seqT),apply(predF2,1,mean),col=sexcol[1])
shade(apply(predF2,1,PI),descaleT(seqT),col=col.alpha(sexcol[1],0.2))

predM2<-predictDon(2,scaleC(quantile(d1$cortisol,0.50)))
lines(descaleT(seqT),apply(predM2,1,mean),col=sexcol[2])
shade(apply(predM2,1,PI),descaleT(seqT),col=col.alpha(sexcol[2],0.2))

#third panel
plot(d1$testosterone,d1$DG_given,type="n",xlab="Testosterone",ylab="donation",main="Cortisol high",xaxs="i",yaxs="i")
abline(h=seq(50,250,50),lty=2,col="grey")

predF3<-predictDon(1,scaleC(quantile(d1$cortisol,0.75)))
lines(descaleT(seqT),apply(predF3,1,mean),col=sexcol[1])
shade(apply(predF3,1,PI),descaleT(seqT),col=col.alpha(sexcol[1],0.2))

predM3<-predictDon(2,scaleC(quantile(d1$cortisol,0.75)))
lines(descaleT(seqT),apply(predM3,1,mean),col=sexcol[2])
shade(apply(predM3,1,PI),descaleT(seqT),col=col.alpha(sexcol[2],0.2))

#TASK 26: Experiment with the plotting functions png, and pdf, png does not know parameter compression, pdf does not know compression nor resolution. Do you know why?
#PNG is itself a compression standard, pdf returns curves not bitmaps.
#Equivalents look like this
png("plot.hormones.png",width=21,height=8,units="cm",res=600)
plotAgain()
dev.off()

pdf("plot.hormones.pdf",width=21*0.5,height=8*0.5)
plotAgain()
dev.off()


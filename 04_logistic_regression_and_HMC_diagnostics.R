library(rethinking)
set_ulam_cmdstan(FALSE) #Maybe you do not need to do this

d<-read.table("body.formidability.txt",sep="\t",header=T)
head(d)

#Create the decision variable: whether the body on the right was selected or not
d$chooser<-ifelse(d$IDl==d$choice,FALSE,ifelse(d$IDr==d$choice,TRUE,NA))
sum(is.na(d$chooser))

#Base model
d.base<-list(
  chooser=d$chooser)

m1<-ulam(alist(
  chooser~dbinom(1,p),
  p~dunif(0,1)
),data=d.base,cores=4,chains=4,sample=T)

stancode(m1)


precis(m1)

#Difference model
d.rating<-list(
  chooser=d$chooser,
  diffR=d$ratingr-d$ratingl)

m2<-ulam(alist(
  chooser~dbinom(1,p),
  p<-a+b*diffR,
  a~dunif(0,1),
  b~dnorm(0,1)
),data=d.rating,cores=4,chains=4)

precis(m2)
traceplot(m2)
trankplot(m2)

#Better model parametrization solves this problem! We need to model log(odds) of p, not p directly by a linear model that possibly generates numbers from the -Inf to Inf domain
m3<-ulam(alist(
  chooser~dbinom(1,p),
  logit(p)<-a+b*diffR,
  a~dnorm(0,1),
  b~dnorm(0,1)
),data=d.rating,cores=4,chains=4)

#Save the first three models
save(m1,m2,m3,file="sampled/binom_first3.Rdata")
load("sampled/binom_first3.Rdata") #You can later load the models like this

save("environment.Rdata")
save.image("environment.Rdata")

precis(m3)
traceplot(m3)

#We can plot the comparison of the parameter estimates like this
dev.off()
plot(coeftab(m2,m3))

#Why are they different? They are on different scales!
#The parameter estimates are on the log-odds scale, which is not always the best for intuitive interpretation. It is better to illustrate model's prediction with a plot.
post<-extract.samples(m3)

plot(density(inv_logit(post$a)),xlim=c(0,1))


plot(density(inv_logit(post$a)),xlim=c(0.4,0.6))
abline(v=0.5,col=2,lty=2)

#Task 4: Think about the prior distribution of "a" in the light of the assumptions it imposes on the data. Try some alternatives and discuss with a friend.
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 24 

#Grouping hypotheses into blocks to get a sensible equivalent of p-value (sometimes reviewers ask for that)
H0<-sum(inv_logit(post$a)==0.5)
H0

HR<-sum(inv_logit(post$a)>0.5)
HL<-sum(inv_logit(post$a)<0.5)

HL/length(post$a)
HR/length(post$a)

#TASK 5: Calculate p-value of something like "Two tailed test" (Probability of null hypothesis or more extreme hypothesis)
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 37 

#Prediction about how rating changes the baseline probability
range(d.rating$diffR)
new.diffR<-seq(-4.5,4.5,by=0.1)

#Create a prediction based on each sample from the posterior
pred<-sapply(1:length(post$a),function(i){inv_logit(post$a[i]+post$b[i]*new.diffR)})
str(pred)

plot(NULL,xlim=c(-4.5,4.5),ylim=c(0,1),xlab="Rating difference (R-L)",ylab="Choice probability (R)",yaxs="i")
for(i in 1:50){
  lines(new.diffR,pred[,i])
}
abline(h=c(0.05,0.5,0.95),lty=2,col=4)
abline(v=0,lty=2,col=4)


#Now we can do some interesting stuff
#Model with the effect of sex
table(d$sex,as.numeric(as.factor(d$sex)))

d.sex<-list(
  sex=as.numeric(as.factor(d$sex)),
  chooser=d$chooser)

str(d.sex)

#Observe that the sex variable that we use is index variable (1 or 2), which is much more elegant than indicator variable (0 or 1). The later loads more uncertainty on the category with number 1 (2 uncertain parameters contribute to it's estimation), which is fine if this is what you want (like in case-control study) but you should not do it by default without thinking about it. 
m.sex<-ulam(alist(
  chooser~dbinom(1,p),
  logit(p)<-a[sex],
  a[sex]~dnorm(0,1)
),data=d.sex,cores=4,chains=4,log_lik=T,sample=T)

stancode(m.sex)

save(m.sex,file="sampled/binom_m.sex.Rdata")
load("sampled/binom_m.sex.Rdata")

precis(m.sex)
precis(m.sex,depth=2)

#TASK 6: Create a model where the observations are grouped not by sex, but by each rater.Model with the effect of rater
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 43 

#First multilevel (varying effects) model
m.vary<-ulam(alist(
  chooser~dbinom(1,p),
  logit(p)<-va[rater],
  va[rater]~dnorm(mean_a,sd_rater),
  mean_a~dnorm(0,1),
  sd_rater~dexp(1)
),data=d.rater,cores=4,chains=4,log_lik=T,sample=T)

save(m.vary,file="sampled/binom_m.vary.Rdata")
load("sampled/binom_m.vary.Rdata")

stancode(m.vary)
precis(m.vary)
traceplot(m.vary,pars=c("mean_a","sd_rater"))

#Not so nice

#Remember what is z-score
vector<-rnorm(100,15,6)
mu<-mean(vector)
sigma<-sd2(vector)

vectors<-(vector-mu)/sigma

mean(vector)
sd2(vector)

mean(vectors)
sd2(vectors)

reconstructed<-vectors*sigma+mu
plot(vector,vectors)

head(cbind(vectors,vector,reconstructed))

#Write as decentered model using z-scores of individual raters as model parameters independent of sd_rater and mean_a
m.vary.decent<-ulam(alist(
  chooser~dbinom(1,p),
  logit(p)<-mean_a+za[rater]*sd_rater,
  za[rater]~dnorm(0,1),
  mean_a~dnorm(0,1),
  sd_rater~dexp(1)
),data=d.rater,cores=4,chains=4,log_lik=T,sample=T)

save(m.vary.decent,file="sampled/binom_m.vary.decent.Rdata")
load("sampled/binom_m.vary.decent.Rdata")

precis(m.vary.decent)
traceplot(m.vary.decent,pars=c("mean_a","sd_rater"))

#Varying intercept model with the effect of sex and rater
d.rater.sex<-list(
  sex=as.numeric(as.factor(d$sex)),
  rater=as.numeric(as.factor(d$rater)),
  chooser=d$chooser)

m.rater.sex<-ulam(alist(
  chooser~dbinom(1,p),
  logit(p)<-mean_a[sex]+za[rater]*sd_rater,
  za[rater]~dnorm(0,1),
  mean_a[sex]~dnorm(0,1),
  sd_rater~dexp(1)
),data=d.rater.sex,cores=4,chains=4,log_lik=T,sample=F)

save(m.rater.sex,file="sampled/binom_m.rater.sex.Rdata")
load("sampled/binom_m.rater.sex.Rdata")

precis(m.rater.sex,depth=2,pars=c("mean_a","sd_rater"))
traceplot(m.rater.sex,pars=c("mean_a[1]","mean_a[2]","sd_rater"))
plot(coeftab(m.rater.sex),pars=c("mean_a[1]","mean_a[2]","sd_rater"))

post<-extract.samples(m.rater.sex)
str(post)

plot(NULL,xlim=c(-0.15,0.30),ylim=c(0,2))
abline(h=1)
lines(PI(post$sd_rater,prob=0.99999),y=rep(1,2),lwd=3)
points(mean(post$sd_rater),1,pch=21,lwd=2,bg=0,cex=1.5)
abline(v=0,lty=2)

dens<-density(post$sd_rater)
polygon(c(dens$x[1],dens$x,dens$x[length(dens$x)]),c(0,dens$y,0)*0.1+1,col=4)


#Get there the effect of rated face
index<-as.numeric(as.factor(c(d$IDl,d$IDr)))
d$IDl<-index[1:nrow(d)]
d$IDr<-index[(nrow(d)+1):(2*nrow(d))]

#Check if all faces occur on both sides of the screen
length(levels(as.factor(d$IDl)))
length(levels(as.factor(d$IDr)))
max(c(d$IDl,d$IDr))

d.crossed.effects<-list(
  Nt=max(c(d$IDl,d$IDr)),
  sex=as.numeric(as.factor(d$sex)),
  rater=as.numeric(as.factor(d$rater)),
  IDl=d$IDl,
  IDr=d$IDr,
  chooser=d$chooser)

m.crossed.effects<-ulam(alist(
  chooser~dbinom(1,p),
  #TASK 7: Fill in this line (if you struggle the solution is in TASK_target_face_vary_solution)
  #If you struggle, you can find the solution in TASK_SOLUTIONS.R line 60 
  logit(p)<-mean_a[sex] + za[rater]*sd_rater + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  za[rater]~dnorm(0,1),
  vector[Nt]:zt ~ dnorm(0,1),
  
  mean_a[sex]~dnorm(0,1),
  sd_rater~dexp(1),
  sd_target~dexp(1)
),data=d.crossed.effects,cores=4,chains=4,log_lik=T,sample=T)

save(m.crossed.effects,file="sampled/binom_m.crossed.effects.Rdata")
load("sampled/binom_m.crossed.effects.Rdata")

precis(m.crossed.effects,depth=2,pars=c("mean_a","sd_rater","sd_target"))
plot(coeftab(m.crossed.effects),pars=c("mean_a[1]","mean_a[2]","sd_rater","sd_target"))

#The crossed effects model is the first serious one (it takes into account all pseudo-repetitions in the data)
rm(m.vary)

#Before we get to more serious models that again take ratings into account, we will play around with correlational model on the good old GDP data.
myplot<-function(x,y,y2=NULL,rounding=2,mycol="#2288FF",...){
  namx<-deparse(substitute(x))
  namy<-deparse(substitute(y))
  
  par(mar=c(4,4,4,1),mgp=c(2.2,1,0))
  plot(x,y,col=mycol,pch=16,xlab=namx,ylab=namy,...)
}

GDPd<-read.table("gdppopdata.txt" ,sep="\t",header=T)
GDPd<-GDPd[!is.na(GDPd$population),]

GDPd$population<-log(GDPd$population*1000,2)
GDPd$totalGDP<-log(GDPd$totalGDP*1000000,2)

myplot(GDPd$population,GDPd$totalGDP)
myplot(GDPd$totalGDP,GDPd$population)
myplot(GDPd$population,GDPd$totalGDP)

clean.d<-list(pop=GDPd$population,
              gdp=GDPd$totalGDP)

#We want a Bayesian equivalent of this
cor.test(clean.d$pop,clean.d$gdp)

cor.model<-ulam(alist(
  c(pop,gdp) ~ multi_normal( c(mean_pop,mean_gdp) , Rho , sigmas ),
  mean_pop ~ dnorm(25,10),
  mean_gdp ~ dnorm(40,10),
  
  Rho ~ lkj_corr(2), #See (or just cite) the paper Lewandowski, D., Kurowicka, D., & Joe, H. (2009). Generating random correlation matrices based on vines and extended onion method. Journal of multivariate analysis, 100(9), 1989-2001.
  
  sigmas ~ dexp(1)
),data=clean.d,chains=4,cores=4)

save(cor.model,file="sampled/cor.model.Rdata")
load("sampled/cor.model.Rdata")

precis(cor.model,depth=3,prob=0.95)


#When visualising correlation, use ellipses
library(ellipse)
myplot(GDPd$population,GDPd$totalGDP)

post<-extract.samples(cor.model)
str(post)

matrix(1:4,nrow=2) %*% matrix(5:8,nrow=2)

for(i in 1:100){
  var.cov.mat<-diag(post$sigmas[i,]) %*% post$Rho[i,,] %*% diag(post$sigmas[i,])
  for(l in c(0.5,0.95)){
    lines(ellipse(var.cov.mat,centre=c(post$mean_pop[i],post$mean_gdp[i]),level=l),col="#80808020")
  }  
}

#Or fill them with colour using function polygon() instead of lines()
myplot(GDPd$population,GDPd$totalGDP)
for(i in 1:20){
  var.cov.mat<-diag(post$sigmas[i,]) %*% post$Rho[i,,] %*% diag(post$sigmas[i,])
  for(l in c(0.5,0.95)){
    polygon(ellipse(var.cov.mat,centre=c(post$mean_pop[i],post$mean_gdp[i]),level=l),col="#80808005",border=NA)
  }  
}

#Back to binomial models of target choice
d.rating.vary<-list(
  Nt=max(c(d$IDl,d$IDr)),
  rater=as.numeric(as.factor(d$rater)),
  IDl=d$IDl,
  IDr=d$IDr,
  diffR=d$ratingr-d$ratingl,
  chooser=d$chooser)

m.rating.vary<-ulam(alist(
  chooser ~ dbinom(1,p),
  logit(p) <- va[rater] + vb[rater]*diffR + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  c(va,vb)[rater] ~ multi_normal( c(mean_a,mean_b) , Rho , sd_rater ),
  
  mean_a~dnorm(0,1),
  mean_b~dnorm(0,1),
  
  vector[Nt]:zt ~ dnorm(0,1),
  
  sd_rater~dexp(1),
  Rho ~ lkj_corr(2), 
  
  sd_target~dexp(1)
),data=d.rating.vary,cores=4,chains=4,log_lik=T,sample=F)

save(m.rating.vary,file="sampled/binom_m.rating.vary.Rdata")
load("sampled/binom_m.rating.vary.Rdata")

precis(m.rating.vary,depth=3,pars=c("mean_a","mean_b","Rho","sd_rater","sd_target"))
plot(coeftab(m.rating.vary),pars=c("mean_a","mean_b","Rho[1,2]","sd_rater[1]","sd_rater[2]","sd_target"))

#We are, nevertheless, again unhappy with the convergence criteria. It might be a good idea to build the model as decentred again.
#But how to draw all these parameters and hyperparameters as independent?

m.rating.vary.decent<-ulam(alist(
  chooser ~ dbinom(1,p),
  
  logit(p) <- mean_a + VaVb[rater,1] + mean_b*diffR + VaVb[rater,2]*diffR + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  transpars> matrix[rater,2]:VaVb<-compose_noncentered(sd_rater,L_Rho,zr),
  
  mean_a~dnorm(0,1),
  mean_b~dnorm(0,1),
  
  vector[Nt]:zt ~ dnorm(0,1),
  matrix[2,rater]:zr ~ normal(0,1),
  
  vector[2]:sd_rater~dexp(1),
  cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky(2), 
  
  sd_target~dexp(1),
  
  gq> matrix[2,2]:Rho <<- Chol_to_Corr(L_Rho)
),data=d.rating.vary,cores=4,chains=4,log_lik=T,sample=T)




stancode(m.rating.vary.decent)

save(m.rating.vary.decent,file="sampled/binom_m.rating.vary.decent.Rdata")
load("sampled/binom_m.rating.vary.decent.Rdata")

precis(m.rating.vary.decent,depth=3,pars=c("mean_a","mean_b","Rho","sd_rater","sd_target"))

traceplot(m.rating.vary.decent,pars=c("mean_a","mean_b","Rho[1,2]","sd_rater[1]","sd_rater[2]","sd_target"))

dev.off()
plot(coeftab(m.rating.vary,m.rating.vary.decent),
     pars=c("mean_a","mean_b","Rho[1,2]","sd_rater[1]","sd_rater[2]","sd_target"))

#Let us use the extract prior function to investigate our assumptions about the covariance between varying intercept and slope better (this takes a while)
prior<-extract.prior(m.rating.vary.decent)
str(prior)
Rho12<-prior$Rho[,1,2]
plot(density(Rho12))
#It looks fine, perhaps we can try to tame the correlations a bit more to eta=4 or we can try to run the model for more iterations (e.g. iter=5000)

post<-extract.samples(m.rating.vary.decent)
Rho12p<-post$Rho[,1,2]
plot(density(Rho12p))


pairs<-rmvnorm2(10000,Mu=c(0,0),Rho = matrix(c(1,0.4,0.4,1),nrow=2))
str(pairs)
plot(pairs[,1],pairs[,2])

#Let us return sex of the participant into the model
d.rating.v.sex<-list(
  Nt=max(c(d$IDl,d$IDr)),
  sex=as.numeric(as.factor(d$sex)),
  rater=as.numeric(as.factor(d$rater)),
  IDl=d$IDl,
  IDr=d$IDr,
  diffR=d$ratingr-d$ratingl,
  chooser=d$chooser)

m.rating.vd.sex<-ulam(alist(
  chooser ~ dbinom(1,p),
  logit(p) <- mean_a[sex] + VaVb[rater,1] + mean_b[sex]*diffR + VaVb[rater,2]*diffR + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  transpars> matrix[rater,2]:VaVb<-compose_noncentered(sd_rater,L_Rho,zr),
  
  mean_a[sex]~dnorm(0,1),
  mean_b[sex]~dnorm(0,1),
  
  vector[Nt]:zt ~ dnorm(0,1),
  matrix[2,rater]:zr ~ normal(0,1),
  
  vector[2]:sd_rater~dexp(1),
  cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky(2), 
  
  sd_target~dexp(1),
  
  gq> matrix[2,2]:Rho <<- Chol_to_Corr(L_Rho)
),data=d.rating.v.sex,cores=4,chains=4,log_lik=T,sample=F)

stancode(m.rating.vd.sex)

save(m.rating.vd.sex,file="sampled/binom_m.rating.vd.sex.Rdata")
load("sampled/binom_m.rating.vd.sex.Rdata")

precis(m.rating.vd.sex,depth=3,pars=c("mean_a","mean_b","Rho","sd_rater","sd_target"))


#TASK 8: Make models that use differences in (standardized) height, weight, and age instead of rating difference and a full model with difference in ratings and measured characteristics. Compare parameter estimates from the two models and discuss.
d.traits.v.sex<-list(
  Nt=max(c(d$IDl,d$IDr)),
  sex=as.numeric(as.factor(d$sex)),
  rater=as.numeric(as.factor(d$rater)),
  IDl=d$IDl,
  IDr=d$IDr,
  diffH=d$heightsr-d$heightsl,
  diffW=d$weightsr-d$weightsl,
  diffA=d$agesr-d$agesl,
  chooser=d$chooser)

#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 63 
m.traits.vd.sex<-ulam(alist(
  chooser ~ dbinom(1,p),
  logit(p) <- mean_a[sex] + VaVb[rater,1] + mean_b[sex]*diffR + VaVb[rater,2]*diffR + zt[IDr]*sd_target - zt[IDl]*sd_target,
  
  transpars> matrix[rater,2]:VaVb<-compose_noncentered(sd_rater,L_Rho,zr),
  
  mean_a[sex]~dnorm(0,1),
  mean_b[sex]~dnorm(0,1),
  
  vector[Nt]:zt ~ dnorm(0,1),
  matrix[2,rater]:zr ~ normal(0,1),
  
  vector[2]:sd_rater~dexp(1),
  cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky(2), 
  
  sd_target~dexp(1),
  
  gq> matrix[2,2]:Rho <<- Chol_to_Corr(L_Rho)
),data=d.rating.v.sex,cores=4,chains=4,log_lik=T,sample=F)

#Let me show you something 
#Remember my Rmarkdown example generative process?

N<-1000 # number of individuals

height <- rnorm(N,175,9) # total height of each individual. SD per each group is 9.

leg_prop <- runif(N,0.4,0.5) #Leg length as a proportion of body height
leg_left <- leg_prop*height + rnorm(N, 0, 0.02)
leg_right <- leg_prop*height + rnorm(N, 0, 0.02)

# combine into data frame and imagine that we want to predict body height from leg length
d <- data.frame(height,leg_left,leg_right)

mLR <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d)

#Regression coefficients are very unsure
precis(mLR)
plot(precis(mLR),pars=c("bl","br"))

#posterior parameter estimates are extremely correlated
post <- extract.samples(mLR)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

#If you try to use sampling function ulam instead of analytical quap, you will see some monkey bussiness (you can try this at home, for now, you can load it pre-prepared from)
#save(mLR.ulam,file="mLR.ulam.Rdata")
load("sampled/mLR.ulam.Rdata")
precis(mLR.ulam)
traceplot(mLR.ulam)
trankplot(mLR.ulam)

#But what if we just use the model to predict body height of some new individuals?
N2<-100 # number of new individuals
height2 <- rnorm(N2,175,9) # total height of each individual

leg_prop2 <- runif(N2,0.4,0.5) #Leg length as a proportion of body height
leg_left2 <- leg_prop2*height2 + rnorm(N2, 0, 0.02)
leg_right2 <- leg_prop2*height2 + rnorm(N2, 0, 0.02)

predict<-link(fit=mLR,data=list(leg_left=leg_left2,leg_right=leg_right2))
str(predict)
h.pred<-apply(predict,2,mean)

#Even sets of uncertain parameters can lead to precise predictions
dev.off()
plot(height2,h.pred)

#The posterior of parameter sum is a needle!
plot(density(post$bl+post$br),xlim=c(-2,2))

#TASK 9: Parametrize the model better to get narrow posterior distributions right away and not damage the predictive accuracy
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 138

#TASK 10: In light of this knowledge think about posteriors from m.traits.vd.sex again. What could you reliably assess despite the wide CIs of model parameters? Visualize it
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 167




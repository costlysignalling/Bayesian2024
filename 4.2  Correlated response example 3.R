#First generate the data
library(rethinking)
climate=seq(0.303,0.600,0.003)

a=3
b=-7
mean=logistic(a+b*climate)
mean=(mean*2)-1
mean

sigma=c(1,2)

#A list of rhos changing with a climate
rholist=list()
for (i in 1:100){
  rholist[[i]]=diag(2)*1
  rholist[[i]][1,2]=mean[i]
  rholist[[i]][2,1]=mean[i]
  
}

#Function for creating covariaonce matrix out of sigmas and Rho
cor2cov_1 <- function(R){
  diag(sigma) %*% R %*% diag(sigma)
}

sigmalist=lapply(rholist, cor2cov_1)

#Mean vectors
mu1=10+5*climate
mu2=20+2*climate

#Response variable object
r3=matrix(0,100,2)

for (i in 1:100){
  r3[i,]=rmvnorm(1,c(mu1[i],mu2[i]), sigmalist[[i]])
}
r3
plot(r3[,1],r3[,2])

#Attempt to define Rho matrix in ulam
mvmodel3<-alist(
  #model
  c(r1,r2)~dmvnorm(c(mu1,mu2), Rho, sigmas),
  mu1<-alpha1+beta1*climate,
  mu2<-alpha2+beta2*climate,
  Rho<-matrix(c(1,cor,cor,1),2,2),
  cor<-a+b*climate,
  alpha1~dnorm(0,100),
  beta1~dnorm(0,100),
  alpha2~dnorm(0,100),
  beta2~dnorm(0,100),
  sigmas~dunif(0,100),
  a~dnorm(0,100),
  b~dnorm(0,100)
  
)

mvposterior3=ulam(mvmodel3,
                  data=list(r1=r3[,1],r2=r3[,2], climate=climate), sample = T)

#Does not work, so lets just generate stancode
mvposterior3=ulam(mvmodel3,
                  data=list(r1=r3[,1],r2=r3[,2], climate=climate), sample = F)


stancode(mvposterior3)


#The produced stancode can be tweaked to use one cycle in which rho is dynamically generated
my.code<-"data{
  vector[100] r2;
  vector[100] r1;
  vector[100] climate;
}
parameters{
  real alpha1;
  real beta1;
  real alpha2;
  real beta2;
  vector<lower=0,upper=100>[2] sigmas;
  real a;
  real b;
}
model{
  vector[100] mu1;
  vector[100] mu2;
  matrix[2,2] Rho;
  real cor;
  vector[2] YY;
  vector[2] MU;
  
  b ~ normal( 0 , 100 );
  a ~ normal( 0 , 100 );
  sigmas ~ uniform( 0 , 100 );
  beta2 ~ normal( 0 , 100 );
  alpha2 ~ normal( 0 , 100 );
  beta1 ~ normal( 0 , 100 );
  alpha1 ~ normal( 0 , 100 );
  Rho[1,1]=1;
  Rho[2,2]=1;
  for ( i in 1:100 ) {
    cor = (inv_logit(a + b * climate[i])*2)-1;
    Rho[1,2]=cor;
    Rho[2,1]=cor;
    mu2[i] = alpha2 + beta2 * climate[i];
    mu1[i] = alpha1 + beta1 * climate[i];
    
    MU = [ mu1[i] , mu2[i] ]';
    YY = [ r1[i] , r2[i] ]';
    
    target += multi_normal_lpdf(YY | MU, quad_form_diag(Rho , sigmas));
    }
}"

#Run the model with stan function from rstan
mc=rstan::stan(model_code=my.code,data=list(r1=r3[,1],r2=r3[,2], climate=climate))

plot(mc)
precis(mc, depth=2)
traceplot(mc) #traceplot from rethinking does not work here
rstan::traceplot(mc)
post=rstan::extract(mc)


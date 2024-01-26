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


mc<-rstan::stan(model_code=my.code,data=list(r1=r3[,1],r2=r3[,2], climate=climate))

plot(mc)
precis(mc)
rstan::traceplot(mc)

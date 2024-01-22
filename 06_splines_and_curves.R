#Now we have been through polynomials. Let us see, why splines are better.
library(rethinking)
data(cherry_blossoms) #First cherry blossom timing recorded in Japan for more than 1000 years
d <- cherry_blossoms
precis(d)

d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy (Day Of Year)

plot(d2$year, d2$doy, col=col.alpha(rangi2,0.3), pch=16)

#Define the number of knots
num_knots <- 15
#And distribute them evenly over the data
knots <- quantile(d2$year, probs=seq(0,1,length.out=num_knots)) 

#Use library splines and use it to design basis functions
library(splines)
B <- bs(d2$year,
        knots=knots[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE ) #Basis function values for each observation
str(B)

#Plot what is going on
plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
points(knots,rep(1,length(knots)),col=2,pch=3)
abline(v=knots,col=2,lty=2)
for (i in 1:ncol(B)){
  lines(d2$year , B[,i])
}

#Construct first model with splines, remember that %*% is sign for matrix multiplication - sum of row elements time columns elements
m.spline.cubic.15k <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
  ), data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

#Extract posterior and plot the results
post <- extract.samples(m.spline.cubic.15k)
w <- apply(post$w ,2 ,mean)
w

plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

#Summarize mean estimation from the sample
mu <- link(m.spline.cubic.15k)
mu_PI <- apply(mu,2,PI,0.97)

plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )

#TASK 15: Repeat with linear splines. (degree=1) How do the predictions differ?
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 295

#TASK 16: Repeat with Cubic splines from 10 to 30 knots. Which model has the best predicted out-of-sample fit?
#HINT:
#You may find parser like this useful (or you will need to work a lot more)
my.favourite.number<-11
my.command<-paste("5+",my.favourite.number,sep="")
my.command
eval(parse(text=my.command))

#And this is how you get rid of unnecessary list levels
enveloped<-list(list(a=1),list(b=2))
enveloped
unlist(enveloped,recursive=F)
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 331

#Sometimes it is not bad to use composite graphical functions for fast exploration. There is a whole class of models called "local regressions", (locally estimated scatterplot smoothing) and LOWESS (locally weighted scatterplot smoothing), that are to some extend related to B-splines. These can be accessed through loess() function or within the ggplot infrastructure
load("sampled/spline.models.Rdata")
models<-unlist(models,recursive=F)
WAICs<-sapply(models,WAIC)

library(ggplot2)
ggplot(data.frame(splines=10:30,WAIC=unlist(WAICs[1,])),
       aes(splines, WAIC)) + geom_point() + geom_smooth()

#We could have also done this with the cherry blossom data, but how would we assess the best model complexity?
ggplot(d2, aes(year, doy)) + geom_point() + geom_smooth()


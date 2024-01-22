#Start a new project

######################
### 1. Quick recap ###
######################

#R is basically a better calculator
2+6
3-9
2*9
1/80
6^2

#Calculator that allows you to store arbitrary numbers and texts as objects with names of your choice
box<-30
box
box*6

#The real deal is the combination of objects and functions. You can tell something is a function when it has parentheses. Box is an object, box() would be a function.
#One of the most useful functions is c() c stands for concatenate - it connects multiple atomic objects into a vector (more items in given order)
box2<-c(30,8,6,521)
box2

box2*6

#Allow me to introduce square brackets. They allow you to address elements of larger structures.
box2[2]
box2[c(2,4)]

#You can of course put these in other objects (also if you create an object and put the whole even of creation into parentheses)
(yet.another.object<-box2[c(2,4)])

#Many interesting mathematical operations are present in R as functions
sqrt(yet.another.object) #square root
exp(yet.another.object) #e to the power of the numbers in the vector
log(yet.another.object) #natural logarithm of those numbers
log(yet.another.object,base=2) #logarithm base 2 of those numbers
log(yet.another.object,2) #same thing - if you enter arguments of a function in the right order, you do not even need to specify their names
?log #see the possible arguments, their order and much more


#################################
### 2. Distribution functions ###
#################################

#Today, we will be most interested in functions that have to do something with probability distributions, specifically with their probability density function or their probability mass function. See some of these mathematical expressions for yourself on the handout. It is generally the same thing - a function that needs some parameter values of a distribution and either a value x for which the probability density should be returned or how much numbers you want to generate from that probability distribution. The term "probability density" is used for continuous distributions (numbers with decimals allowed) the term "probability mass" is used when the distribution allows only discrete counts (such as the number of "heads" out of n coin tosses).

#It is useful to picture a function as a crisps factory - potatoes, salt and other optional ingredients go in and crisps come out. Functions that are used to generate numbers from distributions are no exceptions. Let us start with normal distribution that you are all probably familiar with:

rnorm(n=15,mean=10,sd=6)

#or 
bag<-rnorm(15,10,6) #you want 15 numbers from a distribution characterized by mean 10 and sd 6

#all number-generating functions in R have similar names - they start with "r" (i guess it is R for geneRate, or Random, or just R-trademark letter)

#I will show you just a few more distributions from the "exponential family" that you will likely encounter most often. A simplest distribution in this family is called exponential. It is a continuous distribution that has just one parameter. So by using it, you do not assume much about the number - just that it is positive.

#Behind every distribution, you need to imagine a data-generating process. For exponential distribution that is a process of "events occurring at a stable rate". Earthquakes, fish caught (if you use multiple rods at once, so you are not limited by fish-processing time) papers by experienced scientists that always have multiple articles in the pipeline - are all such events and are not very much influenced by previous occurrences of the same sort. If you are fish-less for an hour, it does not elevate the chance that you will catch a fish in the next minute. So the distribution must be scale-less and self-similar. The relative decrease of probability density between two equally spaced points must also be equal. See the image - this is exactly true for exponential distribution; a distribution that then describes intervals between two catches, two earthquakes, two papers...
rexp(20,rate=0.5) #Rate here basically describes how many events you expect per given unit (of time), so 0.5 stands for e.g. "one fish every two hours"

v<-rexp(1000,rate=0.5)
mean(v) #it fits the formulas form mean and sd on the paper, see?
sd(v)

#If this is paper waiting times, how much time does it take to finish PhD (let us equal this with 4 published papers - we accept the simplification that you start the preparation even before you enlist and they give you the title right after you publish fourth paper)
sum(rexp(4,rate=1.2)) #one PhD time
v<-replicate(1000,sum(rexp(4,rate=1.2))) #the same thing replicated 1000 times

plot(density(v)) #Do you recognize this density? Check the paper if you do not.

#It is gamma distribution
v<-rgamma(1000,shape=4,rate=1.2)
plot(density(v))

#this is the generative process behind gamma - sum of a few numbers from the exponential distribution(s) If you encounter a variable like this, it should be gamma distributed.

#What happens if we increase the "number of events" or the "shape" parameter in this process by a lot:
v<-replicate(1000,sum(rexp(400,rate=1.2))) 
plot(density(v))
#We basically see a familiar bell of normal distribution. That is because normal distribution is precisely expected to result from any additive process - regardless of the distribution from which the small additions (or subtractions) are drawn. It does not matter whether it is from exponential distribution, other normal distribution or just +1 -1, or +1 0 decided by a coinflip

#This is a simulated (unfair, 60% tails) coin
five.hundred.flips<-sample(c(-1,1),size=500,replace=TRUE,prob=c(0.6,0.4))
sum(five.hundred.flips)

v<-replicate(1000,sum(sample(c(-1,1),size=500,replace=TRUE,prob=c(0.6,0.4))))
plot(density(v)) #see?

#or
v<-replicate(1000,sum(sample(c(0,1),size=500,replace=TRUE,prob=c(0.6,0.4))))
plot(density(v)) #see?

#This simple sampling of two alternatives has an analytic form of binomial distribution
rbinom(20,size=500,prob=0.4) #Generates 20 independent sums of 500 coinflips, prob of heads is 0.4 - these are the two parameters of binomial distribution. Maximum number (minimum is theoretically 0) and probability of +1 (heads in this coinflip situation)

plot(density(rbinom(1000,size=500,prob=0.4))) #Effectively the same thing as above. You can see that is again very much normal, since the theoretical minimum and maximum are both very far from the expected mean.

#This gives you the sense of the discrete nature of binomial distribution
hist(rbinom(1000,size=6,prob=0.4))

#binomial distribution is the first distribution encountered in this workshop that allows only discrete integer outcomes. Another very popular discrete number disribution from the same family is Poisson. It is, again, defined through the constant-rate data-generating process of the exponential distribution.
#If these are the intervals between subsequent papers
intervals<-rexp(1000,rate=1.2)
cumsum(intervals) #Then this is the total time from the beginning when the papers are published
(which.y<-cut(cumsum(intervals),breaks=seq(1,sum(intervals),by=1))) #It takes the cut function to detect between which and which year of your life each paper was published
#And by summarizing the which.year vector, you will obtain the number of papers each year (watch out for setting the maxsum parameter of summery to something large, so you are not limited by the amount of most frequent data for which the result is outputted)
(v<-summary(which.y,maxsum=100000))
hist(v) #This distribution together with its mean and sd is implied by the rate parameter of exponential distribution. It is the same parameter lambda that describes it. (Unfortunately the authors of R used different label for it in the rexp distribution, which obscured this simple fact, see that i use the same Greek letter in both distributions on the handout) 
#It has obviously short convenient form with the function rpois()
v<-rpois(1000,lambda=1.2)
hist(v)

#there is also a way, how to get to Poisson distribution from binomial; all it takes is to get a big size (a lot of attempts) and very low rate, so the upper limit does not influence the shape of the distribution. We can picture it then as each second of an hour being an "attempt" in which fish can be caught.

v<-rbinom(1000,size=3600,p=0.00033)
hist(v) #All nice and sound

#One can get also to a bell-ish normal-like curve, but since Poisson distribution has only one parameter, it does not allow to independently modify mean and sd - both are determined by the rate.

v<-rpois(1000,lambda=100)
hist(v)
mean(v)
sd(v) #If you need more variance (overdispersion) check the gamma-Poisson distribution

#It takes the additive process - one that can get you in principle both positive and negative - to get to normal distribution. This is the core of central limit theorem. But the possibility of a negative outcome is not necessary, because normal distribution is the correct choice in many other scenarios. Of of them is the average rating by many raters, or score of a multi-item questionnaire, since they, too, occur from many acts of addition, important is also the question of symmetry - whether floor and ceiling of the possible values play similarly unimportant role.

rbinom(30,size=6,p=0.5)+1 #30 items, Likert scale from 1 to 7

v<-replicate(1000,mean(rbinom(30,size=6,p=0.5)+1))#summed and repeated 1000 times
plot(density(v))

#usually there will be more variation between individuals, since each participant "flips a different coin". The distribution of coin biases can be for instance described by an underlying beta distribution - the only distribution on the paper that is continuous and has both upper and lower limit (0-1, but it can be scaled to this unit from arbitrary interval by subtracting the min(x) and dividing by (max(x)-min(x))).

alpha<-3
beta<-4
v<-rbeta(1000,shape1=alpha,shape2=beta)
plot(density(v))

#In the simplest form, beta distribution is characterized by two "counteracting" parameters alpha and beta. Each parameter - both have to be larger than 0 - can be imagined as a dwarf sitting in either 0 or 1 and luring the probability density to itself (when <1) or pushing it away (when >1). In practice this is handy if you model some ratio between 0 and 1 as a result of two counteracting forces (for instance how much space in the fridge occupies one of two roommates), but for most cases, reparametrization with the beta distribution mean (between 0 and 1) and a shape parameter (usually labelled theta) that describes the degree of probability density concentration works better.

mu<-0.75
theta<-20
alpha<-mu*theta
beta<-(1-mu)*theta
v<-rbeta(1000,shape1=alpha,shape2=beta)
plot(density(v))

# So perhaps the distribution of means can arise like this:
mu<-0.5
theta<-3
alpha<-mu*theta
beta<-(1-mu)*theta
v<-replicate(1000,mean(rbinom(30,size=6,p=rbeta(1000,shape1=alpha,shape2=beta))+1))
plot(density(v))

# Beta and binomial distributions combine so nicely that they have a ready-made combined form called beta-binomial distribution (see the handout). Such distributions are sometimes called mixture distributions and you can create tons of them.

###################################
### 3. Basic exponential curves ###
###################################

#But back to the normal distribution that emerges everywhere where addition is in the core of the generating process (regardless of from which distribution the original numbers come from). It is best to be characterized just by mean and standard deviation. Look at the probability density function. It looks a bit horrible - with all these fractions, square roots and constants, but all it is, is this simple shape in disguise:

curve(exp(-x^2),from=-3,to=3) #notice the useful curve() function
#Everything else in that function is just to move the center along the x axis and to make the distribution wider/narrower while keeping the area under the curve =1

#As you may notice it is very related to the basic form of probability distribution function of exponential distribution, which just does not square the x in the exponent.
curve(exp(-x),from=-3,to=3)

#This basic functions are also the reason while the distributions we talk about and which are presented on the handout are called "exponential family distribution".

#################################################################
### 4. Additive vs multiplicative, real-number domain mapping ###
#################################################################

#What happens when we replace the addition with multiplication; sum with product and a "small contribution" like +1 -1 with increase by 10% (*1.1) or decrease by 9.1% (*1/1.1)
v<-replicate(1000,prod(sample(c(1/1.1,1.1),size=500,replace=TRUE)))
plot(density(v)) #We get a right-skewed distribution

#Try to find it on the handout.
#It is the log-normal distribution; a distribution of numbers whose logarithms are normally distributed.
plot(density(log(v)))

#It is no surprise. Logarithms - together with their inverse exponential function - were designed to translate between additive and multiplicative domain. If our ancestors needed to multiply 3 very large numbers, it was easier for them to covert them to their logarithms (with the help of painstakingly created and distributed logarithm tables and rulers) sum those logs and exponentiate the result.
3354521*856*78991

(sumlog<-log(3354521)+log(856)+log(78991))
exp(sumlog)

#Sometimes this is retold as the wisdom that "logarithm of a product is a sum of logarithms"
log(653*984,base=2)
log(653,base=2)+log(984,base=2) #(true for any base)

#Logarithm and exponential function can be therefore used as neat mapping functions between domain of all real numbers (-Inf,Inf) and positive real numbers. If you need to do a linear regression model where there is a lot of additions, so theoretically, the outcome can be negative, but the modeled parameter is strictly positive (for instance the mean of Poisson distribution) just map the result of the regression to (0,Inf) with exponential function. (Or tell your model - if possible - that what the regression terms add up to is logarithm of the parameter; see the batman mapping in handout. To map between -Inf,Inf and 0,1 domain (useful for instance in mapping linear regression to the p parameter of binomial distribution), you can use logistic/logit mapping).
logit<-function(p){log(p/(1-p))}
inv_logit<-function(x){exp(x)/(exp(x)+1)}

logit(0.99)
inv_logit(-4)
logit(inv_logit(16))

#Mathematicians frequently used natural logarithm (base e=2.7....) or base-10 logarithm if they lean into physics, yet there are better logarithms if you do not need to execute simple mathematical operations but to visualize data and understand relationships between them (see the handout).

#Technically log-normal and not normal distribution is the maximum entropy distribution if you only know that the number you need to draw is definitely positive, there is no strict upper limit to it and its standard deviation may not be equal to the mean (so it is not the exponential distribution).
#It is parametrized by mean natural logarithm (m in the handout - it is also equal to logarithm of median, so this one is not too scary) and standard deviation of natural logarithm (s - this one is more tricky) which may be sometimes difficult to imagine right when you want to generate some data. But again, using some basic arithmetic you can get formulas for these "native" parameters using the parameters that you understand better. For instance you can find log-normal distribution with arithmetic mean (\mu) 175 and standard deviation (\sigma) equal to 8. (Similar to human height, which on one hand is very much influenced by additive genetic variance, yet it is strictly positive number)

#TASK
#Use the formulas on the handout (the other formula for s uses logarithm median instead of mean) to find log-normal distribution with.
mu<-175
sigma<-8


#Solution
m<-log((mu^2)/sqrt(mu^2+sigma^2))
s<-sqrt(log(1+(sigma^2)/(mu^2)))
m
s

v<-rlnorm(1000,meanlog=m,sdlog=s)
plot(density(v))
#You can see that for median/mean so far from 0, and with small sd, log-normal distribution resembles normal distribution a lot. So it is not a huge problem to use normal distribution instead of log-normal and no transformation is needed - that is why so many people get away with using not "the most appropriate" distributions.

######################
### 5. Faking data ###
######################

#So I think that the core message of this workshop becomes pretty clear: To do statistics reliably, you must be able to fake your data - to generate "how the data would look like if my favorite hypothesis was true" because that forces you to think in terms of distributions, relationships between them and the whole data-generating process behind your observations. It will become obvious that each hypothesis is just a set of parameters and a way of how they are connected to let the data at hand arise. Because we never observe hypotheses - we always observe only data - we must assess the probability of hypotheses with a calculation of conditional probability (see the end of the workshop).

#It was quite hard to find two variables that are really expected to fulfill all criteria for being best modeled by normal distribution. The biggest challenge was to find variables that need support on both negative and positive part of the real axis. But I succeeded! I will simulate a relationship between temperature (that can be both positive and negative in Celsius - now hush about absolute 0, because that is unachievable anyway) and a profit of an ice-cream company (which can also be negative if the company looses some money daily - which is also imaginable if the company is stupid enough to be sending ice-cream dealers to the streets when it is freezing out there).

#So first, I need some temperatures, I will generate 52 observations (let us say one observation each week, so I expect to get both positive and negative daily temperatures throughout the year)
n<-52
temp<-rnorm(n,mean=8,sd=10) #Let us say mean temperature is about 8 and sd about 10
temp #some hot days, some freezing days

#When I want to simulate ice-cream company profits that depend on the temperature, I need to define the linear regression that is in the core of the profit-generating process.
#The expected profit can be defined in terms of two numbers - how much the company loses when it is 0 degree Celsius outside (we can call this number "intercept" - and in the "the point in which the line describing the relationship between temperature and profit intercepts the y axis at x=0" or "a" for short) and how much this number changes with each increase of one degree (this number is called "slope" or "b" for short)
a<--1500 #Let us say, that a stupid ice-cream company looses 1000 dollars a day, if it is 0 °C.
b<-1500/12 #we assume that at 12 °C ice-cream companies start making money, so we can define the slope by this intuition

#So the expected profit (the mean or \mu, that would be the average profit if we had many days like this) for each generated temperature is 
mu<-a+b*temp
mu

#And the last parameter of the system will be called sigma (or a standard deviation of the residuals). Because the profit is not deterministic, there is still some variation above the regression line:
sigma<-200 #we set it to 200 dollars

#we will use the mu and sigma to generate the final observations of profit (if you provide the vector of means of the same length as the requested number of generated values, each observation is generated with the same sigma but different mean)
profit<-rnorm(n,mean=mu,sd=sigma)

#we can plot the relationship
plot(temp, profit) #Pretty nice relationship, right?


#Exercise: Come up with an interesting problem/system with about 2-6 parameters, simulate it in the same way. If you cannot come up with anything, simulate one of these relationships (assigned during the workshop), use the "Maximum entropy inspired flowchart", "Data distribution illustrated with fish", or any other wisdom found on the handout.

#Easy
#Aliens1: dependent: number of teeth of an alien, independent: weight of an alien
#Aliens2: dependent: weight of an alien, independent: number of teeth of an alien
#Shamans: dependent: number of religious specialists, independent: group size
#IQ in the guts: dependent: IQ, independent: microbiome (three different types)
#Beer and science: dependent: number of publications per year, independent: yearly beer consumption (liters)
#Generosity: dependent: sustenance donated to a food bank (each participant receives 8 kg of dry rice at the beginning of the experiment) dependent: gender (introduce as many as you like) 

#Medium
#Butterflies: dependent: UV reflectance (as the proportion of wing that reflects UV), independent:  latitude and elevation.
#Plants: dependent: plant height after 3 weeks, independent: 2 types of fertilizer and the amount of fertilizer vs non-fertilized control
#1 of Big 5: dependent: Extraversion, independent: sex (male, female), hormonal contraception usage in females
#Beards: dependent: whether or not a person grows a beard, independent: profession (barista, fireperson, professor), sex, age
#Imprinting: dependent: eye color of the partner (same as parent / different from parent), independent: eye color of the parent (frequencies: black: 0.1, brown: 0.4, green: 0.2, blue: 0.3, note that frequencies in the pool of partners is expected to be the same)
#Conformity: dependent: conformity (score based on yes/no questionnaire of 20 questions) independent: least effort distance of birthplace from the nearest origin of cereal agriculture 

#Hard
#Coats: dependent: How do you like the coat on a scale from 1 to 7, independent: color of the coat (8 different colors), time it took for the seamstress to make the coat, group of raters (fashion professional, layman, blind person)
#Pragmatic altruism: dependent: Number of coins offered in the ultimatum game from the budget of 100 tokens (only discrete counts and there is an inflation above the middle that represents fair share), independent: testosterone level (nmol/litre), sex
#Tools: dependent: number of distinct tool-types in archaeological record, dependent: island (4 different islands, each measured in three well-documented consecutive layers), presence of long-distance trade (assessed based on other signs from the archaeological record)
#Gods: dependent: recorded presence of a moralizing god, independent: usage of a script, number of hierarchical levels in the society (1,2,3), European influence (none, some, heavy)
#Confrontation: dependent: who won, independent: weight of both fighters (perhaps quadratic), height of both fighters (linear)
#Press control: dependent: proportion of a page that is text (vs images), independent: The Economist Democracy Index of publishers country, cost of color ink in given country, television consumption (hours) of an average person from the country, average education (years)

###############################
### 6. Dumbest sampler ever ###
###############################

#What is the most straightforward way, how to get the parameter values - or likelihood of the hypotheses - if you see only the data?
#First we must state, that we are pretty clear about what we mean by hypothesis: Hypothesis is a set of parameter values. So in this context -1500, 125, 200 is the correct hypothesis of how do profits depend on temperature. We know it, because we used these values of a, b, and sigma to generate our data. But let us pretend for a moment that we do not know the truth. That we just have the dataset
d<-data.frame(temp=round(temp,1),profit=round(profit,2)) #We include rounding because why not... It would appear in empirical data anyway
head(d) #Looks pretty plausible, right?

#So let s pretend that you do not know the correct hypothesis, but you want to extract the distribution that will tell you, which hypotheses are likely.
#You can achieve this by a very simple computer simulation. You will simulate a million random hypotheses. You will evaluate the data likelihood for each. And then you will randomly draw those hypotheses with probability proportional to the likelihood of data if they were correct. Simple right?

temp #We have temperature fixed as independent variable
#And let us recap the code that generated our data
a<--1500
b<-125
sigma<-200
mu<-a+b*temp
profit<-rnorm(n,mean=mu,sd=sigma)

#The code of the dumbest sampler ever is almost identical to this one. Only it does not utilize the implementation of probability density (or mass) function to generate data, but to tell you, how likely your data are. It just uses dnrom() instead of rnorm() (see the last column o the distribution page of the handout).
#And, of course, we replace the numbers with random parameter generating process. We pretend that we do not know anything about the data - just that profits are approximately in thousands of dollars. (You usually know at least this much about your data.) So we will draw "a" somewhere between -2000 and 2000 and "b" between -200 and 200 (but we will not set these as strict limits, see the Maximum entropy inspired flowchart for generating random numbers to come up with nice hypothesis generating machine)
a<-rnorm(1,0,1000)
b<-rnorm(1,0,100)
sigma<-rexp(1,1/500)
mu<-a+b*temp
profit.lik<-dnorm(profit,mean=mu,sd=sigma) #see how similar it is?

#So now we have likelihood of each observation under hypothesis
c(a,b,sigma)

#What shall we do with it to get the total dataset likelihood? Think about the likelihood of two "heads" out of two, if you flip an unfair coin with heads probability =0.4
(lik<-prod(profit.lik)) #sure, we will just take the product of all likelihoods
#Has someone something else than 0? It is really a very stupid sampler where the rounding error matters. But do not worry. We will do the same process a million times!

#Let us start with just 100 hypotheses (sapply is a wrap-up for any function in the second argument. It returns the total output as a matrix, lapply would return a list.)
sapply(1:100,function(i){
  a<-rnorm(1,0,1000)
  b<-rnorm(1,0,100)
  sigma<-rexp(1,1/500)
  mu<-a+b*temp
  profit.lik<-dnorm(profit,mean=mu,sd=sigma)
  return(c(a=a,b=b,sigma=sigma,lik=prod(profit.lik)))
})

#It works fine. And some hypotheses have >0 likelihood to produce the data at hand
#Lets scale it up to 1 000 000 hypotheses and save it as an object (if your computer is slow, 100 000 is fine)
H<-sapply(1:1000000,function(i){
  a<-rnorm(1,0,1000)
  b<-rnorm(1,0,100)
  sigma<-rexp(1,1/500)
  mu<-a+b*temp
  profit.lik<-dnorm(profit,mean=mu,sd=sigma)
  return(c(a=a,b=b,sigma=sigma,lik=prod(profit.lik)))
})

#So that was the first lottery. Now for the second. Now we draw 1000 of these hypotheses (columns of the table) with probability proportional to the "lik" row
winners<-sample(1:1000000,1000,replace=T,prob=H["lik",])
post<-H[,winners]
#This is my posterior distribution. Have you heard the term? We have just essentially applied the dumbest sampler based on Bayes theorem ever! The first lottery - the one that forced you to reflect what hypotheses does it even make sense to include - is called prior. If you had some prior knowledge about the relationship between the temperature and profit, you colud have reflected it there (e.g. by drawing b from rnorm(1,100,100) instead of rnorm(1,0,100)).

#Now we can easily summarize the posterior by marginal projections of individual parameter values
plot(density(post["a",]))
plot(density(post["b",]))
plot(density(post["sigma",]))

#Or by plotting them in pairs (do not forget to transpose to columns)
pairs(t(post[1:3,]))

#It is a bit rugged, but - the overall estimates are reliable
apply(post[1:3,],1,mean)
apply(post[1:3,],1,quantile)

#Exercise: Use the data simulated within the first exercise and try to extract the original parameter values (joint posterior distribution of them)

#####################
### 7. Growing up ###
#####################

#Using more clever samplers (Hamiltonian Monte Carlo addressed through the Stan infrastructure and perhaps by rethinking package) is just growing up. You do not need to code them from scratch, but now you understand what they are essentially doing.
library(rethinking)

#see how this sampler is coded from top down much like equations on a table may be (but the translated stan code is, again, from top down)
model<-ulam(alist(
  profit ~ dnorm(a+b*temp,sigma),
  a ~ dnorm(0,1000),
  b ~ dnorm(0,100),
  sigma ~ dexp(0.002)
),data=d,chains=4,cores=4)

precis(model) #It is almost identical to the stupidest sampler
plot(model)

#extracting samples is also easy
post<-extract.samples(model)

#Why are samplers so useful?
#There was no need to divide anything by a large sum. The big divisor in the denominator of Bayes theorem is the same for all hypotheses.

#All sampling techniques use this awesome fact and they just output a random subset of a continuum of plausible hypothesis.

#Our sampler explicitly divides the raffle into two steps to bring closer the idea of hypothesis prior*data likelihood product. It is obviously much more effective to do it all in a single step. Stupidest sampler still generates a lot of junk per one random hypothesis that ends up in the final set of samples. Most samplers are much more effective.

#optional exercise: Use ulam if your problem was too complex to be analyzed by the stupidest sampler ever.

##########################
### 8. Another example ###
##########################

#Here is one more example with data generation and dumbest sampler ever
#If there is too much time during the workshop, we can jump back and forth to it

#relationship between testosterone level (nmol/l) and size of defended territory in two species of birds
n=40
spec<-rep(1:2,each=20) #40 birds, 20 each species

test<-rlnorm(n,log(0.8),s=0.4)#in nmol/l hormonal levels are notoriously right skewed
ltest<-log(test,1.1)-log(median(test),1.1) #It will be useful to convert the testosterone to logarithm base 1.1
#The same is true for defended areas (also normally distributed) - so me model directly those. And for now we assume that each species has a different slope of how log area changes
a<-c(20,50) #two medians
b<-c(1.8,2.2) #two slopes of logarithm base 1.1 increase by increasing testosterone by 10 percent
#Since you want the defended ares of individuals with the same testosterone level to differ maximally by 50%
log(1.5,1.1)
1.1^4.254
#you set sd of 1.1-base logarithm maximally around 2, so +-2SD should be about 4, so
s<-c(1.5,2)

larea<-rnorm(n,ltest*b[spec],s[spec]) #This inclusion of qualitative variable is called "index" and is very intuitive (another useful types are "indicator" and "contrast", see the handout)
area<-a[spec]*1.1^larea #Let us say it is in m^2

plot(test,area,col=spec)

#create the dataset to test, whether we are able to extract the right values
d<-data.frame(test=round(test,2),spec=spec,area=round(area,2))
head(d)

#first we transform the data
dt<-d
dt$test<-log(d$test,1.1)-log(median(d$test),1.1)
dt$area<-log(d$area,1.1) #I do not subtract log median from here, because differences between species in median area are also of interest

#Notice that we generate 2 values per parameter (6 parameters in total)
H<-sapply(1:1000000,function(i){
  a<-rlnorm(2,log(30),1)
  b<-rnorm(2,0,1)
  s<-rexp(2,1)
  lik<-dnorm(dt$area,mean=log(a[dt$spec],1.1)+b[dt$spec]*dt$test,sd=s[dt$spec])
  return(c(a=a,b=b,s=s,lik=prod(lik)))
})

winners<-sample(1:1000000,1000,replace=T,prob=H["lik",])
post<-H[,winners] #and we have posterior
post<-as.data.frame(t(post)) #it is better to convert it to data frame for easier manipulation

summary(post)
# Values we used were
c(20, 50, 1.8, 2.2, 1.5, 2)

#Adding prediction lines to the  plot
ntest<-seq(0,5,by=0.01) #create values of testosterone for which the predictions shall be made
sntest<-log(ntest,1.1)-log(median(test),1.1) #It enters the model on log scale and 
for(i in 1:100){
  lines(ntest,post$a1[i]*1.1^(post$b1[i]*sntest)) #But it is plotted against the non-transformed testosterone levels
  lines(ntest,post$a2[i]*1.1^(post$b2[i]*sntest),col=2)
}

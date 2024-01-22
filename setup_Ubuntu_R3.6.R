#We will start with instructions from https://github.com/rmcelreath/rethinking
#Just follow them and you will succeed. If you do not want to zigzag between rethinking, rstan and related manuals, just follow this script that includes all necessary steps.

#First rethinking directs you to https://mc-stan.org/users/interfaces/rstan.html
#Which redirects you to its own github https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started 

#UBUNTU contains C compiler and many other useful suff already, so the installation should be easier

#First: Install rstan
install.packages("StanHeaders") # cran version works just perfectly
install.packages("rstan") # cran version works just perfectly

#If the system asks for other packages that you do not have, install those as well

#Check out if everything is safe and sound
example(stan_model, package = "rstan", run.dontrun = TRUE)

#Then do the following:
library("rstan") # observe startup messages
#As the startup message says, if you are using rstan locally on a multicore machine and have plenty of RAM to estimate your model in parallel, at this point execute
options(mc.cores = parallel::detectCores())
#In addition, you should follow the second startup message that says to execute
rstan_options(auto_write = TRUE)

#You can try the example again, if nothing is damaged
example(stan_model, package = "rstan", run.dontrun = TRUE)

#Second: try to install cmdstanr, if you do not succeed, it is not a big deal
#they recommend running this is a fresh R session or restarting your current session 
#RESTARTING IS CRUCIAL, do not skip this step and turn off and on you Rstudio again and continue with the lines below
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

#install it (do this just once)
install_cmdstan()

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)

#If this does not work, more datails can be found here: https://mc-stan.org/cmdstanr/articles/cmdstanr.html
#But do not worry too much about it anyway, cmdstanr is faster for most applications, but rstan is just fine

#Third, once rstan and cmdstanr are installed (almost there), then you can install rethinking from within R using:
# On Ubuntu, you will need to install external dependencies via sudo apt-get install libfontconfig1-dev libharfbuzz-dev libfribidi-dev. This will be different on other UNIX distros.
install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))


#Here starts the installation
#If you have R version 4. or newer, continue below. If you want to attempt to install older version of rethinking (version 3.6) go to the line 50 in the setup_Ubuntu_R3.6.R and continue from there. 

#here I use archive version of rethinking before adaptation to R 4.x, for R 4.x, you download from "rmcelreath/rethinking" You can continue with line 50 in the parallel script setup_Windows_R4.R
devtools::install_github("https://github.com/rmcelreath/rethinking/tree/f1a47a37055bb195de2254567791e05e6f81069d") 

#check if rethinking works
library(rethinking)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

# The old version of rethinking is a bit weirdly working with types, so if we leave the original vector c(-1,1), stan throws an error that you cannot draw an integer from normal distribution
#fit_stan <- ulam( f , data=list(y=c(-1,1)) ) 
fit_stan <- ulam( f , data=list(y=c(-1.00001,1.00001)) ) 

precis(fit_stan)

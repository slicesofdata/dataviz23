##################################################  
# Z-test 
# Do people on average tip 20%?
##################################################
dataDir <- "X:/Progs/Data"
setwd("X:/Progs/_install/R/examples")
source("X:/Progs/_install/R/zaatar/R/zaatar.R") #run my 



# Let's make a function out of our z-test formula
zform = function(values, mu, sigma){
  sem = sqrt(sigma / length(values))
  zval = (mean(values) - mu) / sem 
  return(zval)
}
zform2 = function(values, mu, sigma){
  sem = sigma / sqrt(length(values))
  zval = (mean(values) - mu) / sem 
  return(zval)
}

# And a function for the effect size
zeffect = function(z,n){
  r = z/(sqrt(n))
  return(r)
}




m <- mean(3)
# Make up some data
values = c(61, 78, 66, 57, 79, 81) # the sample values

# Look at the sample mean
mean(values)

# Set the known parameters for the population
mu <- 75  # set the test value for the population
sigma <- 20 # define the variance

Z <- zform(values=values, mu=mu, sigma=sigma) # or also z.test(values, mu, sigma)
Z
Z2 <- zform2(values=values, mu=mu, sigma=sigma) # or also z.test(values, mu, sigma)
Z2

zr <- 2/(5.477226)
zr
zr <- zeffect(Z, length(values))
zr

library(asbio)
mu <- 75 
sigma <- 20
xbar <- mean(values, na.rm = TRUE)
n <- length(values)
one.z <- one.sample.z(data = values, null.mu = 75, sigma = sigma, n = n, 
             alternative = "two.sided", conf = 0.95)
one.z$z*

one.z[3[1]]
zr <- -.5714/sqrt(n)
zr

one.z <- one.sample.z(null.mu = mu, xbar = mean(values, na.rm = TRUE), sigma = sigma, n = n)
one.z



# Check the p value
pval <- pnorm(Z, 0, 1, lower.tail = T)
pval
pval < .05 # returns True if pval is less than .05






# let's take a look at the region under the curve between 2 values
pnorm(1, mean=0, sd=1)-pnorm(-1, mean=0, sd=1) # -1 to +1 SD
pnorm(2, mean=0, sd=1)-pnorm(-2, mean=0, sd=1) # -2 to +2 SD
pnorm(3, mean=0, sd=1)-pnorm(-3, mean=0, sd=1) # -3 to +3 SD

pnorm(-3, 0, 1)
pnorm(3, 0, 1)

# Can also see what Z scores correspond to certain areas under the curve.  
qnorm(0.95,mean=0,sd=1) # What is the Z value when there is .95 in the lower tail?
qnorm(0.05,mean=0,sd=1) # What is the Z value when there is .05 in the lower tail?
qnorm(0.025,mean=0,sd=1) # What is the Z value when there is .05/2 in the lower tail?

qnorm(0.01,mean=0,sd=1) # What is the Z value when there is .01 in the lower tail?
qnorm(0.005,mean=0,sd=1) # What is the Z value when there is .01/2 in the lower tail?

##################################################  
# One-sample t-test



# ******Check of the other tip file - one-sample-t-test.R


# Do people on average tip 20%?
##################################################
dataDir <- "X:/Progs/Data"
setwd("X:/Progs/_install/R/examples")
source("X:/Progs/_install/R/zaatar/R/zaatar.R") #run my 

# Remember that we created a function to calculate the z-test value. 
zform = function(values, mu, sigma){
  sem = sigma / sqrt(length(values))
  zval = (mean(values) - mu) / sem 
  return(zval)
}

# We could modify that formula for the one-sample t-test. 
tform = function(values, mu){
  sem = sd(values, na.rm = FALSE) / sqrt(length(values))
  tval = (mean(values) - mu) / sem 
  return(tval)
}

# However, a built-in function already exists; we don't really need write it ourselves.
# We need to specify the data and the population mean
?t.test

values = c(61, 78, 66, 57, 79, 81) # the sample values
mean(values)
mu = 75  # set the test value for the population

# Test our function; specify the data and the population mean arguments
tform(values, mu=75)


# Test the built-in function
t.test (values, mu=75)


#https://stats.stackexchange.com/questions/14931/iterate-through-levels-of-different-factors-by-means-of-regression
#https://www.unt.edu/rss/class/Jon/R_SC/Module9/LMM_Examples.R
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
library(car)
library(readr2)
head(mtcars)

setwd("X:/Courses/CMCStats/0.Curr/0.lect/Topic 09 - Prediction - Bivariate Linear Regression/Tempo and Alcohol")
fname <- paste(getwd(), "/tempo and alcohol - Bach and Schaefer, 1979.csv", sep="")
fname
tempo <- read.csv(fname)
head(tempo)

library(lm)

lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt",
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
summary(lmm.data)
head(lmm.data)
nrow(lmm.data)

lm.1 <- lm(extro ~ open + social, data = lmm.data)
summary(lm.1)
anova(lm.1)

lm.2 <- lm(extro ~ open + agree + social, data = lmm.data) 
summary(lm.2)
anova(lm.2)

anova(lm.1, lm.2)


# However, tradiational OLS Linear Regression can not handle nested (random) effects; such as 
# the model(s) above when scores are located at level 1 (classrooms) nested within level 2 (schools). 
# The next two Linear Models (lm) demonstrate this. Notice in the output, the (categorical) factors
# are automatically dummy coded as would be necessary for them to be considered appropriate predictors 
# in OLS linear regression (but, their nested nature is not specified).

lm.3 <- lm(extro ~ open + social + class + school, data = lmm.data)
summary(lm.3)

lm.4 <- lm(extro ~ open + agree + social + class + school, data = lmm.data)
summary(lm.4)

anova(lm.3, lm.4)









# Using x to predict y
blr <- lm(formula = dan.grump ~ dan.sleep, data = parenthood)
print(blr)
summary(blr)

#Using x1 and x2 to predict y
mlr <- lm(formula = dan.grump ~ dan.sleep + baby.sleep, data = parenthood)
print(mlr)
summary(mlr)

# Once we know the size of r, we can determine if it's size is significantly different from 0
cor.test( x = parenthood$dan.sleep, y = parenthood$dan.grump )

# But you can also get p values too, along with alpha adjustments.
library(lsr)
correlate(parenthood, test=TRUE)

# Condfidence intervals
confint(object = blr level = .99)



# Looking at residuals
residuals(object = blr)

# And standarized residuals
rstandard(model = blr)

# Plot a graph (x = predictor; y = outcome)
plot(x, y)

# Also look at standardized residuals. Studentised residuals/jackknifed residuals
# are different from standardised residuals. Take the ordinary residual and divide 
# it by some quantity in order to estimate some standardised notion of the residual
rstudent(model = blr)


# Examining for and dealing with outliers.
# Leverage: leverage of an observation is defined in terms of its hat value
hatvalues(model = blr)

# Influence: A high influence observation is an outlier that has high leverage
# This is bad for prediction because influence can really change your model
# Cook's distance is a measure of influence 
cooks.distance(model = blr)

# When the main argument x to this function is a linear model object, it will draw 
# one of six different plots, each of which is quite useful for doing regression 
# diagnostics. You specify which one you want using the "which" argument (a number 
# between 1 and 6), otherwise, R will produce all six plots. Use. 4, and 5.
  
plot(x = blr, which = 4)
plot(x = blr, which = 5)

# Now what to go with leverage cases? Look at the model without questionable 
# Cook's distance values by using the subset argument. Remove #64
blrc <- lm(formula = dan.grump ~ dan.sleep + baby.sleep, data = parenthood, subset = -64)

# Then decide to proceed with or without the observations removed.
blr or blrc

#Are residuals distributed normaly?
hist( x = residuals(blr), # data are the residuals
      + xlab = "Value of residual", # x-axis label
      + main = "", # no title
      + breaks = 20 # lots of breaks
      + )

# Standardised residuals plotted as a function of their theoretical quantiles 
# according to the regression model
plot(x = blr, which = 2 )

# Checking the linearity of the relationship
yhat.2 <- fitted.values( object = blr)

# Linearity is best; Big departures from linearity here, suggests problems 
plot( x = yhat.2,
        + y = parenthood$dan.grump,
        + xlab = "Fitted Values",
        + ylab = "Observed Values"
        + )

# or just look at
plot(x = blr, which = 1)

# Or look at the plot and see curvature tests; significance indicates nonlinear
# relationship between the variable and the residuals. Plots the fitted values
# against the residuals for regression model (Pearson residuals=ordinary residuals)
# and residuals for each predictor residuals.
library(car)
residualPlots(model = blr)


# Testing the homogeneity of variance assumption
plot(x = blr, which = 3)
library(car)
ncvTest(blr) #non-constant variance test; significance indicates a violation of HOV
# In this case, the standard error estimates associated with the regression coefficients 
# are not reliable, which also means that the t-tests for the coefficients aren't either.
# Solution: use "heteroscedasticity corrected covariance matrix" when estimating 
# standard errors and test them again; (sandwich estimators); use hccm() in car 

#...or 
library(lmtest)
coeftest(blr, vcov= hccm) # compare these t-values and p-values to the other ones
# They will be similar to the previous t-tests if HOV is not violated much.

# In multiple regression
# Test of collinearity issues; variance inflation factors (VIFs)
library(car)
vif(mlr)
# Square root of the VIF informs how much wider the confidence interval for
# the corresponding coefficient is, relative to what you would have expected
# if the predictors are all nice and uncorrelated with one another.

cor(df) # just get corrlations to looks again


# Selecting a model (be parsimonious) by comparing Akaike information criterion
# (AIC); the model with the smaller AIC value is better.
# step() # forward or backward,
#step(object = mlr, # start at the full model
#     + direction = "backward" # allow it remove predictors but not add them
#     + )

# hierarchical regression
# Better than step methods; just compare AIC for two regression models
# create your models, starting with a null model, then adding predictors. 
M0 <- lm(formula = y ~ x1 + x12, data = df )
M1 <- lm(formula = y ~ x1 + x12 + x2, data = df)

# and compare using the AIC or Bayesian form (BIC)
AIC(M0, M1)
BIC(M0, M1)
# or also
anova(M0, M1)
 




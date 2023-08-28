##################################################  
# One-sample t-test 
# Do people on average tip 20%?
##################################################
dataDir <- "X:/Progs/Data"
setwd("X:/Progs/_install/R/examples")
source("X:/Progs/_install/R/zaatar/R/zaatar.R") #run my 

# read in data
?read.csv
tips <- read.csv("X:/Progs/Data/tips.csv", header = T, sep = ",", dec = ".", fill = T)

# look at the file # to reduce confusion, make sure a column is not the 
# same name as your data frame 
head(tips) #or View(tips)

# look at more details
str(tips) # we see both numeric and factor data

# Is DV (tip) interval/ratio and normally distributed? 0 means no tip left

# look at a histogram
hist(tips$tip) #does not look normal

# What about tips as a proportion of the total bill? Maybe that's better.
# hist(tips$total_bill)
tips$prop <- (tips$tip/tips$total_bill)

# Now take a look
hist(tips$prop) #looks like it could be skewed

# And a quantile-quantile plot
qqnorm(tips$prop, main = "Tips Q-Q Plot") #definitely looks funky

# Do a statistical test for normality
library(nortest) # Load the nortest libary
shapiro.test(tips$prop) # The p value is < .05
lillie.test(tips$prop) # The p value is < .05
# Tipping data appear to be different from what a normal dist. would expect.

# We really should transform the data if we want to do a t-test
tips$proplog <- log10(tips$prop)

# Let's do it all again 
hist(tips$proplog) #looking much better
qqnorm(tips$proplog, main = "Log of Tips Q-Q Plot") #definitely looks funky

shapiro.test(tips$proplog) #  The p value is still < .05
lillie.test(tips$proplog) # The p value is still < .05

# We can also test for skew 
library(moments) #for looking at moments of a disribution
skewness(tips$proplog, na.rm = TRUE) # the skew is negative
agostino.test(tips$proplog) # Even the transformation didn't help

# means and sem
# tips.desc <- breakdown(tips$prop) #breakdown is called from zaatar
# tips.desc

# Boxplot; tips is the dataframe; tips$prop is the dv; NA is not set as null
boxplot(data = tips, prop, na.action = T) # 

# Add a y-axis label for your dv
boxplot(data = tips, prop, na.action = T, ylab = "Tipping %")

# Let's create a constant for all cases in the data; we can look at separate groups later too
head(tips)
tips$all <- 1 # just set all cases equal 1 

# Make sure the variable was created
head(tips) # all is the last variable in the tips data frame

# Run the t-test for determining if the sample mean is significantly GREATER/LESS than 20%
mean(tips$prop[tips$all==1]) # select a subset based on the constant
t.test(tips$prop[tips$all==1], mu = .20, alternative ="two.sided", conf.level=.95) #select only one condition.

# Run the t-test for determining if sample mean is significantly GREATER than 20%
t.test(tips$prop[tips$all==1], mu = .20, alternative ="greater", conf.level=.95) 
# Note: p value is 1 because there is no chance that 16% can be greater than 20% for this one-tailed test

# Run the t-test for determining if sample mean is significantly Less than 20%
t.test(tips$prop[tips$all==1], mu = .20, alternative ="less", conf.level=.95) 
# Note: p value is < .05 ; the overall sample tips less than 20% 

# For Homework.
# H1: Obtain the mean for all men in the dataset for the prop variable in the tips data frame
# Hint: Use the same approach to selecting cases of a variable as we did when 
# we selected all cases (e.g., tip$all==1) for the example.
mean(tips$prop[tips$sex=="Male"])  # take a look at the sample means

# H2: Obtain the mean for all women in the dataset for the prop variable in the tips data frame
mean(tips$prop[tips$sex=="Female"])

head(tips)
boxplot(data = tips, tips$prop~tips$sex, na.action = T) # 
boxplot(data=tips, prop~sex, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Tipping by Sex and Smoking", xlab="Proportion of total bill") 

boxplot(data=tips, prop~sex*smoker, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Tipping by Sex and Smoking", xlab="Proportion of total bill") 


?boxplot
# H3: Conduct a two-tailed one-sample t-test to compare all men to the population mean
t.test(tips$prop[tips$sex=="Male"], mu = .20, alternative ="two.sided", conf.level=.95) #select only male

# H3: Conduct a two-tailed one-sample t-test to compare all men to the population mean
t.test(tips$prop[tips$sex=="Female"], mu = .20, alternative ="two.sided", conf.level=.95) #select only female


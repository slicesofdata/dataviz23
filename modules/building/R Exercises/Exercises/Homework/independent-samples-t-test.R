##################################################  
# Independent samples t-test 
# Do men and women tip the same or differentl?
##################################################
dataDir <- "X:/Progs/Data"
setwd("X:/Progs/_install/R/examples")
source("X:/Progs/_install/R/zaatar/R/zaatar.R") #run my functions

# read in data
tips <- read.csv("X:/Progs/Data/tips.csv", header = TRUE, sep = ",", dec = ".", fill = TRUE)

# Examine the file. Make sure a column is not the same name as your data frame. 
head(tips) #or View(tips)

# Examine more details related to the data frame
str(tips) # we see both numeric and factor data

# Is DV (tip) interval/ratio and normally distributed? 0 means no tip left
# look at a histogram
hist(tips$tip) #does not look normal; transform the data if necessary.

# What about tips as a proportion of the total bill? Maybe that's better.
#hist(tips$total_bill)
tips$prop <- (tips$tip/tips$total_bill)

# Now take a look for each subgroup (men and women).
hist(tips$prop[tips$sex=="Male"])

#
par(mfrow=c(2,2))
hist(tips$prop[tips$sex=="Male"], main="Tipping Behavior", xlab = "Percent of Total Bill", 
     ylab = "Frequency", xlim=c(0,.8),  ylim=c(0, 15), breaks=50, col="blue")
hist(tips$prop[tips$sex=="Female"], main="Tipping Behavior", xlab = "Percent of Total Bill", 
     ylab = "Frequency", xlim=c(0,.8),  ylim=c(0, 15), breaks=50, col="red")

# And a quantile-quantile plot
qqnorm(tips$prop[tips$sex=="Male"], main = "Male Tips Q-Q Plot") #definitely looks funky
qqnorm(tips$prop[tips$sex=="Female"], main = "Female Tips Q-Q Plot") #definitely looks funky
par(mfrow=c(1,1)) #reset the parameters to 1 rol, 1 column

# Look at a boxlplot boxplot(data=thedataframe, dv~group, na.action, xlab, ylab, col=color)
boxplot(data=tips, tips$prop~tips$sex, na.action = NULL, xlab = "Sex", ylab = "Percent of Total Bill", col = "cyan")

# Distribution (should be normal)
# Lets do a Shapiro test for normality
shapiro.test(tips$prop) #IQ should be distributed normally; p should NOT be less than alpha
shapiro.test(tips$prop[tips$sex=="Male"]) #IQ should be distributed normally; p should NOT be less than alpha
shapiro.test(tips$prop[tips$sex=="Female"]) #IQ should be distributed normally; p should NOT be less than alpha

# Or the K-S test
library(nortest) #import the library
lillie.test(tips$prop[tips$sex=="Male"]) # The p value is < .05
lillie.test(tips$prop[tips$sex=="Female"]) # The p value is < .05
# Tipping data appear to be different from what a normal dist. would expect. True for both men and women.
# We really should remove outliers or transform the data to be normal in shape if we want to do a t-test

# means and sem
#tips.desc <- breakdown(tips$prop) #breakdown is called from zaatar
#tips.desc

mean(tips$prop[tips$sex=="Male"]) ; mean(tips$prop[tips$sex=="Female"]) #Use ';' to view together; the means are close
var(tips$prop[tips$sex=="Male"]) ; var(tips$prop[tips$sex=="Female"]) #the means are close

# Test for homogeneity of variance.
library(lawstat)
?levene.test
levene.test(tips$prop, tips$sex, location="mean") # p > .05, so retain H0 that variances are equal.


# Run the t-test for determining if the difference in sample means is larger than 0 under H0. 
t.test(tips$prop[tips$sex=="Male"], tips$prop[tips$sex=="Female"], alternative ="two.sided", 
      paired = F, conf=.95, var.equal=F)
# Setting var.equal = FALSE will correct for differences in variances. 
# Note: The difference between means is not larger than what would be expected due to random variability
# Fail to reject the H0 that the difference = 0


# Because of violations of the normality assumption, the t-test is not appropriate however. 
# Solution, use a test that does not assume normal distributions (e.g., Wilcoxon test)
# The Wilcoxon rank sum test compares MEDIANS for INDEPENDENT groups.

#Take a looks at the medians.
median(tips$prop[tips$sex=="Male"]) ; median(tips$prop[tips$sex=="Female"]) #medians look about the same.

?wilcox.test
wilcox.test(tips$prop[tips$sex=="Male"], tips$prop[tips$sex=="Female"], alternative ="two.sided", 
            paired = FALSE, conf.int = TRUE, conf.level = 0.95)

# Note: The interpretation of medians is the same as the means. 





# For Homework: Examine the data set and compute a different independent samples t-test. 
# Hint: str(tips) to see what variables you can use.
mean(tips$prop[tips$smoker=="No"])
mean(tips$prop[tips$smoker=="Yes"])

t.test(tips$prop[tips$smoker=="No"], tips$prop[tips$smoker=="Yes"], alternative ="two.sided", conf=.95, var.equal=F)



##############
# With two independent variables. Note that the interaction function is 
# not needed, as it is for the other two tests.
?law.stat
leveneTest(len ~ supp*dose, data=tg)


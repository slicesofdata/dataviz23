install.packages("multilevel")
if (!require("nlme")) install.packages("nlme")
library(multilevel)
library(nlme)
library(psych)

  # Need an individual level data.frame
  # and a group level data.frame
  # For the individual level we will use the 
  # cohesion data in the multilevel package
data(package = "multilevel")
data(cohesion)
dim(cohesion)
head(cohesion)

# Then we will create our own group level data.frame
group.size <- data.frame(
  UNIT    = c("1044B", "1044B", "1044C", "1044C"), 
  PLATOON = c("1ST", "2ND", "1ST", "2ND"), 
  PSIZE   = c(3, 3, 2, 3))

head(group.size)

  # We can combine them using the merge() function
new.cohesion <- merge(cohesion, 
                      group.size, 
                      by = c("UNIT", "PLATOON"))
head(new.cohesion) # take a look

  # Sometimes in MLM we want to include a group level variable
  # at the individual level of analysis (e.g. a group mean reassigned to individuals)
  # We can use the aggregrate() function to do that
    # This requires 3 key arguments, first the names of the variables you want to
    # aggregate up to the group level, followed by a list() of the grouping variables,
    # and finally the function you want to use (e.g. mean, length, variance)
TEMP <- aggregate(cohesion[, c("COH01", "COH02")], 
                  list(cohesion$UNIT, cohesion$PLATOON), 
                  mean)
TEMP # Notice the NA value, we can still use na.rm=T if we want

TEMP <- aggregate(cohesion[,c("COH01", "COH02")], 
                  list(cohesion$UNIT, cohesion$PLATOON), 
                  mean, na.rm=T)
TEMP

  # Now we want to merge this with the new.cohesion file. First we need to match
  # the group identifier names. We also want to rename COH01 & COH02 to so that
  # we know they are group means. We'll add "G." in front to do so
names(TEMP) <- c("UNIT", "PLATOON", "G.COHO1", "G.COH02")
TEMP

  # And now merge()
final.cohesion <- merge(new.cohesion, 
                        TEMP, 
                        by = c("UNIT", "PLATOON"))
head(final.cohesion) # Voila! Using merge() & aggregate() to get use ready for MLA
                # on a relatively complicated data set (two levels of nesting)


# Within group agreement & reliability
# We will use a different data set, still in the multilevel package
data(bhr2000)
names(bhr2000)
dim(bhr2000) # Somewhat useful...but how many groups are there?

length(unique(bhr2000$GRP)) # Ah 99 groups
  # Getting within group variances
?rwg # What the hell is rwg?
RWG.RELIG <- rwg(bhr2000$RELIG, bhr2000$GRP, ranvar=2)
RWG.RELIG

psych::describe(RWG.RELIG[,2])  # Descriptives for the group level variances
hist(RWG.RELIG[,2])      # Histogram of the group level variances

  # ICCs
hrs.mod <- aov(HRS ~ as.factor(GRP), 
               data = bhr2000)   # Must use as.factor()
summary(hrs.mod)

ICC1(hrs.mod) # In the multilevel package
  # What percentage of the variance in individual work hours are explained by groups?
ICC2(hrs.mod) # In the multilevel package
  # How reliably can groups be differentiated in terms of avg work hours?
ICC1
ICC2

graph.ran.mean(bhr2000$HRS, 
               bhr2000$GRP, 
               nreps = 1000, 
               limits = c(8,14), 
               bootci = T)
  # This creates a bar graph of group mean average work hours sorted from highest
  # to lowest. The line on the graph represents the means of 99 pseudo groups
  # of the same size as our original groups if group had no effect on the data
  # As we can see, the ICC1 of .17 is not caused by one or two groups 
  # being off the expected line, but a bunch of them.


# Contextual Effects
data(bh1996)
names(bh1996)
head(bh1996)

  # Create a data.frame with Group Number, and two individual variables
  # work hours and well-being
TDAT <- bh1996[,c(1,8,11)]
names(TDAT)                 

  # Use the aggregate function to create group level data
TEMP <- aggregate(TDAT$HRS,list(TDAT$GRP), mean, na.rm=T)
names(TEMP)      # Notice these names suck
names(TEMP) <- c("GRP", "G.HRS")

TBH1996 <- merge(TDAT, TEMP, by="GRP") # Merge individual & Group Data
names(TBH1996)
head(TBH1996)

tmod <- lm(WBEING ~ HRS + G.HRS, data=TBH1996) # A linear model
summary(tmod)     # Notice Group Hours (G.HRS) is significant

plot(TBH1996$WBEING ~ TBH1996$HRS, xlab="Work Hours", ylab="Well-Being", type="n") # type=n omits the points
abline(lm(WBEING ~ HRS, data=TBH1996), col="blue") # Individual level slope
abline(lm(WBEING ~ G.HRS, data=TBH1996), col="red") # Group level slope
legend("topright", legend=c("Individual Level Slope", "Group Level Slope"), col=c("blue", "red"), lty=1, bty="n")
  # Notice the group level effect has as steeper slope
    # However, this method is not appropriate because we ignore the fact
    # that individuals are nested in groups...thus the group level standard error
    # is likely to be too small. We need a random coefficient model...


# Multi-level Random Coefficient Modeling
  # In MLM, we typically want to know three things
    # 1) What level 1 (individual level) factors are related to within-group variance?
    # 2) What level 2 (group level) factors are related to between group differences in intercepts?
    # 3) What level 2 (group level) factors are related to between group differences in slopes?

# We usually assume there are within group variances
  # So we usually start by examining whether there is significant variation in 
  # group intercepts and slopes
  # Unconditional Means Model
Null.Model <- lme(WBEING ~ 1, 
                  random = ~1|GRP, 
                  data = bh1996, 
                  control = list(opt = "optim"))
  # First we list the fixed part of the equation. In this case, we say WBEING ~ 1. 
  # This means that the only predictor of WBEING is an intercept term
  # The random formula comes next, random = ~1|GRP. This means that the intercept
  # may vary as a function of group membership. The data part gives it the data
  # set name and the control argument tells R to use the general purpose optimization
  # routine. However, a slightly better optimizer is "nlmimb" but, it sometimes
  # fails to converge. So in practice, use "nlmimb" but revert to "optim" if you
  # fail to converge.

summary(Null.Model) # Not a lot to do here
  # We need to know if the between group variance is enough to merit
  # MLM. Use the VarCorr() function.
VarCorr(Null.Model)
.03580079 / (.03580079+.78949727) # To get the ICC

  # This is similar to an ICC from an ANOVA model
tmod <- aov(WBEING ~ as.factor(GRP), data=bh1996)
ICC1(tmod)

  # For effects to occur at the group level, it helps to have reliable
  # group means. We can use GmeanRel() to get that information for us

GREL.DAT <- gmeanrel(Null.Model) 
names(GREL.DAT)         
GREL.DAT
GREL.DAT$MeanRel  # Here are the reliabilities for the group means
mean(GREL.DAT$MeanRel) # The average group mean reliability
  # We can confirm with an ANOVA model
ICC2(tmod) # In this case it is slightly higher because of unequal group sizes
          # With equal group sizes, these numbers would be identical

# Of note, we can also estimate models using Full, rather than Restricted, 
# maximum likelihood
mod.ml <- lme(WBEING ~ 1, random=~1|GRP, data=bh1996, method="ML", control=list(opt="optim"))
VarCorr(mod.ml) # In this case, both ML and REML provide identical answers

  # Ok. So was the variance component from the Unconditional Means Model Significant?
    # We will use gls() to create a model with no random intercept, and then
    # compare the two 
Null.Model.2 <- gls(WBEING ~ 1, data=bh1996, control=list(opt="optim"))
logLik(Null.Model.2)*-2 # Getting a chi-square for the no-intercept model
logLik(Null.Model)*-2 # Getting a chi-square for the random intercept model
logLik(Null.Model.2)*-2 - logLik(Null.Model)*-2 # This is a chi-square difference
anova(Null.Model, Null.Model.2) # Testing for a difference
  # Thus, a model that includes group level information will be better
  # than a model that does not.

  # Now, lets say we think hours of work is related to well-being. And we 
  # suspect that group level of hours of work will predict well-being along
  # with the effect of individual level hours of work. So, we will 
  # predict well-being from individual level work and group level work
  # simultaneously.

Model.1 <- lme(WBEING ~ HRS + G.HRS, random=~1|GRP, data=bh1996, control=list(opt="optim"))
summary(Model.1)

  # How much variance did we explain? Remember that previously we had a group
  # level variance in Well-Being of .0358 and a within group variance of .780.
VarCorr(Model.1) # We explained a lot of between group variance, and some within group

  # So far, we have only focused on explaining variation in intercepts (means)
  # what about slopes? 
  # First let's visualize slope variation...we'll use a new graphics
  # package (lattice) to do so.
install.packages("lattice")
library(lattice)

  # xyplot is similar to plot(), but it allows us to use a | to divide the plot
  # up by groups easier. Notice that we only selected data from the first 25 groups
  # because plotting 99 groups would be too much to see.
xyplot(WBEING~LEAD|as.factor(GRP), data=bh1996[1:1582,], type=c("p", "g", "r"),
      col="dark blue", col.line="black", xlab="Leadership Consideration",
      ylab="Well-Being")
  # This is probably a more straightfoward way to see that we have taken 
  # a sub-selection of groups. 
xyplot(WBEING~LEAD|as.factor(GRP), data=subset(bh1996, bh1996$GRP < 26), type=c("p", "g", "r"),
      col="dark blue", col.line="black", xlab="Leadership Consideration",
      ylab="Well-Being")
  # Here is another way...      
xyplot(WBEING~LEAD | as.factor(GRP), 
       data=bh1996[bh1996$GRP < 26,], 
       type=c("p", "g", "r"),
      col="dark blue", 
      col.line="black", 
      xlab="Leadership Consideration",
      ylab="Well-Being")
      

  # There appears to be slope variation, but is it significant?
Model.2 <- lme(WBEING ~ HRS + LEAD + G.HRS, 
               random = ~LEAD|GRP, 
               data = bh1996, 
               control = list(opt = "optim"))
summary(Model.2)
  # Well, LEAD is a statistically significant predictor of Well-Being
  # but we want to know if the slopes have significant variances
  # We need to test the -2*LogLik of a model without a random slope
  # estimated for Leadership Consideration on Well-Being. We can use the update()
  # function to do so this time because we already have a model with the 
  # random slope included.
Model.2a <- update(Model.2, random=~1|GRP)
anova(Model.2, Model.2a) # And then compare the two models using ANOVA
    # The model is statistically significant so the model with LEAD estimated
    # as a random slope is better than one without it.

    # Ok. So both Work Load and Leadership predict well-being, but might they
    # interact? That is, maybe leadership has more of an effect when group
    # work load is high. This is a cross-level interaction.
?lme

Final.Model <- lme(WBEING ~ HRS + LEAD + G.HRS + LEAD:G.HRS, random=~LEAD|GRP, data=bh1996,
      control=list(opt="optim"))
round(summary(Final.Model)$tTable,dig=3) # So there is a significant interaction
    # Let's make a picture of it though
mean(bh1996$G.HRS)
sd(bh1996$G.HRS)
  # One SD below and above the Mean
mean(bh1996$G.HRS) - sd(bh1996$G.HRS); mean(bh1996$G.HRS) + sd(bh1996$G.HRS)
mean(bh1996$LEAD) - sd(bh1996$LEAD); mean(bh1996$LEAD) + sd(bh1996$LEAD)

  # Now we can build a small data set that includes the interacting variables
  # and the means for the non-interacting variables (i.e. individual hours)
  # Then use predict() to get estimates of the outcome 
  
small <- data.frame(HRS=c(11.2987, 11.2987, 11.2987, 11.2987), 
                    LEAD=c(2.12, 2.12, 3.66, 3.66),
                    G.HRS=c(10.44, 12.16, 10.44, 12.16),
                    GRP=c(1,1,1,1))
small
predict(Final.Model, small, level=1)
small$WBEING <- predict(Final.Model, small, level=1) 
with(small, interaction.plot(LEAD,G.HRS,WBEING)) 
?glm

  # Can make the effect look a bit more dramatic
  # by using 7 and 12 for the Group Hours
small <- data.frame(HRS=c(11.2987, 11.2987, 11.2987, 11.2987), 
                    LEAD=c(2.12, 2.12, 3.66, 3.66),
                    G.HRS=c(7, 12, 7, 12),
                    GRP=c(1,1,1,1))
predict(Final.Model, small, level=1)
small$WBEING <- predict(Final.Model, small, level=1)  
with(small, interaction.plot(LEAD,G.HRS,WBEING))
  # So...leadership matters more when workload (i.e. stress) is high
  
# Centering considerations
  # You might want to think about whether you want to center your
  # variables at the group level or at the Grand level. Grand mean
  # centering is often identical to using raw variables (as we did here)
  # but can reduce multi-collinearity and convergence difficulty.
  # One should know why one wants to use group mean centering when doing so
  # but both are easy to do in R. For grand mean centering, simply
  # use scale(x, scale=F) on the raw variable. For group mean centering
  # use aggregate() as we did before to create groups means, then subtract
  # them from the raw individual scores. 
  

# The future of MLM in R:
# The lme4 package is probably both easier to use and more flexible
# than the nlme package. The function for linear mixed effects models
# is lmer() (pronounced "elmer"). But it also has functions for 
# non-linear (i.e. generalized) mixed models and growth curve modeling.
# Unfortunately, while the lmer() function works the entire package, 
# which will have many other great functions, is not yet finished and there
# is no timeline for when it will be completed yet. Here is the basic formula
# for lmer() though.

fm1 <- lmer(outcome ~ 1 + (1|GrpID), data=datasetname)
fm2 <- lmer(outcome ~ IV1 + (1+IV1|GrpID), data=datasetname)
fm3 <- lmer(outcome ~ IV1 + (1+IV1|GrpID) + (1+IV1|Nest2ID), data=datasetname)


# The incredibly amazing reshape() function in R.
# So your data aren't in "long," or "person-period," or "stacked" format
# (i.e. your cases only occupy one row and your repeated measures are
# in the columns. 

	# First, let's make up some data in that format
set.seed(123)
wide.df <- data.frame("SID"=c(1, 2, 3, 4, 5, 6), "Time1"=rnorm(6), "Time2"=rnorm(6), "Time3"=rnorm(6), "Gender"=c(rep("M",3),rep("F",3)))
wide.df
dim(wide.df)

# Now, let's recognize what we want. We want Times 1-3 to be in one column.
# We also want to create a new column that keeps track of this "time"
# information with the scores 1, 2, and 3 in it. Finally, we also
# want gender to be correctly associated with each record.
# We will use the reshape() function to make it so.

?reshape

long.df <- reshape(wide.df, varying=names(wide.df[,2:4]), timevar="Time", idvar="SID", times=names(wide.df[,2:4]), 
				v.names="Score", direction="long")
long.df
dim(long.df)

	# So what are we saying here. First, we name the data set we wish to reshape. Then we give
	# it the names of the time-varying variables that we would like to make into one column.
 	# If you have more than one set of time varying variables, you can use more than one
	# names() part and wrap them both in a list() function. So list(names(set1vars),names(set2vars)).
	# Next we tell it what we want to call the new column(s) that will be created carrying
	# the names of the old columns. By default, reshape() calls it Times or Time, but that is
	# actually what we want here so that is ok. In some cases you may want to call that
	# "items," or "measurement" or "condition." Next we specify the identifying variable(s)
	# that group the data together. The "times" argument specifies the values for the 
	# newly created value (which we are calling Time). In this case, we want it to be the 
	# names of the already existing variables, so we will just use a names function.
	# But you could put anything in there as long as it has as many elements as
	# you told it things you want to reshape (i.e. are varying). v.names lets you
	# name the "stacked" time measurements variable. By default it tends to
	# name it whatever the name of the first time-varying variable was. 
	# Lastly, we tell it we would like the direction to be "long".


# There are other features of reshape() as well incuding a drop= argument
# that lets your drop variables before reshaping. Notice we didn't have to even
# mention the gender variable and it did what it was supposed to.

# What if we want to go back? 
reshape(long.df, direction="wide")
	# R keeps in memory the way it used to look and puts it back.
	
# This was a simple example of using reshape(). Learning how to use
# it takes practice, but with mastery reshape is one of the most
# incredibly useful data managment tools R has to offer (imo). 

# Check your working directory
getwd()
# 
setwd("C:/users/gcook/Desktop/Psyc109/")

# The following command installs the package on your computer 
install.packages("psych") 
install.packages("moments") 

# The following command loads the package for use in the current script
library("psych")
library("moments") 

# Read the csv file. Because the csv data file is in the "data" directory of your 
# working directory, you need to specify the subdirectory location. 

# paste() will let us paste the directory path information together.
file <- paste(getwd(),"/data/cdc.csv")
file # but notice the space after Psyc109

file <- paste(getwd(),"/data/cdc.csv", sep = "") # remove the separation
file

cdc <- read.csv(file)

# or you could put the paste function call into the read.csv() function directly.
cdc <- read.csv(paste(getwd(),"/data/cdc.csv", sep = ""))

# This command will show you the data
cdc

View(cdc) # View the data file
is.data.frame(cdc) # Determine if the data is a data frame object
str(cdc) # Let's you know the type of the data (e.g., numeric, integer, Factor, etc.)
dim(cdc) # dimensions of the data frame; 20,000 observations; 9 columns
names(cdc) # What are the column/variable names in the data frame?
head(cdc) #Shows the first few cases of your data set
tail(cdc) #Shows the last few cases of your data set

mean(cdc$weight) #Shows the Mean of the variable weight
median(cdc$weight) #Shows the Median of the variable weight
var(cdc$weight) #Shows the variance of the variable weight
sd(cdc$weight) #Shows the Standard Deviation of the variable weight
summary(cdc$weight) #Provides a summary of the variable weight
skewness(cdc$weight) #Shows the Skewness of the variable weight
kurtosis(cdc$weight) #Shows the Kurtosis of the variable weight

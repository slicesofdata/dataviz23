#look into the view option

https://stackoverflow.com/questions/18462736/looping-through-column-names



.libPaths() # get library location
library()   # see all packages installed
search()    # see packages currently loaded

loadlibs <-c("Hmisc", "ggplot2", "lawstat", "coin", "plyr", "psych", "nortest")
lapply(loadlibs, library, character.only = TRUE)

#define the csv file name
setwd("G:/Dropbox/Progs/_install/R/examples")
#define the directory with the datafile file name
dirName <- "X:/Courses/CMCStats/0.Curr/0.lect/Topic 12 - Students t-test - Independent Samples/"
#xlsxfilename <- "relaxation-headach?nortes.xlsx"

#define the csv file name
csvfilename <- "relaxation-headaches.csv"
#df <- read.xlsx2(paste(dirName, filename, sep =""), header = TRUE, sheetName = "Sheet1")
#typeof(df)
#t.test(before,after)

#Read in the xlsx file into a dataframe.
#df <- read.xlsx2(paste(dirName, xlsxfilename, sep =""), sheetIndex, sheetName="Sheet1", startRow=1,
#           colIndex=NULL, endRow=NULL, as.data.frame=TRUE, header=TRUE,
#           colClasses="character")

#Read in the csv file into a dataframe; paste the dirName and the filename together and remove spaces
rm(df)
df <- read.csv(paste(dirName, csvfilename, sep =""), header=TRUE)
typeof(df)

#read.csv reads a table as a data frame, but in other instances, you 
#can make sure the file has been read in as a dataframe; see also as.data.frame(df)
is.data.frame(df) # will return TRUE if it is, otherwise FALSE

#Take a look inside
str(df) 

#Look at the first several lines of the data frame
head(df)

#Make sure the column names are acually column names rather than data
colnames(df)

#Determine if the variable of interest is a factor or numeric
is.factor(df$headaches)
is.numeric(df$headaches)
#make it numeric if a factor headaches <- as.numeric(as.character(headaches))

#Do a t-test, test variances, test normality, and plots
df[df.headaches]
vars = c(df[headaches],df[before])
for (var in names(vars)) {
  lillie.test(i)
}

#Normality examination for each variable
lillie.test(df$before)
qqnorm(df$before) #look at a qqplot
hist(df$before)
lillie.test(df$after)
qqnorm(df$after) #look at a qqplot
hist(df$after)

#Vizualize
boxplot(df$before,df$after,ylab="Headaches", names=c("Before","After"), main="Number of Headaches Before and after\n Relaxation Training")

#t-tests with and without variances corrected (Welch's correction)
t.test(df$headaches ~ condition, data=df, var.equal=T, alternative="two.sided")
t.test(df$headaches ~ condition, data=df, var.equal=F, alternative="two.sided")
#"two.sided", "less", "greater"
t.test(df$before,df$after,alternative="greater",var.equal=T)
# or t.test(df$before,df$after)

#Determine if variances are equal
levene.test(df$before, df$after)
bartlett.test(df$headaches,df$condition)

#One-sample t-test
headacheNormal <- df$before
t.test(df$after,mu=headacheNormal) # Ho: mu=3

#Permutation test
oneway_test(df$headaches ~ as.factor(df$condition), distribution=approximate(B=bnum))



indttest <- function(x, y, loc) {
  shapirox <- shapiro.test()
  print(paste("Shapiro for", x, shapirox))
 # print(paste(y, shapiro.test())
  print(t.test(x,y))
  print(var.test(y,x))
  print(levene.test(y,x,location=loc))
}
x <- df$before
y <- df$after
indttest(y,x,"median")

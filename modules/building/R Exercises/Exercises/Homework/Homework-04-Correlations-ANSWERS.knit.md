---
title: "HW04 - Correlations"
author: "replace with your names"
date: "replace with the date"
output: 
  html_document:
    toc: true # this will create a table of contents of hyperlinks (change true to omit)
    toc_depth: 2
---

#Part A
## 0. Before you begin 

This homework exercise involves having you answer some questions, write some code, and create a nice HTML file with your results. When asked different questions, simply either type your coded or provide written responses after the ANSWER message. When asked to write code to complete sections, type your code in the empty code blocks that follow the **ANSWER** line. After adding your code, you must make sure that it will execute. So remember to run your code by highlighting it and pressing the RUN button or pressing PC: `CONTROL+ENTER`; Mac: `COMMAND+ENTER`. 

If your code does not execute, then `R Markdown` won't know what you are telling it to do and your HTML file will not be produced. Also, don't create your HTML file until you finish.

## 1. Downloading the source files 

1.1. Run the code below to download the some libraries needed for this assignment. By using `include=FALSE, cache=FALSE` in the r code header, we will make sure that any error messages do not appear in the HTML file you create.




1.2. Key functions used for this assignment. Functions from libraries that are not built into the base `R` version will use specific calls which are preceded by the library name and two colons [e.g., `library::function()`]: 

- `cor()` for computing the correlation between two variables
- `cor.test()` for computing the correlation between two variables and showing significance 
- `cov()` for computing the covariance of two variables
- `lattice::histogram()` for plotting nicer histograms
- `lattice::xyplot()` for showing the data in a histogram
- `library()` for loading libraries that are not in the base version of `R`
- `mean()` for the mean of a variable
- `moments::kurtosis()` for the kurtosis of a variable 
- `moments::skewness()` for the skewness of a variable 
- `read.csv()` for reading a csv file into a data frame
- `sqrt()` for computing the square root
- `var()` for computing the variance of a variable
- `View()` for viewing the data.frame
- `with()` for selecting the data frame on which to perform other operations 


## 2. Loading libraries
Use the `library()` function to load the following libraries: moments, lattice, psych

**ANSWER:** 

```r
library(moments)
library(lattice)
library(psych)
```


## 3. Checking your working directory 
Use `getwd()` to make sure that your working directory points to Psyc109 on your desktop. If the directory is not correct, set it using `setwd()`. See previous homework for more details using `setwd()`.




#Part B
## 1. Pearson's *r* Correlation
Pearson's *r* describes the magnitude of a linear relationship between two variables. More precisely,
the usual measure of a correlation describes the relationship between two equal interval
numeric variables. This part will examine how to use the Pearson *r* correlation.

## 2. Assumptions of bivariate linear correlation using Pearson *r*

First, let's take a look at the assumptions of correlations the Pearson *r* correlation coefficient: 

  1. Our two variables should be measured at the interval or ratio level (i.e., they are continuous).

  2. There needs to be a linear relationship between the two variables.

  3. There should be no significant outliers. 

  4. Your variables should each be approximately normally distributed.


## 3. Reading in a data file ##
3.1. Read a data file and assign its contents to a data frame object named `ICECREAM`. We could name this object any name we want (e.g., bananasundae, ketchup, or ice). In other words, the object name does not need to match the name of the file. In this case, however, "ICECREAM" seems appropriate as a name.

Ex: `dataframe <- read.csv("filename.csv")`


**ANSWER:**








































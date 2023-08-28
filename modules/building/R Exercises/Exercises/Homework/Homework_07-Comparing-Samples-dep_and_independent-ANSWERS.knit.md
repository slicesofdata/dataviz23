---
title: "HW 7: Independent and Dependent *t*-tests"
author: "partner names"
date: "type date"
output: 
 html_document:
   toc: true # this will create a table of contents of hyperlinks (change to false to omit)
   toc_depth: 2
---

#Part A

General Questions (optional: some students wanted these questions in the past)

1. **QUESTION:** Describe how the independent-samples *t*-test is different from the paired-samples *t*-test. 

*ANSWER:* 


2. **QUESTION:** What is an assumption of the independent-samples *t*-test that is different from the other *t*-tests.

*ANSWER:* 

#Part B

Before you begin: This homework exercise involves you answering some questions, writing some code, and creating a nice HTML file with your results. When asked different questions, simply type your coded or written responses after the ANSWER message. When asked to write code to complete sections, type your code in the empty code blocks that follow the ANSWER message (between the back ticks). After adding that code, you must make sure that it will execute. So, remember to read the content and run each line of your code as you write it so that you know it executes correctly. If your code does not execute, then R Markdown won't know what you are telling it to do and your HTML file will not be produced. Also, don't create your HTML file until you finish and know that all your code works correctly.


##1. Checking your working directory
Always make sure that your working directory points to Psyc109 on your desktop. 

```r
getwd()
```

```
## [1] "C:/Users/lklap/Dropbox/Exercises/Homework"
```


1.2. Key functions used for this assignment: 

- `as.data.frame()` for combining vectors into a data frame
- `by()` for subsetting groups 
- `lattice::histogram()` for histograms
- `length()` obtain the length of an object or variable
- `car::leveneTest()` for testing the homogeneity of variance assumption 
- `mean()` obtain the mean of an object or variable
- `nortest::lillie.test()` for testing normality
- `options()` for changing the scientific notation display (e.g., scipen = 999)
- `qqnorm()` and `qqline()` for examining normality
- `read.csv()` read in a csv file
- `shapiro.test()` for testing normality
- `str()` to obtain the structure of a data.frame
- `t2r()` for calculting the effect size for a t.test; we built this function
- `t.test()` for calculating t-tests
- `wilcox.test()` for comparing two groups when *t*-test assumptions are violated
- `xlsx::read.xlsx()` for reading an Excel spreadsheet file as a data frame
- `xlsx::write.xlsx()` for writing a data frame to an Excel file


##2. Loading libraries

2.1. Run the code below to download some libraries needed for this assignment. By using `include=FALSE, cache=FALSE` in the `R` code header, we will make sure that any error messages do not appear in the HTML file you create.



If you know what libraries you will use for your code, you can load them now. Use `library()` to load the following libraries: `lattice`, `nortest`, `moments`, `lattice`, `psych`, `xlsx`. Install any that are missing. 


*ANSWER:* 

```r
library(lattice)
library(nortest)
library(moments)
library(lattice)
library(psych)
#library(xlsx)
```

Change from scientific notation if you want. 


```r
options(scipen = 999) # to turn on, set: options(scipen=0)
```

##3. Overivew of the *t*-test for paired sample means

3.1. The situation you just learned about (the *t*-test for a single sample) is for when you know a population's mean but not its variance and you also have a single sample of scores. It turns out that in most research you do not even know the population's mean; plus, in most research situations, you usually have not one set but two sets of scores. These two things, not knowing the population mean and having two sets of scores, almost always go together. This situation is very common. This kind of research situation is called a repeated-measures design (also known as a within-subjects design). A common example is when you measure the same people before and after some social or psychological intervention. This specific kind of repeated-measures situation is called a *before-after design*.


3.2. The hypothesis-testing procedure for the situation in which each person is measured twice (that is, the situation in which we have a repeated-measures design) is called a *t*-test for dependent means. The *t*-test for dependent means is also called a paired-sample *t*-test, a *t*-test for correlated means, a  *t*-test for matched samples, and a *t*-test for matched pairs. Each of these names come from the same idea that in this kind of *t*-test you are comparing two sets of scores that are related to each other in a direct way, such as each person being tested before and after some procedure.

3.3. You do a *t*-test for dependent means in exactly the same way as a one-sample *t*-test, except that:

- **(a)** you calculate and compare the difference of the paired values (e.g., difference scores)

- **(b)** you assume that the population mean (of the difference scores) is 0 under H0. 



3.4. With a repeated-measures design, your sample includes two scores for each experimental unit (e.g., person) instead of just one. The way you handle this is to make the two scores per person into one score per person! You do this magic by creating difference scores: For each person, you subtract one score from the other. If the difference is before versus after, difference scores are also called change scores.

To show this we are going to read in pre-test and post-test scores from a group of students who ranked their happiness scores (0 least happy to 30 most happy) before and finals week and assign them to a data frame object named HAPPY. There are 3 variables" `Student` containing the student number, `Pre` for happiness scores before finals, and `Post` for happiness scores after finals. These can be added to a data frame using `data.frame()`.



```r
# create a data frame with your variables.
Student = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Pre = c(18,21,16,22,19,24,17,21,23,18,14,16,16,19,18,20,12,22,15,17)
Post = c(22,25,17,24,16,29,20,23,19,20,15,15,18,26,18,24,18,25,19,16)
HAPPY <- data.frame(Student, Pre, Post)
write.csv(HAPPY, "FinalsHappiness.csv")
# you can save your data frame to your working directory as an excel file using the xlsx library if you want 
# xlsx::write.xlsx(HAPPY, "FinalsHappiness.xlsx", sheetName = "Happy", row.names = F)

# If you don't like creating data frames in R, you enter the data into an Excel file and read it in as shown below. If you do not name your worksheet in Excel, the default name will likely be Sheet1 

# read in your excel file (no needed if you created the data frame above)
# HAPPY <- xlsx::read.xlsx("your filename", sheetName = "Sheet1 or sheetname")

str(HAPPY)
```

```
## 'data.frame':	20 obs. of  3 variables:
##  $ Student: num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Pre    : num  18 21 16 22 19 24 17 21 23 18 ...
##  $ Post   : num  22 25 17 24 16 29 20 23 19 20 ...
```

As you can see there are 20 observations and 3 variables. If you think of the paired *t*-test as a one-sample test with `t.test()`, you can create the difference scores so we can analyze the data to examine if the difference is close to or far away from 0 (mu). Make note of the values in the output. The default test is two-tailed. 


```r
HAPPY$Diff <- with(HAPPY, Pre - Post)

with(HAPPY, t.test(Diff, mu = 0))
```

```
## 
## 	One Sample t-test
## 
## data:  Diff
## t = -3.2313, df = 19, p-value = 0.004395
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -3.3778749 -0.7221251
## sample estimates:
## mean of x 
##     -2.05
```


3.5. To conduct a paired-samples *t*-test without the difference scores, we will use `t.test()` as well as write out the equation. The two variables are included as separate arguments; the order does not matter. Make sure to specify that the data are paired by setting the paired argument to TRUE `(e.g., paired = TRUE)`. You did not have to do this for the one-sample *t*-test and by default the test is two-tailed. 

The general formula is:  

- Ex: `t.test(dependent variable 1, dependent variable 2, paired = TRUE)`


```r
with(HAPPY, t.test(Pre, Post, paired = TRUE))
```

```
## 
## 	Paired t-test
## 
## data:  Pre and Post
## t = -3.2313, df = 19, p-value = 0.004395
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.3778749 -0.7221251
## sample estimates:
## mean of the differences 
##                   -2.05
```

Note how the values are the same as the one-sample test.


3.6. The *t*-value, df, *p*-value, and mean difference scores are included in the output. If you use the *p*-value to help determine the evidence for the difference value being the same or different from 0, the paired-samples *t*-test reveals the true difference in means is not equal to 0. Rather, there is a difference in happiness before and after final exams. Finally, because the sample difference score is estimated from the sample, it is not a completely accurate representation of the population difference. A 95% confidence interval provides a range for the actual difference score that you might expect the true difference to be. Notice that the interval does not include a difference of 0, so you can be 95% confident that the real difference score is not 0. Instead, there must be some systematic reason for the difference in happiness scores that is not simply due to random chance. Perhaps students are stressed out before final exams and they relax after them. 

To report the outcome of the test, the general form is:

  *t*(df) = t-value, *p* < or > alpha 

By substitution and making sure to italicize t and p, we can say:

  Students' happiness ratings were statistically higher following final exams than they were before those exams, *t*(19) = 3.38, *p* < .05.


##4. Assumptions for *t*-tests

4.1. As we have seen, when you are using an estimated population variance, the comparison distribution is a *t* distribution rather than a *z* distribution (normal).

However, in order to perform a *t*-test, there are few assumptions that should be met. 


4.2. Assumptions for the paired-samples *t*-test

- **(a)** The data must be interval/ratio
- **(b)** The difference scores for the matched pairs should follow a normal probability distribution.
- **(c)** The sample of pairs represents a random sample from its population


4.3. When you have a repeated-measures design with two measurement variables and do not have interval/ratio data or have violated normality, you can use an alternative test called the Wilcoxon Signed-Ranks Test using the `wilcox.test()` from the built-in `stats library`. However, if the X1 and X2 values are the same (ties), they are dropped from the analyses.

Similar to the paired-samples *t*-test, the function takes the form:

- Ex: `wilcox.test(dependent variable 1, dependent variable 2, paired = TRUE)` 


```r
with(HAPPY, wilcox.test(Pre, Post, paired = TRUE))
```

```
## Warning in wilcox.test.default(Pre, Post, paired = TRUE): cannot compute
## exact p-value with ties
```

```
## Warning in wilcox.test.default(Pre, Post, paired = TRUE): cannot compute
## exact p-value with zeroes
```

```
## 
## 	Wilcoxon signed rank test with continuity correction
## 
## data:  Pre and Post
## V = 29, p-value = 0.008105
## alternative hypothesis: true location shift is not equal to 0
```

The Wilcoxon signed-rank test does not calculate the magnitude of the difference between X1 and X2 scores. Rather, it examines the ranking of the variables; how many times X1 > X2 and X2 > X1 independent of the size of the difference. When X1 and X2 are the same (ties), they are dropped from the analysis. The test compares the sum of the ranking order of the difference (e.g., the number of times X1 > X2 vs. X2 > X1) and compares that to a distribution that assumes the ranks are the same as expected under H0. The output provides a Wilcoxon test value and a *p*-value associated with the likelihood of such differences occurring under H0.

If you examine the data, you will see that the Diff variable has only 1 tie (0), 4, instances of Pre > Post scores and 15 instances of Post > Pre scores. If using the *p*-value and alpha of .05 to make decisions about Pre = Post, you will see that p < .05, so that can be used as evidence of a difference in happiness before and after final exams using the Wilcoxon signed-Ranks Test for paired samples. 


By substitution and making sure to italicize V and p, we have:

  Students' happiness ratings were statistically higher following final exams than they were before those exams, *V* = 29, *p* < .05. 


4.4. Paired-Samples *t*-test: The estimated effect size for a study using a *t*-test for dependent means is the mean of the difference scores divided by the estimated standard deviation of the population of difference scores. Thus, Cohen's *d* for a paired-samples *t*-test is simply the size of the mean difference as a function of the standard deviation of the difference. If we go back to the pre-test and post-test scores for happiness before and after finals week we can determine the effect size below:


```r
# using the difference score,
#with(HAPPY, cohens.d.one.sample(Diff, mu = 0) )

# or create the equation by code
with(HAPPY, abs((mean(Diff)) / sd(Diff)) )
```

```
## [1] 0.7225301
```

From that example we have an effect size of about .72 which reveals a medium Cohen's *d* effect size which is on the higher end of medium effects. Because classifying the value can lead to confusion, just state the numeric value and don't label the effect as small, medium, or large; instead, let individuals decide for themselves what is small or large.


As Field suggests, r is a form of effect size which can be derived from the values of the *t*-test. I've made a function for converting *t* to *r*. You can simply put `t.test()` inside the function.

- Ex: `t2r(t.test())`


```r
#t2r(with(HAPPY, t.test(Pre, Post, paired = TRUE)))
```

Using *r* as a measure of effect size might indicate that the effect is *large* rather than a high medium effect size like Cohen's *d*.

#Part C
##1. Overview of the *t*-test for independent-samples

1.1. The independent-samples *t*-test is used to compare two independent groups on a single dependent variable; experimental units provide only one dependent variable. Independent groups simply refers to the groups as indendent of each other; experimental units that are in one group are not the same as those in another group. This is very different from the dependent aspect of the paired-samples *t*-test. The independent-samples *t*-test has the same assumptions of normality and measurement scale as the other *t*-tests. These assumptions should be tested as usual, but are not always done for this assignment. In addition, there is a new assumption related to the groups. 

1.2. Under H0, the assumption for this test is that the population values (e.g., means) of two independent groups is the same (e.g., mu1 = mu2). Because samples estimates those populations, the independent-samples *t*-test tests whether a difference between the two population means is different enough from 0 to suggest that H0 is not a reasonable account of the data. 

1.3. Because the assumption is that the two populations are equal to one another, the samples taken from them should yield the same means and variances. Having equivalent variances is an assumption of this test called the *homogeneity-of-variance* assumption. To the extent that the variances are not equivalent, there is a violation of the assumption known as *heterogeneity-of-variance*. Because the sample size can influence variance, having roughly equivalent ns for the two groups is advised. There are two ways to address this concern of equivalent variance: 


- **(a)** compare the variances statistically using a test like the Levene's test using `car::levene.test()`, or 
- **(b)** do not assume the variances are equal by specifying this by setting your `t.test()` to adjust the results of *t*-test for you using a Welch correction


##2. Doing the independent-samples *t*-test

2.1. We can test some data that have met the conditions for doing an independent-samples *t*-test by comparing the two sections of the class on the Sporle accuracy movie data.  Which class has more movie knowledge or are they the same?


```
## 'data.frame':	58788 obs. of  24 variables:
##  $ title      : Factor w/ 56007 levels "'?' Motorist, The",..: 47 48 49 50 51 52 53 2 3 4 ...
##  $ year       : int  1971 1939 1941 1996 1975 2000 2002 2002 1987 1917 ...
##  $ length     : int  121 71 7 70 71 91 93 25 97 61 ...
##  $ budget     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ rating     : num  6.4 6 8.2 8.2 3.4 4.3 5.3 6.7 6.6 6 ...
##  $ votes      : int  348 20 5 6 17 45 200 24 18 51 ...
##  $ r1         : num  4.5 0 0 14.5 24.5 4.5 4.5 4.5 4.5 4.5 ...
##  $ r2         : num  4.5 14.5 0 0 4.5 4.5 0 4.5 4.5 0 ...
##  $ r3         : num  4.5 4.5 0 0 0 4.5 4.5 4.5 4.5 4.5 ...
##  $ r4         : num  4.5 24.5 0 0 14.5 14.5 4.5 4.5 0 4.5 ...
##  $ r5         : num  14.5 14.5 0 0 14.5 14.5 24.5 4.5 0 4.5 ...
##  $ r6         : num  24.5 14.5 24.5 0 4.5 14.5 24.5 14.5 0 44.5 ...
##  $ r7         : num  24.5 14.5 0 0 0 4.5 14.5 14.5 34.5 14.5 ...
##  $ r8         : num  14.5 4.5 44.5 0 0 4.5 4.5 14.5 14.5 4.5 ...
##  $ r9         : num  4.5 4.5 24.5 34.5 0 14.5 4.5 4.5 4.5 4.5 ...
##  $ r10        : num  4.5 14.5 24.5 45.5 24.5 14.5 14.5 14.5 24.5 4.5 ...
##  $ mpaa       : Factor w/ 5 levels "","NC-17","PG",..: 1 1 1 1 1 1 5 1 1 1 ...
##  $ Action     : int  0 0 0 0 0 0 1 0 0 0 ...
##  $ Animation  : int  0 0 1 0 0 0 0 0 0 0 ...
##  $ Comedy     : int  1 1 0 1 0 0 0 0 0 0 ...
##  $ Drama      : int  1 0 0 0 0 1 1 0 1 0 ...
##  $ Documentary: int  0 0 0 0 0 0 0 1 0 0 ...
##  $ Romance    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Short      : int  0 0 1 0 0 0 0 1 0 0 ...
```

```
##                      title year length budget rating votes   r1   r2  r3
## 1                        $ 1971    121     NA    6.4   348  4.5  4.5 4.5
## 2        $1000 a Touchdown 1939     71     NA    6.0    20  0.0 14.5 4.5
## 3   $21 a Day Once a Month 1941      7     NA    8.2     5  0.0  0.0 0.0
## 4                  $40,000 1996     70     NA    8.2     6 14.5  0.0 0.0
## 5 $50,000 Climax Show, The 1975     71     NA    3.4    17 24.5  4.5 0.0
## 6                    $pent 2000     91     NA    4.3    45  4.5  4.5 4.5
##     r4   r5   r6   r7   r8   r9  r10 mpaa Action Animation Comedy Drama
## 1  4.5 14.5 24.5 24.5 14.5  4.5  4.5           0         0      1     1
## 2 24.5 14.5 14.5 14.5  4.5  4.5 14.5           0         0      1     0
## 3  0.0  0.0 24.5  0.0 44.5 24.5 24.5           0         1      0     0
## 4  0.0  0.0  0.0  0.0  0.0 34.5 45.5           0         0      1     0
## 5 14.5 14.5  4.5  0.0  0.0  0.0 24.5           0         0      0     0
## 6 14.5 14.5 14.5  4.5  4.5 14.5 14.5           0         0      0     1
##   Documentary Romance Short
## 1           0       0     0
## 2           0       0     0
## 3           0       0     1
## 4           0       0     0
## 5           0       0     0
## 6           0       0     0
```

The variables that we are interested in are: 

- Section (Section 1, Section 2)
- Vector/variable Accuracy (ratio: 0-24) 


2.2. We should compare whether the data for Accruracy is distributed normally using `shapiro.test()`. However, obtaining a subset of the data from a single variable depending on whether respondents are in Section 1 or Section 2. 

Without subseting we basically have:






















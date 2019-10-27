# Homework 1
## Introduction
Since the Bootstrapping is introduced during the class, I have decided to run a simple Bootstrap testing to compare wages between male and female to determine the wage inequality issues between male and female.<br/>
The data file employment.csv has been used in this test and it is downloaded from the moodle site provided.<br/>
Data Analysis has been analysed using R Studio.<br/>
This study is focusing on wage inequality since the relationship between wage and education has been proved long time ago.

## Data Description
* Sex - (1 = Male, 2 = Female)
* Wage - Wages of the individual
* Education - Years of education
* Sector - Working sector
## Data Analysis
**Data Importing and Exploration**<br/>
Set directory and Import the Data files named as "work".<br/>
Display the data set imported as follow.<br/>
``` javascript
setwd("C:/Users/Kai Lim/Desktop/R Data File")
work <- read.csv(file="employment.csv",header=TRUE,sep=",")
View(work)
names(work) #Check the variables available in data set
class(work$sex) #check the class of the variable "sex"
table(work$sex) #show the data count of male and female while 1=male and 2=female
with(work,tapply(wage,sex,mean)) #check the mean wage of male and female
      1        2 
59299.41 44537.57 
#create a boxplot to show the summarized data
boxplot(work$wage~work$sex,las=1,ylab="Wage",xlab="Gender",main="Wage by Gender")
#create a scatterplot to illustrate the relationship between wage and education
plot(work$wage~work$education, main="Scatterplot of Wages and Education", 
     xlab="Education",ylab="Wages",pch=19)
abline(lm(work$wage~work$education),col="red")
lines(lowess(work$education,work$wage),col="blue")
```
The relationship between education and wage has been illustrated using scatterplot below.<br/>
**Illustration of Scatterplot**<br/>
![Image of Scatterplot](https://github.com/ominousthoo/statistic/blob/Data-files/Rplot01.png)<br/>
From the Scatterplot above, it shows us that the wage is always having a positive relationship with education.<br/>
Along with the scatterplot, a linear model fitted line and lowess fitted line has been included and both are showing a similar result.<br/>
**Boxplot Analysis**<br/>
![Image of Boxplot](https://github.com/ominousthoo/statistic/blob/Data-files/Rplot.png)<br/>
From the boxplot above we knew that there is quite a number of outliers in both gender which will be going to influence the mean to be bias.<br/>
Therefore, Median approach has been added to compare the difference.<br/>
``` javascript
with(work,tapply(wage,sex,median))
 1        2 
40043.86 31165.67 
```
Create the Test-stat 1 & 2 and run a Two-Sample t-test<br/>
Test-stat 1 for mean difference and Test-stat 2 for median difference.<br/>
```javascript
#absolute difference has been assigned to test_stat1 for mean difference
#absolute difference has been assigned to test_stat2 for median difference
test_stat1 <- abs(mean(work$wage[work$sex=="1"])-mean(work$wage[work$sex=="2"]))
test_stat2 <- abs(median(work$wage[work$sex=="1"])-median(work$wage[work$sex=="2"]))
#run a two sample t-test
t.test(work$wage~work$sex,paired=F,var.eq=F)
```
**Result of Two-Sample t-test**<br/>
```
Welch Two Sample t-test

data:  work$wage by work$sex
t = 6.2725, df = 1889.6, p-value = 4.394e-10
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 10146.23 19377.46
sample estimates:
mean in group 1 mean in group 2 
       59299.41        44537.57 
```
**Bootstrapping Process**<br/>
Bootstrap has been ran using the R-code below.<br/>
```javascript
set.seed(112233) #use set seed for reproducibility
n <- length(work$sex) #the number of observation to sample
n #check the number of observation to sample
B <- 20000 #number of bootstrap sampling
variable <- work$wage #the variable that resample from

#create the bootstrapping sample with matrix
BootstrapS <- matrix(sample(variable,size=n*B,replace=TRUE),
                     nrow=n,ncol=B)
dim(BootstrapS) #check the dimension of matrix
```
Lets create the vector to store the Bootstrap Test-stat<br/>
Calculate boot_test_stat 1 and 2 and save the mean and median difference<br/>
boot_test_stat1 for mean difference and boot_test_stat2 for median difference<br/>
```javascript
Boot_test_stat1 <- rep(0,B)
Boot_test_stat2 <- rep(0,B)
for (i in 1:B){
  Boot_test_stat1[i] <- abs(mean(BootstrapS[1:994,i])-
                              mean(BootstrapS[995:2000,i]))
  Boot_test_stat2[i] <- abs(median(BootstrapS[1:994,i])-
                              median(BootstrapS[995:2000,i]))
}
```
Check the outcome of bootstrap in the first column<br/>
Compare the first 25 outcome of bootstrap with the test-stat<br/>
The p-value of the bootstrapping outcome has been printed.<br/>
```javascript
round(Boot_test_stat1[1:25],1)
round(Boot_test_stat2[1:25],1)
#TRUE will be presented if the outcome of bootstrap is bigger than test-stat
(Boot_test_stat1 >= test_stat1)[1:25]
(Boot_test_stat2 >= test_stat2)[1:25]
#check the p-value or the probability that bootstrapping result is biggher than test-stat
P1 <- mean(Boot_test_stat1 >= test_stat1)
P2 <- mean(Boot_test_stat2 >= test_stat2)
P1;P2
 P1       P2 
 0        0 
```
According to the result, both mean is equal to zero.<br/>
Out of the 40000 bootstrapping outcome of mean and median difference.<br/>
None of them are equal or bigger than the test-stat.<br/>
This result is quite unusual since the test-stat of mean and median difference seems huge and significant.<br/>
Thus, another check has been ran to find out the reason.<br/>
Another code has been used to check the standard deviation of wage for male and female.<br/>
```javascript
with(work,tapply(wage, sex, sd)) 
      1        2 
58271.75 46209.52 
```
Since the standard deviation of wage is very huge for both gender, the result of bootstrapping becomes reasonable.<br/>
Thus,H0 is failed to reject.<br/>
The Wage inequality issues does not exsist between male and female since the mean and meadian difference between wage of male and female is statistically equal to zero.<br/>
<br/>
### Reference
*MarinStatsLecutres - Introduction to Hypothesis Testing in R*<br/>
*GitHub Help - Basic writing and formatting syntax*<br/>
*DataCamp - Quick R-Tutorial*<br/>

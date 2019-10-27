# Homework 1
## Introduction
Since the Bootstrapping is introduced during the class, I have decided to run a simple Bootstrap testing to compare wages between male and female to determine the wage inequality issues between male and female.<br/>
The data file employment.csv has been used in this test and it is downloaded from the moodle site provided.<br/>
Data Analysis has been analysed using R Studio.

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
#create a boxplot to show the summarized data
boxplot(work$wage~work$sex,las=1,ylab="Wage",xlab="Gender",main="Wage by Gender")
```
![Image of Boxplot](https://github.com/ominousthoo/statistic/blob/Data-files/Rplot.png)<br/>
From the boxplot we knew that that is quite a number of outliers in both gender.<br/>
Median approach has been added to compare the difference.<br/>
``` javascript
with(work,tapply(wage,sex,median))
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
```
According to the result, both mean is equal to zero<br/>
Out of the 40000 bootstrapping outcome of mean and median difference<br/>
None of them are equal or bigger than the test-stat<br/>
This result is quite unusual since the test-stat of mean and median difference seems huge and significant<br/>
Thus, another check has been ran to find out the reason<br/>
Another code has been used to check the standard deviation of wage for male and female.<br/>
```javascript
with(work,tapply(wage, sex, sd)) 
      1        2 
58271.75 46209.52 
```

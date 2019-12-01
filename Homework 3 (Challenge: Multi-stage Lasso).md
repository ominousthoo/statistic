# Homework 3 (Challenge: Multi-stage Lasso and Post Selection Inference)
### Introduction
In the class we have learned about many new models for estimation and for the last week, we have recently focusing on learning and compare the differences between the basic OLS, Lasso, Multi-stage Lasso, Relaxo, Ridge Regression and so on. Thus, this homework will be going to illustrate the differences between Lasso, Relaxo, OLS regression and Ridge Regression by using a "true" model as a baseline to compare the differences. This homework will be mainly written in `R` code.<br/>Ridge regression and OLS are two pretty similar regression model which is estimating the estimator for each variables but for the Lasso and Relaxo are much different from the two approaches mentioned above. Lasso and Relaxo basically are a shrinkage method which will be shrinking the coefficients of each variables to zero according to the degree called lambda. Thus, at a certain degree of lambda, only significant coefficients will be left and the other insignificant or less significant coefficients will be reduced to zero. At a very high degree of lambda, all coefficients will eventually shrinked to zero. Therefore, the "best lambda" has to be used in order to generate the most significant result and this can be easily illustrated using plot.
### Simulation
In this simulation, I will apply the four approaches which are mentioned above on the same "true" model in order to compare the different outcome of each approach. OLS, Ridge Regression, Lasso and Relaxo will be tested accordingly. The simulation will be running on a sample size of 200. In order to simulate the different approach, each sample come with 120 variables while 7 out of the 120 variables will be non-zero coefficients and the remaining 113 variables will be zero. A matrix `X` has been created in order to store the coefficients generated. The 7 non-zero variables will be 4, -3, -5, 5, 7, -1 and 10. The `R` codes for generating the "true" model and sample size is as follow.</br>
``` r
n <- 200 #sample size
v <- 120 #number of variables
t <- 7 #number of non-zero coefficients
set.seed(123)
X <- matrix(rnorm(n*v),ncol=v)
#coefficients of first 7 variables is non-zero, remaining are zero
beta <- c(4,-3,-5,5,7,-1,10,rep(0,v-t))
#use matrix function
set.seed(311) #for reprodcible data set
#create the y variables accordingly to X while rnorm(n) will
#be the error which didnt captured in the model with normal distribution mean=0 and standard deviation=1.
Y <- X%*%beta + rnorm(n) 
```
After generating the sample data set and coefficients, we have to test the performance of standard OLS by using the packages `lmtest`. At this section, an OLS regression will be ran for y on x and we will find out how many coefficients will be non-zero and statistically significant eventually.
``` r
require(lmtest) #to use the lmtest package for OLS test
ols <- lm(Y ~ X-1) #run a fitted OLS regression
coeftest(ols) #to check the significance of each variable
sum(ols$coefficients!=0) #count the number of coefficients in OLS which is non-zero
pvalue_ols <- summary(ols)$coefficients[,4]
#count the number of coefficients in OLS which is statistically significant
sum(pvalue_ols<0.05) 
```
After running the OLS regression and testing all the coefficients, we found that all 120 variables are non-zero coefficients while in fact only 7 of them are non-zero and the remaining 113 variables are zero. According to the coefficients test, out of the 120 non-zero variables only 11 variables is statistically different from zero at significance level 0.05. The result of first 15 variables has been attached below. According to the result below we can see that other than the first 7 variables, although the other variables are non-zero coefficients but they are very close to zero at some point. Thus, the OLS regression is able to reduce the variables from 120 variables to 11 significant variables while actually only 7 of them are solid.
``` r
       Estimate Std. Error  t value
X1    3.8620576  0.1268897  30.4363
X2   -2.7274413  0.1096452 -24.8752
X3   -4.9574310  0.1081125 -45.8544
X4    5.0475608  0.1028252  49.0887
X5    6.9355130  0.1074771  64.5302
X6   -1.0640286  0.1139968  -9.3338
X7    9.7924911  0.1201840  81.4792
X8    0.1015512  0.1149194   0.8837
X9    0.1896257  0.0996244   1.9034
X10  -0.1259855  0.1106746  -1.1383
X11   0.1077594  0.1175874   0.9164
X12   0.1289521  0.1189182   1.0844
X13   0.1763625  0.1118386   1.5769
X14   0.1253173  0.1115902   1.1230
X15   0.0072114  0.1128145   0.0639
```
After going through the OLS regression, now the Ridge Regression will be implemented in order to compare the differences between OLS regression and Lasso. Ridge regression basically is same as Lasso while the only difference between Ridge regression and Lasso is Ridge regression is using alpha level = 0 and Lasso is using Alpha level = 1. The `R` codes below is used to generete a Ridge regression based on the same data set.
``` r
require(glmnet) #require glmnet package for Ridge regression
cv_ridgereg <- cv.glmnet(X,Y,intercept=FALSE,alpha=0)
brlambda <- cv_ridgereg$lambda.min #find the best ridge lambda for regression
#run Ridge regression by using the optimal lambda level
best_ridgereg <- glmnet(X,Y,intercept = FALSE,alpha = 0, lambda = brlambda)
coef(best_ridgereg) #check the coefficients generated by Ridge regression
#generate Ridge regression by using all lambda
ridgereg <- glmnet(X,Y,intercept = FALSE,alpha = 0)
#plot Ridge regression according to different lambda
plot(ridgereg,xvar = "norm")
plot(cv_ridgereg)
sum(best_ridgereg$beta!=0) #count the non-zero coefficients in Ridge regression
print(best_ridgereg$beta) #print the coefficients result
```
The plots below will be able to shows that how the optimal lambda has been chose in order to generate the best and the most efficient result. Through the graph below we can know that the optimal lambda for this Ridge regression is very small and close to 1 as the Mean Squared Error is increasing along the lambda increase. </br>
![Images of ridgemse](https://github.com/ominousthoo/statistic/blob/Data-files/ridgemse.png)
![Images of ridgelog](https://github.com/ominousthoo/statistic/blob/Data-files/ridgelog.png)</br>
The result of first 15 coefficients has been generated in order to interpret the result of Ridge regression. According to the result of Ridge regression, all 120 coefficients are non-zero coefficients at the level of optimal lambda chosen but in fact, only first 7 coefficients is different from zero while the remaining coefficients are very close to zero. 
``` r
V1    3.2435054541
V2   -2.2208113828
V3   -4.4226560006
V4    4.4760726965
V5    6.0545460692
V6   -0.8434921081
V7    8.4638725364
V8   -0.0372235285
V9    0.1139310728
V10  -0.1622707338
V11   0.1151671847
V12   0.1232439405
V13   0.5618809759
V14   0.0356220250
V15   0.3496986783
```
After going through Ridge regression and Ordinary Least Square Regression, now we have to implement the Lasso approach on the same data set in order to compare the difference of using Lasso method. Lasso approach is one of the shrinkage method which will continously shrink the coefficients towards zero and by using a suitable level of lambda, the significant coefficients will be remained while the other insignificant coefficients will be shirinked to zero. The `R` codes to perform the Lasso approach will be attached as follow.<br/>
``` r
require(glmnet) #use glmnet package
cv_lasso <- cv.glmnet(X,Y,intercept=FALSE,alpha=1)
blambda <- cv_lasso$lambda.min #get the best fitted lambda
plot(cv_lasso) #illustrate the choosing of the best fitted lambda
best_lasso <- glmnet(X,Y,intercept = FALSE,alpha=1,lambda = blambda)
sum(best_lasso$beta!=0) #count the coefficients which is non-zero while using best-fitted lambda
all_lasso <- glmnet(X,Y,intercept = FALSE,alpha = 1)
plot(all_lasso,xvar="lambda") #plot the graph of Lasso at different Lambda level
```
The way of best fitted lambda has been chosen can be illustrated by the graph below. The best fitted lambda will be able to produce the result of lowest possible Mean Squared Error. Since the Mean Squared Error grows along with the lambda value increase, the best fitted lambda for lasso is extreamly small which is much smaller than the lambda value in Ridge regression. The second figure below shows us that Lasso is actually shrinking the coefficients toward zero as the degree of lambda increase. According to the figure, while the value of logged lambda is higher than 2, all coefficients has shrinked become zero.<br/>
![Image of lassomse](https://github.com/ominousthoo/statistic/blob/Data-files/lassomse.png)
![Image of lassolog](https://github.com/ominousthoo/statistic/blob/Data-files/lassolog.png)<br/>
Part of the outcome of Lasso has been printed below in order to illustrate and interpret the results of Lasso. According to the result of Lasso, there are total 25 coefficients be non-zero coefficients. While looking at the result of Lasso below, there are only the first 7 coefficients to be meaningful and the remaining 18 coefficients is actually very close to zero. Compare to the result of OLS regression above, Lasso has included more insignificant coefficients in the model.
``` r
V1    3.884394157   V33  -0.017149938
V2   -2.733646825   V41  -0.025272475
V3   -4.938060047   V42   0.067656623
V4    4.966772956   V56  -0.033611169
V5    6.881507472   V60   0.081194799
V6   -0.906736079   V66   0.079805157
V7    9.780769814   V72   0.035778280
V10  -0.029630620   V75  -0.094726723
V12   0.018164826   V78   0.026165494
V13   0.031295647   V111 -0.063621038
V14   0.072265464   V113  0.038360805
V20   0.006142708   V117  0.046865409
V33  -0.017149938   V120 -0.005737009
```
After we have completed the Lasso test, we have knew that Lasso test is not really efficient in this case and it perform even worse than OLS since OLS has a closer significant coefficients number to the real model while Lasso included more insignificant coefficients in this case. While the real model only include 7 significant coefficients, OLS founds 11 significant coefficients and Lasso has shirnked to 25 coefficients which is much higher than OLS estimation. Thus, Relaxo is neccessary to carry out in order to further decrease the number of significant coefficients. The `R` codes for relaxo implementation has been disclosed as follow. Compared to Lasso, Relaxo is much simplier.
``` r
require(relaxo) #use the relaxo package for relaxo
best_relaxo <- cvrelaxo(X,Y)
sum(best_relaxo$beta!=0) #count the coefficients which is not equal to zero
print(best_relaxo$beta) #display the outcome coefficients of relaxo
#outcome coefficients has printed as follow
V1  4.048841
V2  -2.90484
V3  -5.035838
V4  5.083147
V5  6.943908
V6  -1.028875
V7  9.927208
```
### Conclusion
According to the results of relaxo, the significant coefficients has been reduced to only 7 which is excatly the same as the "true" model and the value of each coefficients is close enough to the "true" coefficients which is 4, -3, -5, 5, 7, -1 and 10. Thus, from the simulation above it clearly shows us that the limitation of each kind of estimation method and under this circumtances, Relaxo is the best method to reduce and shrink the number of coefficients.<br/>
<br/>
<br/>

*Reference*<br/>
*Jiaming Mao - Model Selection and Regularization*<br/>
*GitHub Help - Basic Formatting and Writting Syntax*<br/>
*DataCamp - Quick R-tutorial*<br/>
*Nicome - The Data Blog*<br/>
<br/>
*You can access to my GitHub pages for other Homeworks [here](https://github.com/ominousthoo/statistic/tree/Homework).*

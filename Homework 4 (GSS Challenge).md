# Homework 4 (GSS Homework Challenge)
### Introduction & Background
Referring to the Homework Challenge given in the class, we are required to perform a high-dimensional analysis based on the [data](https://github.com/ominousthoo/statistic/blob/Data-files/Homework.sav) we collected from the United States: General Social Survey. In order to perform high-dimensional analysis by using different variable reduction regression, I have selected more than 50 variables including the target variable from GSS website. This analysis will be mainly performed using `Rstudio` and the `R` code will be attached along with the explaination and elaboration in each part of the analysis. In this study, I am going to compare the performance between three different measures which is LASSO, Ridge Regression and Elastic Net while using the ordinary least square regression (OLS) as baseline to compare the result between the three different measures mentioned above.</br>
I have selected the level of general happiness of repondents as the target variable in order to identify the influencial factors which will be able to affect the general happiness of publics. For the control variables, I have selected more than 50 variables which I think that it could possibly influence the general happiness level of an individual. For instance, I have selected Gender, Family Income, Number of Working Weeks per Year, Number of Children, Education Level, Age When FIrst Married, Number of Siblings et cetera. By including all these factors, I will possibly find out is that generally man are always happier than women, higher family income will increase the general happiness level or both of them are neither significant. After downloading the data from GSS, I have clean out the data and ommited missing values in order to improve the accuracy of the study since the data set includes plenty of ambigous and missing response such as the option Not Applicable, Don't Know or Not Sure. Other than that, I have transformed some of the categorical variables into different dummy variables in order to carry out the high-dimensional anaylsis by using SPSS since SPSS is having a better data visualization. 
### Analysis
At this part, I have imported the cleanned data into `Rstudio` and ommited those missing values which exist in the data set in order to improve the accuracy of the analysis. Thus, a linear regression will be implemented in order to compare the difference of result between the variables reduction methods.
``` r
library(haven) #import the .sav data
Homework2 <- read_sav("C:/Users/Kai Lim/Desktop/Homework2/Homework.sav")
View(Homework) #view and check the variables imported
names (Homework2) #check the variables in the data set
library(dplyr) #for omiting values
library(stargazer) #for presenting regression outcome
#omit the individual which is having missing responses
Homework <- Homework2 %>%
  na.omit(Homework)
dim(Homework) #double check the dimension of new data set
simreg <- lm(HAPPY ~ INCOME + NATDRUG + PRAY + HAPMAR + HEALTH + LIFE + SOCFREND + 
    SOCBAR + WEEKSWRK + PARTFULL + SATJOB + SATFIN + FINALTER + GOD + RINCOME + 
    HOMPOP + WRKSLF + AGEWED + SIBS + CHILDS + AGE + EDUC + PAEDUC + MAEDUC + SEX + 
    race_w + race_b + born_y + reg_pro + reg_cat + reg_jew + reg_oth + racelive_y + 
    lowclass + workclass + upclass + statful + statpart + statretired + statrest + 
    stathouse + marr_m + marr_wi + marr_di, data = Homework)
```
After running OLS based on the equation above, the outcome of the analysis is attached as follow. According to the result below, it shows that all of the variables is basically not equal to zero but out of more than 50 variables, there is only 8 of them is statistically significant. I think that it is quite ideal in this case. The result below tell us that the race of Black people is generally having a lower level of happiness, happy marriage will increase general level of happiness and so on.
``` r
stargazer(simreg, type="text")
===============================================
                        Dependent variable:    
                    ---------------------------
                               HAPPY           
-----------------------------------------------                                         
HAPMAR                        0.309***         
                              (0.093)                        
                                               
LIFE                          0.426***         
                              (0.089)                          
                              
SATFIN                        0.243***         
                              (0.075)                                                                      
                                               
race_b                       -0.686**          
                              (0.339)          
                                                                
statpart                      0.682*           
                              (0.359)                   
                                               
statrest                      0.733*           
                              (0.422)          
               
marr_wi                      1.418***          
                              (0.424)          
                                               
marr_di                       0.658**          
                              (0.322)          
                                               
Constant                     4.347***          
                              (1.020)                            
-----------------------------------------------
Observations                    176            
R2                             0.508           
Adjusted R2                    0.352           
Residual Std. Error      0.505 (df = 133)      
F Statistic           3.263*** (df = 42; 133)  
===============================================
Note:               *p<0.1; **p<0.05; ***p<0.01
```
After OLS has been ran using the data set above, I have to run three different type of variables selection model which is LASSO, Ridge Regression and Elastic Net at once in order to compare the differences between them. In order to measure the performance of these three different models, I am using RMSE to measure their performance respectively and the model with lowest RMSE will be selected as the best model in this case. Thus, the `R` packages `glmnet` has been used to apply variable selection analysis. The plot of 3x2 generated below is to visualize the plot solution path and cross-validated MSE as function of λ.
``` r
require(glmnet)
yvars <- Homework$HAPPY #General Happiness Level as target variable
xvars <- model.matrix(HAPPY~.,Homework)[,-1] #remaining variables are controls
dim(xvars) #check the dimension of control variables
#for plotting variables reduction process
lasso.all <- glmnet(xvars,yvars,alpha=1)
ridge.all <- glmnet(xvars,yvars,alpha=0)
elnet.all <- glmnet(xvars,yvars,alpha=0.3)
#for plotting lambda selection based on MSE
cv.lasso <- cv.glmnet(xvars,yvars,alpha=1)
cv.ridge <- cv.glmnet(xvars,yvars,alpha=0)
cv.elnet <- cv.glmnet(xvars,yvars,alpha=0.3)
#plotting a 3x2 graph and compare the difference between
par(mfrow=c(3,2))
plot(lasso.all, xvar="lambda")
plot(cv.lasso, main="LASSO")
plot(ridge.all, xvar="lambda")
plot(cv.ridge, main="Ridge")
plot(elnet.all, xvar="lambda")
plot(cv.elnet, main="Elastic Net")
```
![Image of Graph](https://github.com/ominousthoo/statistic/blob/Data-files/plot_zoom_png.png) </br>
After we have calculated the best lambda for each variable reduction model, now we are able to build the fitted model for each different models and compare the difference between their outcome. In this case, I am using the minimum best lambda in order to include as much variables as I can into the model. The following part is to generate the fitted model of each measures using the best lambda selected according to the MSE and compare the difference between their outcome. </br>
``` r
#selecting best lambda using cross-validation
laslambda.best <- cv.lasso$lambda.min
rglambda.best <- cv.ridge$lambda.min
enlambda.best <- cv.elnet$lambda.min
#generate fitted model for each measures using best lambda selected
fit.lasso <- glmnet(xvars,yvars,alpha = 1, lambda = laslambda.best)
fit.ridge <- glmnet(xvars,yvars,alpha = 0, lambda = rglambda.best)
fit.elnet <- glmnet(xvars,yvars,alpha = 0.3, lambda = enlambda.best)
```
After generate the fitted model, now we can go through and compare the difference between their outcomes. By comparing the result below, it clearly shows that the coefficients between these three different model are very close to each other and the only difference is LASSO included one less variable and Ridge Regression has included almost all variables but most of them are very close to zero. (Only non-zero coefficient is presented for LASSO and Elastic Net) The positive coefficient of the variables HEALTH, LIFE and SATFIN indicates that if the individual are having better health condition, more optimistic about their living life and higher satisfaction about their financial situation will tend to have a higher level of general happiness.
``` r
coef(fit.lasso)
coef(fit.ridge)
coef(fit.elnet)

                LASSO         Elastic Net     Ridge Reg
(Intercept)  3.39783235       3.270952555     3.28120801
HAPMAR       0.09053336       0.078813765     0.09372250
HEALTH       0.04460083       0.044455352     0.07179605
LIFE         0.37936792       0.324614117     0.32076201
SATJOB      ...               0.007601173     0.04545480
SATFIN       0.16745213       0.148674985     0.15066101
race_b      -0.09487760      -0.087955545    -0.20502712
marr_wi      0.28262370       0.236947460     0.45473281
marr_sep    -0.12798803      -0.141350369    -0.38875491
```
Moving to the final stage which is comparing the RMSE value between these three diffent models, RMSE will be the performance indicator in this case which means that the model with lower RMSE value will be the best model for analysing this study. The result will be presented using the package `stargazer`.
``` r
predict_lasso = predict(fit.lasso,newx = xvars) # predict lasso
RMSE_lasso = sqrt(mean((predict_lasso - yvars)^2)) #calculate RMSE
predict_ridge = predict(fit.ridge,newx = xvars)
RMSE_ridge = sqrt(mean((predict_ridge - yvars)^2))
predict_elnet = predict(fit.elnet,newx = xvars)
RMSE_elnet = sqrt(mean((predict_elnet - yvars)^2))
RMSE <- c(RMSE_lasso, RMSE_ridge, RMSE_elnet)
Type <- c("LASSO", "Ridge", "Elastic Net")
#grouping the RMSE value accordingly
RMSE1 <- data.frame(Type,RMSE)
#display the result
stargazer(RMSE1, type = "text", summary = FALSE, rownames = TRUE,
          column.labels = c("RMSE"), title = 
            "Squared Root Mean Square Error")
            
 ##################           
 ## RMSE Result ###
 ##################
 
Squared Root Mean Square Error
===================
     Type     RMSE 
-------------------
1    LASSO    0.493
2    Ridge    0.475
3 Elastic Net 0.500
-------------------
```
### Conclusion
According to the outcome above we notice that across different methods we have used in the analysis, Ridge Regression is having the lowest RMSE value which means that Ridge Regression works better in analysing this case. The possible explaination is most of the variables I have selected are certainly contributing to the variation of the general happiness level of an individual but some of the influence are small until eliminated by variable shrinkage. Thus, I can conclude that in this case, by increasing the dimension of the model will be significantly reduce the mean squared error of the study. Therefore, Ridge Regression out-performed the other two measures.<br/>
<br/>
<br/>
*Reference* <br/>
*Jiaming Mao - Model Selection and Regularization*<br/>
*GitHub Help - Basic Formatting and Writting Syntax*<br/>
*DataCamp - Quick R-tutorial*<br/>
*Rpubs - RNotebook*<br/>
*LASSO,Ridge & Elastic Net - NCSU*<br/>
<br/>
*You can access to my GitHub pages for other Homeworks [here](https://github.com/ominousthoo/statistic/tree/Homework).*

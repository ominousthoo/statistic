## R-code for Final Project
``` r
rm(list = ls(all.names = TRUE))
library(haven) #import the .sav data
library(ggplot2)
library(ggpubr)
happy <- read_sav("C:/Users/Kai Lim/Desktop/Final_Project/Final.sav")
View(final)
names(final)
library(dplyr) #for omiting values
library(stargazer) #for presenting regression outcome
#omit the individual which is having missing responses
final <- happy %>%
  na.omit(final)
dim(final)
ggplot(final, aes(x=SELF==1, y=RINCOME)) + 
  geom_boxplot()
ggplot(final, aes(x=factor(SELF), y="Percentage", fill=factor(HAPPY)))+
  geom_bar(width=1, stat = "identity",position = "fill")
names(final)
simreg2 <- lm(RINCOME ~ NATDRUG + PRAY  + HEALTH  + CLASS
             + SATFIN + GOD + MYSKILLS + SATJOB + INCOME + SIBS
             + CHILDS + AGE + EDUC + LOCAL + WHITE + BLACK + MALE
             + SELF + FULLTIME + PARTIME + TEMPUN + UNEMPL + RETIRED
             + SCHOOL + MARRIED + WIDOW + DIVORCE + SEPERATED + SINGLE
             +SELFxEDUC + SELFxWHITE + SELFxBLACK + SELFxCLASS + SELFxAGE
             + SELFxSKILL + SELFxMALE, data = final)
stargazer(simreg,simreg2, type="text")
require(glmnet)
yvars1 <- final$HAPPY #General Happiness Level as target variable
xvars1 <- model.matrix(HAPPY~.,final)[,-1] #remaining variables are controls
yvars2 <- final$RINCOME #Income Level as target variable
xvars2 <- model.matrix(RINCOME~.,final)[,-1] #remaining variables are controls
lasso.all1 <- glmnet(xvars1,yvars1,alpha=1)
lasso.all2 <- glmnet(xvars2,yvars2,alpha=1)
par(mfrow=c(1,2))
plot(lasso.all1, xvar="lambda",main="HAPPY")
plot(lasso.all2, xvar="lambda",main="RINCOME")
cv.lasso1 <- cv.glmnet(xvars1,yvars1,alpha=1)
cv.lasso2 <- cv.glmnet(xvars2,yvars2,alpha=1)
plot(cv.lasso1, main="LASSO")
plot(cv.lasso2, main="LASSO")
lambda.best1 <- cv.lasso1$lambda.1se
lambda.best2 <- cv.lasso2$lambda.1se
fit.lasso1 <- glmnet(xvars1,yvars1,alpha = 1, lambda = lambda.best1)
fit.lasso2 <- glmnet(xvars2,yvars2,alpha = 1, lambda = lambda.best2)
coef(fit.lasso1)
coef(fit.lasso2)
```

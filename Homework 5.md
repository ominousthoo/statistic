# Homework 5: Comparison between SVM, Classification Tree, Regression Tree and Random Forest
### Introduction
During the class we have learned that classification and decision tree is one of the best method to predict a logistic estimation since classifiaction tree provide an estimation which is crystal clear and easy to interpret. But the main problem that exist in the classification tree measure is it will goes overfit very easily. Thus, prunning is a very important step in running classification tree estimation. In order to compare the performance of different classification method, I have analyze the [`Default.csv`](https://github.com/ominousthoo/statistic/blob/Data-files/Default.csv) data set by using SVM Classification (Support Vector Machine), Classification Tree, Regression Tree and Random Forest in order to compare the performance and accuracy between different methods that have been selected in this model. Therefore, I have estimate the default rate according to the characteristic of 10000 individuals based on their balance, income and status which indicates whether they are student or not. This analysis will be conducted by using `R`.<br/>
### Data Analysis
The first step of the analysis is to import the data into `R studio` and call the neccessary package which is needed in the later part.<br/>
``` r
#set working directory
setwd("C:/Users/Kai Lim/Desktop/R Data File")
#import csv data files
mydata <- read.csv(file = "Default.csv",header = TRUE)
#call neccessary library
library(car)
library(ggplot2)
library(rpart) #build tree
library(rpart.plot) #plot tree
library(randomForest) #Random Forest
library(e1071) #SVM
library(caret)
#visualize the raw data
View(mydata)
#check data
str(mydata)
```
After importing the data set into `R studio`, now we are able to have a glance on the organized data by using the `R` codes below. [`Default.csv`](https://github.com/ominousthoo/statistic/blob/Data-files/Default.csv) consist of 4 variables which is Default, Income, Balance and Student. The variables Default and Student are factor variables which indicates whether the individual is a student and declared as default or not while the variables Income and Balance are continous variables which reflects the balance and income of the respective individual. 
``` r
'data.frame':	10000 obs. of  4 variables:
 $ default: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
 $ student: Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 2 1 1 ...
 $ balance: num  730 817 1074 529 786 ...
 $ income : num  44362 12106 31767 35704 38463 ...
#preliminart data visualization
par(mfrow=c(2,2))
barplot(prop.table(table(mydata$default)),xlab = "Default",ylab = "Percentage")
hist(mydata$balance, xlab = "Balance", main = "Histogram of Balance")
barplot(prop.table(table(mydata$student)),xlab = "Student",ylab = "Percentage")
hist(mydata$income, xlab = "Income", main = "Histogram of Income")
```
![imageofmainhist](https://github.com/ominousthoo/statistic/blob/Data-files/mainhist.png) <br/>
According to the barchart and histogram above, we are able to notice that there are only small size of the observation collected are actually default and most of the individual (more than 70%) are not student. The histogram above also shows that most of the balance of the observation are less than 1500 and this has makes the distribution of balance slightly skewed. After having a glance on the preliminary statistic, the data has been further present by using scatter plot in order to visualize the distribution of the data according to the variables Income, Balanace, Student Status and Default. The scatter plot diagram will be formed by using the codes below.
``` r
par(mfrow=c(1,1))
ggplot(mydata, aes(x = balance, y = income, shape=factor(student))) +
  geom_point(aes(color = factor(default))) + scale_shape_manual(values=c(1, 2))
```
![imageofscatterplot](https://github.com/ominousthoo/statistic/blob/Data-files/scatter.png)
The scatter plot above has well presented the distribution of the data according to the variables such as the scatter plot has clearly shows that observation with relatively higher balance will be more likely to be default compared to the observation with lower balance. The default rate is not significantly different between student and not student based on the scatter plot above which means that balance has played a main role in affecting the default rate. Thus, the impact of income level is not significant as well according to the scatter plot above.<br/>
#### Support Vector Machine Classification (SVM)
Firstly, the SVM classification will be applied to analyse the default rate and further compare between different classification measures in order to identify which classification methods will be having the lowest error rate. The SVM classification has been implemented as below and the classification result is able to illustrate graphically.
``` r
#svm
svm.fit <- svm(default ~., data=mydata, kernel='linear', cost=10, scale=FALSE)
#plot classification boundary
plot(svm.fit,mydata, income~balance,slice=list(income=3,balance=4))
print(svm.fit)
#check accuracy
table(predict(svm.fit), mydata$default, dnn=c("Prediction", "Actual"))
confusionMatrix(mydata$default, predict(svm.fit))
```
![imageofsvm](https://github.com/ominousthoo/statistic/blob/Data-files/SVM%20Classification.png)
``` r
Confusion Matrix and Statistics

          Reference
Prediction   No  Yes
       No  9614   53
       Yes  268   65
                                          
               Accuracy : 0.9679          
                 95% CI : (0.9643, 0.9713)
    No Information Rate : 0.9882      
    P-Value [Acc > NIR] : 1               
```
The scatter plot and the linear classification boundaries of the SVM above has stated that only a small part at the bottom right side of the graph has been predicted to be default according to the data set. Referring to the result and the confusion matrix of SVM, we can see that the accuracy of Support Vector Machine (SVM) is having an accuracy up to 96.79% which means that the prediction area are succesfully catch 96.79% of the default status correctly and the remaining 3.21% is the misclassification rate. <br/>
#### Classification Tree
Moving to the next classification method which is Classification Tree, Classification Tree is so different from the SVM estimation above since CLassification Tree is growing a tree accodingly to the variables and prune the tree afterwards. Classification tree is having an advantages of intepreting the result and includes the interaction effects automatically. Thus, classification tree will be implememted by the codes below. <br/>
``` r
#classification tree
set.seed(1234) #for reproducible
fit0 = rpart(default~ student + balance + income,mydata,method = "class", control=rpart.control(cp=0))
fit = prune(fit0,cp=fit0$cptable[which.min(fit0$cptable[,"xerror"]),"CP"])
par(mfrow=c(1,2))
rsq.rpart(fit)
rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE) #plotting the tree
plotcp(fit) #cpplot
print(fit)
printcp(fit) #print cp result
summary(fit)
```
![imageofcpclassification](https://github.com/ominousthoo/statistic/blob/Data-files/cpplotclass.png)<br/>
![imageofclassificationrsq](https://github.com/ominousthoo/statistic/blob/Data-files/classificationrsq.png)<br/>
The image above able to shows that as the number of splits increase, up to 3 splits, the x relative errors has been reduced to minimum level as well as the R-squared value increase along the split increase. Therefore, the graphs above are able to tell us that 3 splitting of the classification tree might be the best solution in this case. Thus, the classification tree and the results has been produced as follow.<br/>
![imageofclassificationtree](https://github.com/ominousthoo/statistic/blob/Data-files/classifi.png)<br/>
``` r
Call:
rpart(formula = default ~ student + balance + income, data = mydata, 
    method = "class", control = rpart.control(cp = 0))
  n= 10000 

           CP nsplit rel error    xerror       xstd
1 0.108108108      0 1.0000000 1.0000000 0.05387952
2 0.078078078      1 0.8918919 0.9219219 0.05180290
3 0.036036036      2 0.8138138 0.8348348 0.04936919
4 0.007507508      3 0.7777778 0.8078078 0.04858594

Variable importance
balance  income student 
     94       3       3 
set.seed(123)
yhat = predict(fit,mydata,type="class") 
err = 1-mean(mydata$default==yhat)
accuracy <- 1-err
accuracy #accuracy of the model
accuracy = 0.9741
```
The result of classification tree above has supported the statement I have made above regarding the scatter plot above which is the default rate is mainly decided by the level of balance. The result above has stated that balance is having the highest value of variable importance which is 94 has declare that the variable balance is the most influencial variables in this estimation model. The classification tree above has indicates that if the balance of the observation is greater than 1800, there is 97% of chance it would turn default. After calculate the error rate by using the prediction and yhat, we can conclude that classification tree model is having an estimating miscalssification rate of 2.59% or 97.41% accuracy which is performing much better than the previous SVM classification.
#### Regression Tree
The regression tree classification is very similar to the classification tree above and the only difference between regression tree and classification tree is the method using in the `rpart` which means that the classification is using `method = class` while regression tree model is using `method = anova`. Thus, the regression tree model will be implemented by using the codes below and the result will be used to compared with the other methods in order to identify the most suitable estimation model in this case. 
``` r 
#regression tree
set.seed(123) #for reproducible
fit.r0 = rpart(default~ student + balance + income,mydata, method = "anova",control=rpart.control(cp=0))
fit.r = prune(fit.r0,cp=fit0$cptable[which.min(fit0$cptable[,"xerror"]),"CP"])
par(mfrow=c(1,2))
rsq.rpart(fit.r) #display rsquare and error rate
par(mfrow=c(1,1))
rpart.plot(fit.r, box.palette="RdBu", shadow.col="gray", nn=TRUE) #plot regression tree
plotcp(fit.r) #plot cp
print(fit.r)
printcp(fit.r)
summary(fit.r) #show summary
```
![imageofcpreg](https://github.com/ominousthoo/statistic/blob/Data-files/cpplotregtree.png)<br/>
![imageofregrsq](https://github.com/ominousthoo/statistic/blob/Data-files/regtreersq.png)<br/>
Same as the classification tree model, the image above shows that as the number of splits increase, up to 3 splits, the x relative errors has been reduced to minimum level as well as the R-squared value increase along the split increase. Therefore, the graphs above are able to tell us that 3 splitting of the classification tree might be the best solution in this case. Thus, the regression tree and the results has been produced as follow.<br/>
![imageofregtree](https://github.com/ominousthoo/statistic/blob/Data-files/regtree.png)<br/>
``` r
Call:
rpart(formula = default ~ student + balance + income, data = mydata, 
    method = "anova", control = rpart.control(cp = 0))
  n= 10000 

           CP nsplit rel error    xerror       xstd
1 0.257980888      0 1.0000000 1.0005174 0.05204918
2 0.042294290      1 0.7420191 0.7490060 0.04083954
3 0.024892542      2 0.6997248 0.7091704 0.03593281
4 0.009550804      3 0.6748323 0.6922766 0.03651729
5 0.007507508      4 0.6652815 0.6911645 0.03691698

Variable importance
balance  income student 
     95       3       2 
set.seed(123)
yhat.r = predict(fit.r,mydata,type="class") 
err.r = 1-mean(mydata$default==yhat.r)
accuracy.r <- 1-err.r
accuracy.r
accuracy.r = 0.9741
```
According to the regression tree above, the tree plot shows the difference between regression tree and classification tree which the regression tree includes the income in the tree model while classification tree only includes balance as control variables. The regression tree above tells a similar result as classification tree which is if the balance of the observation is more than 1800, there is 97% of chances that the observation will be default. Refers to the summary of regression tree model, the weight of balance in variable importance become even larger as 95, compare to 94 in classification tree model. for the accuracy of regression tree model, it stays the same as classification tree which is having 97.41% of accuracy and only having 2.59% of misclassification rate. 
#### Random Forest
Moving to the final part of the analysis, now we have to analyse the data by using Random Forest which means that we are going to generate a bunch of trees randomly and group all of them together and this implementation gave the name of Forest to the model. Thus, the Random Forest estimation will be commited using the codes below. 
``` r
#random forest
fit.rf <- randomForest(default ~ student + balance + income, data=mydata)
#view results
print(fit.rf)
#importance of each predictor
importance(fit.rf)
set.seed(123) #for reproducible
#calculate misclassification rate
yhat.rf = predict(fit.rf,mydata) 
err.rf <- 1-mean(mydata$default==yhat.rf)
err.rf
accuracy.rf <- 1-err.rf
accuracy.rf
table(yhat,mydata$default)
```
The result of Random Forest has been attacted as follow. The result below has shows us that same as the three different model above, balance is still the most important control variable in the model and the accuracy rate is increased a lot in random forest model to 97.58% which means that the misclassification rate of random forest is only 2.42%, which is much lower compare to any three model above.
``` r 
importance(fit.rf)
        MeanDecreaseGini
student         2.672152
balance       180.335510
income         30.502662

yhat.rf   No  Yes
    No  9646  221
    Yes   21  112
accuracy.rf = 0.9758
```
### Conclusion
By comparing the misclassification rate of all four different models we have applied above, we found that among the different classification model Random Forest is having the lowest misclassification rate which is 2.42% compared to the other model which means that we can conclude that among the four models we have applied, Random Forest performed best in this study.
``` r
Model <- c("Support Vector Machine","Classification Tree", "Regression Tree", "Random Forest")
MisclassificationRate <- c(err.svm, err, err.r, err.rf)
data.frame(Model,MisclassificationRate)
           Model               MisclassificationRate
1 Support Vector Machine                0.3210
2    Classification Tree                0.0259
3        Regression Tree                0.0259
4          Random Forest                0.0242
```
<br/>
<br/>

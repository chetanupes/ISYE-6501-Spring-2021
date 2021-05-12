install.packages("MASS")


library(ggplot2)
library(outliers)
library(caret)
library(corrplot)
library(MASS)
library(reshape2)

#Getting the training and test data
crime_data<-read.table('uscrime.txt', sep = "", header = TRUE )
test<-read.table('test.txt', sep="", header = TRUE)

max(crime_data$Crime)
#Exploratory data analysis and pre-processing

#We can explore the data for outlier using the strategies we learned earlier. 

#Scaling the data to fit the box plot in the plot
crime_scale<-scale(crime_data[,1:15])
out<-melt(crime_scale)

#Box plot to view the outliers
ggplot(out,aes(x=Var2, y=value))+geom_boxplot()

#Some of the predictors seems to have outliers. We cannot just simply remove 
#them. Thus, for this exercise we will keep the original dataset without 
#further investigating them

#Linear regression model  
model1<-lm(Crime~.,data = crime_data)          

summary(model1) # Model summary
pred<-predict(model1, test) #Prediction on test data

#Plotting model results
plot(model1)

#Linear regression model 1a using cross validation 
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
model1a<-train(Crime~.,data = crime_data, method='lm',  #Linear Model with CV
               trControl=train.control)          

summary(model1a) # Model summary
pred1<-predict(model1a, test) #Prediction on test data

#Plotting residual vs fitted data to make sure there is no trend in the errors
res<-resid(model1a)
plot(fitted(model1a), res)
abline(0,0) #Adding a horizontal line

#The residual plot shows no pattern confirming that errors have no explanatory 
#power for the above model and this is what we want that the explanatory power
#of the model resides with the predictors

#Model 2

#Linear regression model 2 
model2<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob,data = crime_data)          

summary(model2) # Model summary
pred2<-predict(model2, test) #Prediction on test data

#Plotting model results
plot(model2)

#Linear regression model 2 using cross validation 
set.seed(123)
train2.control <- trainControl(method = "cv", number = 10)
model2a<-train(Crime~M+Ed+Po1+U2+Ineq+Prob,
               data = crime_data, method='lm',  #Linear Model2 with CV
               trControl=train.control)          

summary(model2a) # Model summary
pred2a<-predict(model2a, test) #Prediction on test data

#Plotting residual vs fitted data to make sure there is no trend in the errors
res2<-resid(model2a)
plot(fitted(model2a), res2)
abline(0,0) #Adding a horizontal line

#The residual plot shows no pattern confirming that errors have no explanatory 
#power for the above model and this is what we want that the explanatory power
#of the model resides with the predictors

#Model 3: Model transformation using log transformation on response

#Linear regression model 3 on full dataset using model1
model1_trans<-lm(log(Crime)~.,data = crime_data)

summary(model1_trans)
pre_trans<-predict(model1_trans, test)

#Converting prediction to the same scale
final<-exp(pre_trans)

#Plotting model results
plot(model1_tans)

#Linear regression model on smaller dataset
modela_trans<-lm(log(Crime)~M+Ed+Po1+U2+Ineq+Prob,data = crime_data) 

summary(modela_trans) # Model summary
pred_trans1<-predict(modela_trans, test) #Prediction on test data

#Converting prediction to the same scale
final1<-exp(pred_trans1)

#Plotting model results
plot(modela_trans)

step(model1)

##Homework 1##

#This files gives the  code related to the problem. Additional pdf will 
#made to explain the methodology and insigths to compliment the the code logic

##

#Installing necessary packages

install.packages("kernlab")


#importing the libraries
library(kernlab)
library(ggplot2)
library(dplyr)

#Getting the working directory
getwd()

#Reading the text file
credit_data<-read.table('credit_card_data-headers.txt', sep="", header = TRUE)

#Viewing the data
View(credit_data)

#Summarizing the data
str(credit_data)

#Defining the training features (predictors variables) and target lable (response variable)

x<-credit_data[,1:10]
y<-credit_data[,11]


#Plotting using two features to find some insights
#Multiple plots were drawn with the the present plot telling that features 4 and 5 could be most important as
#can separate the data nicely. Just an observation.

plot(x[,5],x[,4], col=as.factor(y))  


#Setting seed for reproducibility

set.seed(42)

#Creating SVM Model and the model summary to do find a range for C by trial and run

svm_model<-ksvm(x=as.matrix(x), y=as.factor(y),type="C-svc", kernel="vanilladot", C=2000, scaled=TRUE, cross=5)
print(svm_model)

#After doing several trial and run it was decided to test the following range of C (.0001 to 1)

#Predictions
pred<-predict(svm_model, x[,1:10])

#Percentage of model predictions matching actual classifications
sum(pred == y)/nrow(credit_data)

#Now our task is to find the best value of C, within the decided range of C and then using a loop to use them
#we will also collect the training errors for each C and thus decide on the best C

err<-list() #Creating an empty error list
regu<-as.list(seq(from = 0.0001, to = 1, by = 0.0005))  #C is the regularization term

#Now running a loop to evaluate the error and using different C
for (i in 1:length(regu)){
  
  set.seed(42)
  test_model<-ksvm(x=as.matrix(x), y=as.factor(y), type="C-svc",kernel="vanilladot", 
                   C=regu[[i]], scaled=TRUE, cross=5)
  err[[i]]=test_model@error

}

#Plotting using ggplot needs to put C and errot in a dataframe
dat<-data.frame(C=unlist(regu), Error=unlist(err))

#Plotting using ggplot
ggplot(data=dat,aes(x = C, y =Error)) + geom_point(alpha=0.5,color = "red")

#Sorting the dataframe based on least error and then using the corresponding C for the best model

dat_sort<-dat[order(dat$Error),]

best_c<-dat_sort[1,1]

#Model based on best C
best_model<-ksvm(x=as.matrix(x), y=as.factor(y), type="C-svc",kernel="vanilladot", 
                 C=best_c, scaled=TRUE, cross=5)

#Predictions
pred_new<-predict(best_model, x[,1:10])

#Percentage of model predictions matching actual classifications
sum(pred_new == y)/nrow(credit_data)

#Finding the coefficient and intercept
coff<-colSums(best_model@xmatrix[[1]]*best_model@coef[[1]])
intercept<-best_model@b

###############################################################################
#Using Radial Kernal
###############################################################################
err_non<-list()
regu_non<-as.list(seq(from = 0.5, to = 200, by = 0.5))  


#Now running a loop to evaluate the error and using different C
for (i in 1:length(regu_non)){
  
  set.seed(42)
  test_model<-ksvm(x=as.matrix(x), y=as.factor(y), type="C-svc",kernel="rbfdot", 
                   C=regu_non[[i]], scaled=TRUE, cross=5)
  err_non[[i]]=test_model@error
  
}

#To plot using ggplot C and error are put in a dataframe
dat_non<-data.frame(C=unlist(regu_non), Error=unlist(err_non))
ggplot(data=dat_non,aes(x = C, y =Error)) + geom_point(alpha=0.5,color = "red")

#Sorting the dataframe based on least error
dat_sort_non<-dat_non[order(dat_non$Error),]

best_non_c<-dat_sort_non[1,1]

#Model based on best C
best_model_non<-ksvm(x=as.matrix(x), y=as.factor(y), type="C-svc",kernel="rbfdot", 
                 C=best_non_c, scaled=TRUE, cross=5)

#Predictions
pred_non_new<-predict(best_model_non, x[,1:10])

#Percentage of model predictions matching actual classifications
sum(pred_non_new == y)/nrow(credit_data)

coff_non<-colSums(best_model_non@xmatrix[[1]]*best_model_non@coef[[1]])
intercept_non<-best_model_non@b
coff_non
intercept_non


#Question 3.1 (b)

###############################################################################
#Splitting data into training, Validation and Test
###############################################################################

#We will use the caret library to do the splitting
library(caret)
library(kknn)
library(ggplot2)

#Getting the data
credit_data<-read.table('credit_card_data-headers.txt', sep = "", header = TRUE)

#The data will be divided with 70% Train, 15% Validation and 15% Test data
set.seed(42)

#Using cresteDataPartion 
train_index<-createDataPartition(y=credit_data$R1, p=0.7, 
                                times = 1, list = FALSE)
#Training data
train_data<-credit_data[train_index,]

#Validation and testing data, this data is further divided into two sets
rest_data<-credit_data[-train_index,]
rest_index<-createDataPartition(y=rest_data$R1, p=0.5, times = 1,list = FALSE)

#Validation data
val_data<-rest_data[rest_index,]

#Testing data
test_data<-rest_data[-rest_index,]

#Using kkNN
acc<-list() #Creating an empty list to store accuracy

#Testing the kknn method for different k values (1-50)

for (i in 1:50){
  model_kknn<-kknn(R1~., train = train_data, test = val_data,
                   k=i, scale = TRUE)
  pred<-fitted(model_kknn)
  pre<-ifelse(pred>0.5,1,0)
  acc[[i]]=sum(pre==val_data[,11])/nrow(val_data)
}

#Choosing the best value of k from the validation accuracy. To this we put both
#accuracy and k in a data frame and plot it

score<-data.frame(k=list(1:50), Accuracy=unlist(acc))
ggplot(data = score, aes(x=X1.50, y=Accuracy))+geom_point(alpha=0.5,color='red')

#Finding the best model from the previous data frame
score_sort<-score[order(-score$Accuracy),]

message('Best k: ',score_sort[1,1])
message('Accuracy of the model using the best k: ', score_sort[1,2])

#Now testing this model  on the test data
model_test<-kknn(R1~.,train = train_data, test = test_data, k=score_sort[1,1],
                 scale = TRUE)
test_predict<-ifelse(fitted(model_test)>0.5,1,0)
test_accuracy<-sum(test_predict==test_data[,11])/nrow(test_data)

message('Accuracy of the model on the test using the best k:', test_accuracy)


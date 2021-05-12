#Question 3.1 (a) 

#Using the same data set (credit_card_data.txt or credit_card_data-headers.txt) 
#as in Question 2.2, use the ksvm or kknn function to find a good classifier:
#(a)	using cross-validation (do this for the k-nearest-neighbors model; SVM is 
#  optional); and
#(b)	splitting the data into training, validation, and test data sets 
#(pick either KNN or SVM; the other is optional).

###############################################################################
#Doing cross validation manually
###############################################################################
#Importing libraries 
library(kknn)
library(ggplot2)

#Reading the data
credit_data<-read.table('credit_card_data-headers.txt', sep = "", header = TRUE)

#Viewing the data
head(credit_data)

#Summarizing the data
str(credit_data)

#Setting the seed for reproducibility
set.seed(42)

#Shuffling the data randomly
credit_data<-credit_data[sample(nrow(credit_data)),]

#Creating 10 groups for cross validation. One can also user input to decide 
#on the number of cross validation groups. Will be more automated.
group <- cut(seq(1,nrow(credit_data)),breaks=10,labels=FALSE)

pre<-list() #Creating an empty list to store predictions
accu<-list() #Creating an empty list to store accuracy

for (j in 1:50){   #Loop to test different k between 1-50
  acc1=0           #Setting Accuracy to 0 for new value of k
  for (i in 1:10){ #Loop to test k-fold cross validation
    ind<-which(group==i, arr.ind = FALSE) #This create a set using the group
    model_knn = kknn(R1~.,
                     train=credit_data[-ind,], 
                     test=credit_data[ind,], 
                     k = j, 
                     scale = TRUE)
    pred_knn = fitted(model_knn)
    pre[[i]]<-ifelse(pred_knn>0.5,1,0)
    acc1=acc1+sum(pre[[i]] == credit_data[ind,11])/nrow(credit_data[ind,])
  }
  accu[[j]]=acc1/10 #Accuracy for the jth value of k
}

#Plotting k vs accuracy
dat<-data.frame(k=list(1:50), Accuracy=unlist(accu))
ggplot(data=dat,aes(x=X1.50,y=Accuracy))+geom_point(alpha=0.5,color='red')

#Sorting the dataframe to get best k

dat_sort<-dat[order(-dat$Accuracy),]
best_k<-dat_sort[1,1]

message('Best k: ',best_k)
message('Accuracy of the model using the best k: ', dat_sort[1,2])


#####Now trying the same thing using cv.kknn#####################

accu_new<-list() #Creating an empty list to store accuracy

#Model fit
for (m in 1:50){
  set.seed(42)
  kNN_fit<-cv.kknn(R1~.,data=credit_data, 
                   scale=TRUE, k = m, kcv = 10)
  pred<-ifelse(kNN_fit[[1]][,2]>0.5,1,0)
  accu_new[[m]]=sum(pred==credit_data[,11])/nrow(credit_data)
}

#Plotting k vs accuracy
dat_new<-data.frame(k=list(1:50), Accuracy=unlist(accu_new))
ggplot(data=dat_new,aes(x=X1.50,y=Accuracy))+geom_point(alpha=0.5,color='red')

#Sorting the dataframe to get best k

dat_sort_new<-dat_new[order(-dat_new$Accuracy),]
best_k_new<-dat_sort_new[1,1]

message('Best k: ',best_k_new)
message('Accuracy of the model using the best k: ', dat_sort_new[1,2])



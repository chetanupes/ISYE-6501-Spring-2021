###############################################################################
#kNN-k Nearest Neighbour
###############################################################################

#Installing kkNN package
install.packages("kknn")

library(kknn)

#Getting the working directory
getwd()

#Reading the text file
credit_data<-read.table('credit_card_data-headers.txt', sep="", header = TRUE)

#Viewing the data
head(credit_data)

#Summarizing the data
str(credit_data)

k<-as.list(seq(from = 1, to = 50, by =1))  


pre<-list()
accu<-list()

for (j in 1:length(k)){
  for (i in 1:nrow(credit_data)){
    model_knn = kknn(R1~.,
                     credit_data[-i,], 
                     credit_data[i,], 
                     k = j, 
                     scale = TRUE)
    pred_knn = fitted(model_knn)
    pre[[i]]<-ifelse(pred_knn>0.5,1,0)
  }
  accu[[j]]=sum(pre == credit_data[,11]) / nrow(credit_data)
}


#To plot using ggplot k and accuracy are put in a dataframe
dat<-data.frame(k=unlist(k), Accuracy=unlist(accu))
ggplot(data=dat,aes(x = k, y =Accuracy)) + geom_point(alpha=0.5,color = "red")

#Sorting the dataframe based on least error
dat_sort<-dat[order(-dat$Accuracy),]

best_k<-dat_sort[1,1]
message('Best k: ',best_k)
message('Accuracy of the model using the best k: ', dat_sort[1,2])




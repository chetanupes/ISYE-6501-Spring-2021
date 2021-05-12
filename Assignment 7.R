

library(ggplot2)
library(randomForest)
library(tree)

#Getting the training and test data
crime_data<-read.table('uscrime.txt', sep = "", header = TRUE)
test<-read.table('test.txt', sep="", header = TRUE)

#Now fitting an unpruned regression tree to the training data
lmr<-tree(Crime~., data=crime_data)
summary(lmr)

#Visualization
plot(lmr)
text(lmr, pretty = 0)
title(main = "Unpruned Regression Tree")

#predicting using the unpruned model
pred_un<-predict(lmr, crime_data[,1:15])

#Calculating R2 for the unpruned model
SSE<-sum((pred_un-crime_data[16])^2)
SST<-sum((crime_data$Crime-mean(crime_data$Crime))^2)
R2<-1-SSE/SST

#As with classification trees, we can use cross-validation to select a good 
#pruning of the tree.
set.seed(18)
lmr_cv = cv.tree(lmr)
summary(lmr_cv)
plot(lmr_cv$size,lmr_cv$dev, type = "b",
     xlab = "Tree Size", ylab = "CV-DEV")

#The plot shows deviation which is a measure of the errors is least with tree size 7 
#with 5 and 6 in close proximity in terms of their dev values. But the overall dev for 
#each node is much higher the unpruned model suggestive of overfitting.
#Now we try to prune using 5 and 6 tree size and see the R2

#Pruning the tree using tree size of 6
lmr_prune_6<-prune.tree(lmr, best = 6)
summary(lmr_prune_6)

#predicting with tree size 6
pre_6<-predict(lmr_prune_6, crime_data[,1:15])

#Calculating R2 for the unpruned model
SSE_cv<-sum((pre_6-crime_data[16])^2)
R2_6<-1-SSE_cv/SST

#Cross validating pruned tree of size 6
set.seed(18)
lmr_cv_6 = cv.tree(lmr_prune_6)
summary(lmr_cv_6)
plot(lmr_cv_6$size,lmr_cv_6$dev, type = "b",
     xlab = "Tree Size", ylab = "CV-DEV")

#Viewing the deviation for each tree for pruned tree
lmr_cv_6$size
lmr_cv_6$dev

#Now trying this for a tree size of 5
lmr_prune_5<-prune.tree(lmr, best = 5)
summary(lmr_prune_5)

#predicting with tree size 5
pre_5<-predict(lmr_prune_5, crime_data[,1:15])

#Calculating R2 for the unpruned model
SSE_cv<-sum((pre_5-crime_data[16])^2)
R2_5<-1-SSE_cv/SST

#Cross validating pruned tree of size 5
set.seed(18)
lmr_cv_5 = cv.tree(lmr_prune_5)
summary(lmr_cv_5)
plot(lmr_cv_5$size,lmr_cv_5$dev, type = "b",
     xlab = "Tree Size", ylab = "CV-DEV")

#Viewing the deviation for each tree for pruned tree
lmr_cv_5$size
lmr_cv_5$dev

###############################################################################

#Random forest

#RF model
rf<-randomForest(Crime~., data=crime_data)
summary(rf)
print(rf)

#predicting using RF model
pred_rf<-predict(rf)

#Calculating R2 for RF model
SSE_rf<-sum((pred_rf-crime_data[16])^2)
SST<-sum((crime_data$Crime-mean(crime_data$Crime))^2)
R2_rf<-1-SSE_rf/SST

#Running CV on RF Model and check the R2
rf_cv<-rfcv(trainx = crime_data[,1:15], trainy = crime_data$Crime, 
            cv.fold = 10)
print(rf_cv)
plot(rf_cv$error)

#predicting using RF model
pred_rf_cv<-rf_cv$predicted[1]

#Calculating R2 for RF CV model 
SSE_rf_cv1<-sum((pred_rf_cv-crime_data[16])^2)
SST<-sum((crime_data$Crime-mean(crime_data$Crime))^2)
R2_rf_cv<-1-SSE_rf_cv1/SST

###############################################################################
#install.packages('dummy')
#install.packages("fmsb")
#install.packages("e1071")
#install.packages('pROC')

library(reshape2)
library(dummy)
library(caret)
library(MASS)
library(car)
library(e1071)
library(pROC)

#Getting the training and test data
credit_data<-read.table('germancredit.txt', sep = "", header = TRUE)

summary(credit_data)

#Renaming the columns as per the description in the document
newnames<-c('Checking account Status','Duration in month',
            'Credit history','Purpose','Credit amount',
            'Savings account/bonds','Employment since',
            'Installment rate in % disposable income','Status& Sex',
            'Other debtors / guarantors','Residence since','Property',
            'Age in years','Other installment plans','Housing',
            'Number of existing credits at this bank',
            'Job','People being liable to provide maintenance for',
            'Telephone','foreign worker', 'Customer_Status')

colnames(credit_data)<-newnames

#Exploratory Data Analysis
#The document shows that we have 7 numeric and 13 categorical predictors
#Let's group the numerical variables to visualize the data

num<-scale(credit_data[,c(2,5,8,11,13,16,18)])
num<-melt(num)
#Box plot to view the outliers
ggplot(num,aes(x=Var2, y=value))+geom_boxplot()+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

#Credit amount show quite a large number of outliers but we not 
#remove them for this analysis.

#The next step is to create dummy variables for the categorical variables 
#using the dummy package
categorical<-credit_data[,-c(2,5,8,11,13,16,18,21)]
numerical<-credit_data[,c(2,5,8,11,13,16,18,21)]

cat_dummy<-dummy(categorical)

#Now we need to drop one variable from each category to avoid multi-collinearity 
#in our model. Thus we will drop one dummy variable from each category

cat_dummy_new<-cat_dummy[,-c(1,5,10,20,25,30,34,37,41,44,47,51,53)]

#Now let's combine the numerical and dummy data together
new_data<-cbind(cat_dummy_new,numerical)
new_data$`Customer_Status`<-ifelse(new_data$`Customer_Status`==1,1,0) 
#Converting response to 1 an 0, 0=Good and 1=Bad


#Splitting the data into test train. 
#Also, we must be careful that response data is proportionately split into train 
#and test data. Below table shows we have 699 Good and 300 Bad values

table(new_data$`Customer_Status`)

#Setting seed
set.seed(42)
#Using create Data partition
train_index<-createDataPartition(y=new_data$`Customer_Status`, p=0.7,
                                 times = 1, list = FALSE)
#Training data
train_data<-new_data[train_index,] 

#Testing data
test_data<-new_data[-train_index,]

# Initial Model with all variables
initial_model <- glm(train_data$`Customer_Status`~.,family = binomial, 
                     data=train_data)
summary(initial_model)

#Now looking at the p-values of the variable, lot's of them are not significant 
#and thus we can build a new model with only the significant variables. We will 
#use the p value threshold as 0.05

#Model with most significant variables from the initial model
model1<-glm(formula = train_data$Customer_Status ~ Checking.account.Status_A13 + 
              Checking.account.Status_A14 + Credit.history_A34 + 
               Purpose_A41 + Purpose_A42 + Purpose_A43 + 
               Savings.account.bonds_A62 + Savings.account.bonds_A65 + 
               Other.debtors...guarantors_A102 + 
               Other.installment.plans_A143 + 
               foreign.worker_A202 + `Duration in month` + `Credit amount` + 
               `Installment rate in % disposable income`, 
             family = binomial, data = train_data)

summary(model1)

#We will keep model1 as final model as most of the variable being significant
#Now changing the test data with the same variables as in the model1 for 
#predictions

test_new<-test_data[,c('Checking.account.Status_A13', 
                         'Checking.account.Status_A14','Credit.history_A34',
                         'Purpose_A41','Purpose_A42','Purpose_A43',
                         'Savings.account.bonds_A62','Savings.account.bonds_A65',
                         'Other.debtors...guarantors_A102',
                         'Other.installment.plans_A143',
                         'foreign.worker_A202','Duration in month','Credit amount',
                         'Installment rate in % disposable income','Customer_Status')]


#Calculate the predicted probabilities for the test data using 0.5 as threshold
predicted<- predict(model1,test_new,type = "response" )
predicted_int<-as.integer(predicted>0.5)

tab<-table(predicted_int, test_new$Customer_Status)
con<-confusionMatrix(tab,positive = '1')                
con

#Accuracy predicted with 0.5 threshold is 75%

#Trying some more thresholds to see the accuracy
#Threshold 0.3
predicted_int1<-as.integer(predicted>0.3)

tab1<-table(predicted_int1, test_new$Customer_Status)
con1<-confusionMatrix(tab1,positive = '1')                
con1

#Accuracy predicted with 0.3 threshold is 72%

#Threshold 0.6
predicted_int2<-as.integer(predicted>0.2)

tab2<-table(predicted_int2, test_new$Customer_Status)
con2<-confusionMatrix(tab2,positive = '1')                
con2

#Accuracy predicted with 0.6 threshold is 71%

#Thus the accuracy is best between 0.5-0.6 thresh

#Plotting ROC
roc_curve<-roc(test_new$Customer_Status,predicted_int)
plot(roc_curve)
roc_curve$auc
#A highly performant classifier will have an ROC that rises steeply to the 
#top-left corner, that is it will correctly identify lots of positives without 
#misclassifying lots of negatives as positives.



#So accuracy of the model is around 75% with a 0.5 as threshold. Accuracy 
#basically tells us how effective the model was in characterizing bad and 
#good status.


#The aim of the exercise is to reduce the number of defaulters.
#The business aim is to minimize the number of loans given to defaulters.
#The False positives needs to be reduced. So the sensitivity needs to be high

#Threshold 0.6
predicted_int6<-as.integer(predicted>0.6)

tab6<-table(predicted_int6, test_new$Customer_Status)
con6<-confusionMatrix(tab6,positive = '1')                
con6

#Sensitivity predicted with 0.6 threshold is 79%

#Threshold 0.4
predicted_int4<-as.integer(predicted>0.4)

tab4<-table(predicted_int4, test_new$Customer_Status)
con4<-confusionMatrix(tab4,positive = '1')                
con4

#Sensitivity predicted with 0.6 threshold is 92%

#Threshold 0.2
predicted_int2<-as.integer(predicted>0.2)

tab2<-table(predicted_int2, test_new$Customer_Status)
con2<-confusionMatrix(tab2,positive = '1')                
con2

#Sensitivity predicted with 0.1 threshold is 98%

#Threshold 0.1
predicted_int1<-as.integer(predicted>0.1)

tab1<-table(predicted_int2, test_new$Customer_Status)
con1<-confusionMatrix(tab2,positive = '1')                
con1

#Sensitivity predicted with 0.1 threshold is 98%
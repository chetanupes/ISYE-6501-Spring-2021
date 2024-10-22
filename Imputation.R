#Getting the data
data<-read.table('breast-cancer-wisconsin.data.txt', sep = ',', header = TRUE,
                 stringsAsFactors = FALSE)

#Renaming response variable
colnames(data)[colnames(data) == 'X2.1'] <- 'R'


#Finding columns with missing data
apply(data, 2, function(x) which(x == "?"))

#From the above code we can see that only one column X1.3 has missing 
#values

#Using Mean imputation
df<-data
df<-as.data.frame((apply(df,2,as.numeric))) #Converting data type to Numeric
df[is.na(df[,7]),7]<-round(mean(df[,7],na.rm=TRUE),digits = 2)

#Using Mode imputation
df1<-data

#Creating a function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Mode for the column Bare Nuclei
d<-Mode(df$X1.3)

#Now replacing the '?' in the column with the mode value
df1$X1.3[df1$X1.3=='?']<-d

#################################################################################
library(MASS)
library(plyr) 

#Getting the data
cancer<-read.table('breast-cancer-wisconsin.data.txt', sep = ',', header = TRUE,
                 stringsAsFactors = FALSE)

#Separating the missing value rows and creating two data frames
idx<-which(cancer$X1.3=='?')
miss_data<-cancer[idx,]

#Modeling without the missing data
new_data<-cancer[-idx,]

#Building the model
model<-lm(X1.3~X5+X1+X1.1+X1.2+X3+X1.4+X1.5+X2.1, data=new_data)

#Model Summary
summary(model)

#The model is only approximately 70% accurate. Applying the variable selection 
#method we learned for better prediction

##Using Step-wise regression and evaluating the results
ste<-stepAIC(model, scope = list(lower = X1.3~1, upper = X1.3~X5+X1+X1.1+X1.2+X3+X1.4+X1.5+X2.1),
             direction = 'both',trace = 1)
summary(ste)

#We can see that the step model predicts 6 predictors for the best model.
#Now we will build a new regression model using just the 6 predictors and
#evaluate the model

#New model
new<-lm(X1.3 ~ X1 + X1.1 + X1.2 + X3 + X1.4 + X2.1, data = new_data)
#New model summary
summary(new)

#Testing data
test<-miss_data[,c('X1','X1.1','X1.2','X3','X1.4','X2.1')]

#Predicting
pred<-predict(new,test)

#Binding the predicted data together
total<-data
total<-replace(data,data=='?',round(pred,digits = 0))

############################################################################
set.seed(42)
d<-data
imp <- rnorm(nrow(miss_data), pred, sd(pred))
imp

#Binding the predicted data together
purt<-data
purt<-replace(data,data=='?',round(imp,digits = 0))

#############################################################################
# Compare the results and quality of classification models
library(kknn)

#Building the Mean Imputed data
df_mean<-df[,2:11]
df_index_mean<-sample(1:nrow(df_mean), size = round(0.7*nrow(df_mean)), replace=FALSE)

#Training data
train_mean<-df_mean[df_index_mean,]
#Testing data
test_mean<-df_mean[-df_index_mean,]

acc_mean<-list()
for (j in 1:10){ #Loop to test different k between 1-10
  
  model_mean = kknn(R~.,
                    train=train_mean,
                    test=test_mean,
                    k = j,
                    scale = TRUE)
  pred_mean = fitted(model_mean)
  pre_mean<-ifelse(pred_mean>2,4,2)
  acc_mean[j]=sum(pre_mean == test_mean[,10])/nrow(test_mean)
}

#Building the Mode Imputed data
df_mode<-df1[,2:11]
df_index_mode<-sample(1:nrow(df_mode), size = round(0.7*nrow(df_mode)), replace=FALSE)

#Training data
train_mode<-df_mode[df_index_mode,]
#Testing data
test_mode<-df_mode[-df_index_mode,]

acc_mode<-list()
for (j in 1:10){ #Loop to test different k between 1-10
  
  model_mode = kknn(R~.,
                    train=train_mode,
                    test=test_mode,
                    k = j,
                    scale = TRUE)
  pred_mode = fitted(model_mode)
  pre_mode<-ifelse(pred_mode>2,4,2)
  acc_mode[j]=sum(pre_mode == test_mode[,10])/nrow(test_mode)
}

#Building the Regression Imputed data
df_reg<-total[,2:11]
df_index_reg<-sample(1:nrow(df_reg), size = round(0.7*nrow(df_reg)), replace=FALSE)

#Training data
train_reg<-df_reg[df_index_reg,]
#Testing data
test_reg<-df_reg[-df_index_reg,]

acc_reg<-list()
for (j in 1:10){ #Loop to test different k between 1-10
  
  model_reg = kknn(R~.,
                   train=train_reg,
                   test=test_reg,
                   k = j,
                   scale = TRUE)
  pred_reg = fitted(model_reg)
  pre_reg<-ifelse(pred_reg>2,4,2)
  acc_reg[j]=sum(pre_reg == test_reg[,10])/nrow(test_reg)
}

#Building the Perturbation Imputed data
df_purt<-as.data.frame(purt[,2:11])
df_index_purt<-sample(1:nrow(df_purt), size = round(0.7*nrow(df_purt)), replace=FALSE)

#Training data
train_purt<-df_purt[df_index_purt,]
#Testing data
test_purt<-df_purt[-df_index_purt,]

acc_purt<-list()
for (j in 1:10){ #Loop to test different k between 1-10
  
  model_purt = kknn(R~.,
                    train=train_purt,
                    test=test_purt,
                    k = j,
                    scale = TRUE)
  pred_purt = fitted(model_purt)
  pre_purt<-ifelse(pred_purt>2,4,2)
  acc_purt[j]=sum(pre_purt == test_purt[,10])/nrow(test_purt)
}

#Building without imputation
data_without<-data[-idx,]
df_without<-data_without[,2:11]
df_index_without<-sample(1:nrow(df_without), size = round(0.7*nrow(df_without)), replace=FALSE)

#Training data
train_without<-df_without[df_index_without,]
#Testing data
test_without<-df_without[-df_index_without,]

#Building the model using Mean Imputation
acc_without<-list()
for (j in 1:10){ #Loop to test different k between 1-10
  
  model_knn = kknn(R~.,
                   train=train_without,
                   test=test_without,
                   k = j,
                   scale = TRUE)
  pred_knn = fitted(model_knn)
  pre<-ifelse(pred_knn>2,4,2)
  acc_without[j]=sum(pre == test_without[,10])/nrow(test_without)
}

#Printing Accuracy for Mean Imputed Model
print(acc_mean)
#Printing Accuracy for Mode Imputed Model
print(acc_mode)
#Printing Accuracy for Regression Imputed Model
print(acc_reg)
#Printing Accuracy for Perturbation Imputed Model
print(acc_purt)
#Printing Accuracy for Mean Imputed Model
print(acc_without)

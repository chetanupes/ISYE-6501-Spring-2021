install.packages('glmnet')

library(MASS)
library(caret)
library(glmnet)

#Getting the data
crime_data<-read.table('uscrime.txt', sep = "", header = TRUE)

#Set seed 
set.seed(42)

#Fitting the full model
lmr<-lm(Crime~., data = crime_data)
summary(lmr)

#Using Stepwise regression and evaluating the results
step<-stepAIC(lmr, scope = list(lower = Crime~1, upper = Crime~.),
              direction = 'both',trace = 1)
#Model summary
summary(step)
step$anova

#We can see that the step model predicts 8 predictors for the best model. 
#Now we will build a new regression model using just the 8 predictors and 
#evaluate the model

#New model
new<-lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = crime_data)

#New model summary
summary(new)

#Now to further evaluate this model let use cross validation 
#Linear regression model using cross validation 
set.seed(123)
train.control <- trainControl(method = "cv", number = 5)
model<-train(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob,
               data = crime_data, method='lm',  #Linear Model2 with CV
               trControl=train.control)          
# CV Model summary
summary(model) 

#Note: The summary shows M.F and U1 are not significant based on the p-value
#(0.05). We can further test this by removing both M.F and U1

#New model
new_1<-lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = crime_data)

#New model summary
summary(new_1)

#Now to further evaluate this model let use cross validation 
#Linear regression model using cross validation 
set.seed(123)
train.control <- trainControl(method = "cv", number = 5)
model1<-train(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob,
             data = crime_data, method='lm',  #Linear Model2 with CV
             trControl=train.control)  

# CV Model summary
summary(model1) 

#The whole purpose of stepwise regression is to eliminate insignificant factors 
#and make the model simpler. This should not be at the cost of losing important 
#variables.We can see that the new model with only 6 factors produced an adjusted 
#R2 of 73%. The previous model with all the 8 factors produced an adjusted R2 of
#74%. So, the metrics for both models is close so we might as well keep the 8 
#factors i n our model.

###############################################################################
#LASSO:#Note we should only scale numerical variables and not categorical

#Let first separate response and predictor for the algorithm
X<-crime_data[,1:15]
y<-crime_data[16]

#We need to also scale the data except the response data
X_scale<-scale(X)

# Setting alpha = 1 implements Lasso regressions
set.seed(42)
lasso_cv <- cv.glmnet(x=as.matrix(X_scale), y=as.matrix(y), alpha = 1,
                     nfolds = 10)

lasso_cv$dev.ratio[which(lasso_cv$lambda == lasso_cv$lambda.min)]
lasso_cv$glmnet.lasso_cv$dev.ratio[which(fitnet$glmnet.lasso_cv$lambda == fitnet$lambda.min)]

lasso_cv$glmnet.fit$dev.ratio[which(lasso_cv$glmnet.fit$lambda==lasso_cv$lambda.min)]

# Plot cross-validation results
plot(lasso_cv)

#Lambda that gives minimum mean cross-validated error
lasso_cv$lambda.min

#Selected Lambda and the corresponding coefficients
coef(lasso_cv, s=lasso_cv$lambda.min)

#Now using the above variables from the Lasso regression results and run a new 
#regression model

new_from_lasso<-lm(Crime ~ M+So+Ed+Po1+M.F+NW+U1+U2+Ineq+Prob+Wealth, 
                   data = crime_data)
summary(new_from_lasso)

#Further based on the p-value we can remove So,M.F,NW,U1,Wealth and run a new model
mod<-lm(Crime ~ M+Ed+Po1+U2+Ineq+Prob, 
        data = crime_data)
summary(mod)
mod

#The following model have all variables with significant values so we can keep it.
#Also, it is the same model we got through stepwise regression

#######################################################
#Elastic net

#For elastic net we use different values of alpha and see which model gives 
#us the best result.

# Build the model using the values of alpha between 1-0

metric<-list()
rng<-as.list(seq(from = 0, to = 100, by = 1))

for (i in 1:length(rng)) {
  set.seed(42)
  elastic<-cv.glmnet(x=as.matrix(X_scale), y=as.matrix(y),
                          alpha=i/100,nfolds = 10)
  metric[i]=elastic$glmnet.fit$dev.ratio[which(elastic$glmnet.fit$lambda==
                                                        elastic$lambda.min)]
}

# Getting the best alpha from the metric list above
mat<-data.frame(Alpha=unlist(rng), R2=unlist(metric))

#Sorting the dataframe for best alpha
data_sort<-mat[order(-mat$R2),]

#Best Alpha
best_alpha<-data_sort[1,1]

#Now building a elastic net using the best alpha and evaluating the results
best_mod<-cv.glmnet(x=as.matrix(X_scale), y=as.matrix(y),
                    alpha=best_alpha,nfolds = 10)

# Plot cross-validation results
plot(best_mod)

#Lambda that gives minimum mean cross-validated error
best_mod$lambda.min

#Selected Lambda and the corresponding coefficients
coef(best_mod, s=best_mod$lambda.min)

#Now using the above variables from the Elastic net and run a new 
#regression model

new_from_elastic<-lm(Crime ~ M+So+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob, 
                   data = crime_data)
summary(new_from_elastic)

#Further based on the p-value we can remove So,LF,M.F,NW and run a new model
mod_last<-lm(Crime ~ M+Ed+Po1+U2+Ineq+Prob, 
        data = crime_data)
summary(mod_last)


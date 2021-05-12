#Question 4.2 

#The iris data set iris.txt contains 150 data points, each with four predictor 
#variables and one categorical response. The predictors are the width and length
#of the sepal and petal of flowers and the response is the type of flower. 
#The data is available from the R library datasets and can be accessed with 
#iris once the library is loaded. It is also available at the UCI Machine 
#Learning Repository (https://archive.ics.uci.edu/ml/datasets/Iris ). 
#The response values are only given to see how well a specific method performed 
#and should not be used to build the model.

#Use the R function kmeans to cluster the points as well as possible. 
#Report the best combination of predictors, your suggested value of k, 
#and how well your best clustering predicts flower type.
###############################################################################

#Getting the data
iris_data<-read.table('iris.txt', sep = "", header = TRUE)

#Visulaizing the data using various combination to get some intitution of the
#data distribution
ggplot(data = iris_data, aes(Sepal.Length, 
                             Sepal.Width, 
                             color=iris_data$Species))+
  geom_point(alpha=1) #The plot shows two clusters with sertosa having a clean
#cluster. The other two species are in the other cluster

ggplot(data = iris_data, aes(Petal.Length, 
                             Petal.Width, 
                             color=iris_data$Species))+
  geom_point(alpha=1) #Shows three good clusters

ggplot(data = iris_data, aes(Petal.Length, 
                             Sepal.Length, 
                             color=iris_data$Species))+
  geom_point(alpha=1) #Shows three ok clusters

ggplot(data = iris_data, aes(Petal.Width, 
                             Sepal.Width, 
                             color=iris_data$Species))+
  geom_point(alpha=1) #Shows three good clusters

ggplot(data = iris_data, aes(Petal.Length, 
                             Sepal.Width, 
                             color=iris_data$Species))+
  geom_point(alpha=1) #Shows three clusters

ggplot(data = iris_data, aes(Petal.Width, 
                             Sepal.Length, 
                             color=iris_data$Species))+
  geom_point(alpha=1) #Shows three clusters

#This above method of trying to plot all comibnation of predictor variables will
#not be effective when there are a lot of predictors. It seems with this process
#we need somewhere between 2 to 3 clusters, but we will test for upto 5 clusters

#######Now evaluating kmeans with different data combinations#################

#Scaling the data
scale_iris<-scale(iris_data[-5])

#Tesing the algorithm with all the predictors for different clusters
table1<-list()
for (i in 2:5){
  model_all<-kmeans(x=scale_iris[,1:4], centers = i, nstart = 30)
  table1[[i]]=table(iris_data$Species,model_all$cluster)
  print(table1[[i]])
}

#Tesing the algorithm with Petal Length & Petal Width for different clusters
table2<-list()
for (i in 2:5){
  model_all<-kmeans(x=scale_iris[,3:4], centers = i, nstart = 30)
  table2[[i]]=table(iris_data$Species,model_all$cluster)
  print(table2[[i]])
}

#Tesing the algorithm with Petal Width & Sepal Width for different clusters
table3<-list()
for (i in 2:5){
  model_all<-kmeans(x=scale_iris[,c(2,4)], centers = i, nstart = 30)
  table3[[i]]=table(iris_data$Species,model_all$cluster)
  print(table3[[i]])
}

#Comparing all the tables its clear that 3 clusters can define the data well, 
#with Petal length and Petal width providing the best classification with 
#Sertosa in cluster 3 completely without any mis-classification. For Versicolor
#most of them are in cluster 2 with only 2 in cluster 1. Similarly,for virginica
#most of them are in cluster 1 with only 4 in cluster 2
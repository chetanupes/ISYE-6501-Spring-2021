install.packages('outliers')

library(outliers)
library(ggplot2)

#Getting the data
data<-read.table('Crime Data.txt', sep = "", header = TRUE)

#Testing if the dataset (Crime rate) in question is normally distributed before 
#applying grubbs test. Normality is tested using Histogram, Q-Q Plot,  
#Shapiro-WilK test 

#Histogram
ggplot(data=data, aes(Crime)) + geom_histogram()

#Q_Q Plot
ggplot(data, aes(sample=Crime))+ stat_qq() + stat_qq_line()

#Shapiro-WilK test 

#Stating the hypothesis, threshold alpha=0.05 (significance level)
#H0: The data is normally distributed
#H1: The data is not normally distributed. 

#Test 1
norm_test<-shapiro.test(data$Crime)
norm_test

#Since p value for the test is less than than the chosen threshold the null 
#hypotheses is rejected and the data used in this test is not normally 
#distributed. A log normal transformation can be achieve normality or by 
#removing the outliers as Shapiro test is sensitive to sample size, meaning if
# sample size is sufficiently large this test may detect even trivial departures 
#from the null hypothesis. So we will assume the data to to normally distributed
#as also evident from the Q-Q plot where most of the data lies in a st. line.

#Outlier detection using Box plot

ggplot(data,aes(Crime))+geom_boxplot()

#The box plot shows that there might be few points as outliers. We can now 
#perform Grubbs Test

#Using Grubbs test (alpha(sigificance level)=0.05)
test1<-grubbs.test(data$Crime, type = 10, opposite = FALSE, 
                   two.sided = FALSE)
test1

#test1 show point 1993 is an outlier is an outlier as is close to alpha.Next we will remove  
#this point and continue to investigate if we have more outliers.

#Removing 1993 data 
data2<-data[-which.max(data$Crime),]

test2<-grubbs.test(data2$Crime, type = 10, opposite = FALSE, 
                   two.sided = FALSE)
test2

#test1 show point 1969 is an outlier as is close to alpha.Next we will remove  
#this point and continue to investigate if we have more outliers.

#Removing 1969 data 
data3<-data2[-which.max(data2$Crime),]

test3<-grubbs.test(data3$Crime, type = 10, opposite = FALSE, 
                   two.sided = FALSE)
test3

#test1 show point 1664 is not an outlier as p is much greater than alpha. 
#We have now removed all the outliers 


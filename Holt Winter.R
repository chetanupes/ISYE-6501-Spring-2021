install.packages("TTR")
install.packages("forecast")
install.packages("xlsx")

library(ggplot2)
library(TTR)
library(forecast)
library(xlsx)


#Getting the data
temp<-read.table('temps.txt',sep = "", header = TRUE)

#Converting the data to make it ready for time series analysis. The code below 
#puts the data in a long list containing the yearly data with temperatures 
#for the four months
con<-as.vector(unlist(temp[,2:21]))

#Time series data
dt<-ts(con, start = 1996, frequency = 123)

#Plotting the time series data
plot.ts(dt)

#Decomposing the data to understand its components. This step helps to see how 
#to use Holt Winters method. 
plot(decompose(dt))

#The above plot shows that there is seasonality in the data but there is no 
#specific trend in the data (increasing/ decreasing)

#The additive model is useful when the seasonal variation is relatively constant
#over time.The multiplicative model is useful when the seasonal variation 
#increases over time. Thus, we will use the additive model for Holt Winter's 
#since seasonal variations are constant.
 
#Applying Holt Winter's method taking care of level, trend and seasonality 

hw<-HoltWinters(dt, alpha =NULL , beta=NULL, gamma = NULL, 
                seasonal = 'additive')

#Root Mean Square Error
sqrt(hw$SSE)

#Plotting the results
plot(hw)

#Looking into the individual components of the fitted data seasonality data of 
#the Holt Winter's results and taking the seasonality data to do CUSUM and see 
#if we can see detect any change and in the temperature

plot(hw$fitted[,1]) #Predicted data
plot(hw$fitted[,2]) #Level
plot(hw$fitted[,3]) #Trend
plot(hw$fitted[,4]) #Seasonality

#Extracting the seasonality data as a matrix and exporting it into a csv
seas<-matrix(hw$fitted[,4], nrow = 123)
write.xlsx(seas, "C:/Users/cheta/OneDrive/Desktop/seas.xlsx")

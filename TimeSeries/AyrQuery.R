#Name:Harshita Singh
#Time Series - Part 2

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Setup directory
setwd("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis//WorkDirectory-Assignment")

#Load file:
load("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/Assignment -2/Ayr.RData")

#structure of the data frame
str(Ayr)

#Install Packages
if(!require(tseries))(install.packages("tseries"))
library("tseries")

if(!require(TTR))(install.packages("TTR"))
library("TTR")

if(!require(smooth))(install.packages("smooth"))
library("smooth")


#Question 1(1)
#Data Transformation
AyrDataTransform_HS <- ts(Ayr, start=c(1968),frequency=1)
AyrDataTransform_HS
head(AyrDataTransform_HS)

#Question 2(1)Descriptive Data Analysis
#Summarize the information (mean, std dev, etc.)
summary(AyrDataTransform_HS)
AyrDataTransform_HS


#Question 2(2)
#Plot the time series data
plot.ts(AyrDataTransform_HS, main = "Average Temperature")

#Question 2(3)
#Smooth the temperature chart using a moving average. 
#Try 3 different values for the moving average and choose the one you think best shows the trend (if any).
AyrDataStudySMA_HS <-SMA(AyrDataTransform_HS, n=3)
plot.ts(AyrDataStudySMA_HS)

AyrDataStudySMA_HS <-SMA(AyrDataTransform_HS, n=5)
plot.ts(AyrDataStudySMA_HS)

AyrDataStudySMA_HS <-SMA(AyrDataTransform_HS, n=7)
plot.ts(AyrDataStudySMA_HS)

#Question 2(4)
#Determine if the time series is stationary.
adf.test(AyrDataTransform_HS)
#Conclusion: p-value = 0.0451; so, it's is smaller than 0.05 and non-stationary

#Question 2(5)
#Create an autocorrelation chart (using acf) and comment on which lags are significant.
acf(AyrDataTransform_HS)
#Conclusion:


#Question 3(1)
#Create a simple moving average forecast of temperature in Ayr for five years beyond the data provided.
#Graph your results along with a 75% prediction interval.
AyrMovingAvg_HS <- sma(AyrDataTransform_HS)
AyrMovingAvg_HS
AyrMovingAvg_HS <- forecast(AyrMovingAvg_HS, h=5, level=0.75)
AyrMovingAvg_HS
plot(AyrMovingAvg_HS)


#Question 3(2)
#Create an exponentially smoothed forecast of temperature in Ayr for five years 
#beyond the data provided. Graph your results along with a 75% prediction interval
AyrExpoFore_HS <- es(AyrDataTransform_HS)
AyrExpoFore_HS
AyrExpoFore_HS <- forecast(AyrExpoFore_HS, h=5, level=0.75)
AyrExpoFore_HS
plot(AyrExpoFore_HS)
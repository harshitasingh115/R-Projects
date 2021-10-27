#Name:Harshita Singh
#Time Series - Part 1

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Setup directory
setwd("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis//WorkDirectory-Assignment")

#Load file:
load("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/Assignment -2/Woodstock.RData")

#structure of the data frame
str(Woodstock)

#Install Packages
if(!require(tseries))(install.packages("tseries"))
library("tseries")

if(!require(TTR))(install.packages("TTR"))
library("TTR")

if(!require(smooth))(install.packages("smooth"))
library("smooth")

#Question 1(1)
WoodStockTransform_HS <- ts(Woodstock, start=c(1988, 1),end = c(2017, 12), frequency=12)
WoodStockTransform_HS
head(WoodStockTransform_HS)

#Question 2(1)
#Convert to a Time Series datatype

summary(WoodStockTransform_HS)
WoodStockTransform_HS

#Question 2(2)
plot.ts(WoodStockTransform_HS, main="Average Temperature", ylim=c(-15,25))

#Question 2(3)
#Decompose and check for Autocorrelation
decomp_HS <- decompose(WoodStockTransform_HS, type ="additive")
decomp_HS
plot(decomp_HS)


#Question 2(4)
#Determine if the time series is stationary
adf.test(WoodStockTransform_HS)
#Output: p-value smaller than (WoodStockTransform_HS) of p-value, we can say it's likely stationary
#and reject the null hypothesis(non stationary)

#Question 2(5)
#Deseasonalize the information and plot the result.
Deseasonalize_HS <- WoodStockTransform_HS -decomp_HS$seasonal

plot.ts(Deseasonalize_HS, main ="Deseasonalize - Average Information",  ylim=c(-15,25))
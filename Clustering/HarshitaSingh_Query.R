##################################################
###                   PROG8430                  ##
###   Unsupervised Learning: K-Means Clustering     ## 
##################################################
#                                               ##
##################################################
# Written by Harshita Singh
# ID: 8786434
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

##################################################
### Install Libraries                           ##
##################################################

#ggplot2
if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

#cluster
if(!require(MASS)){install.packages("cluster")}
library("cluster")

#factoextra
if(!require(factoextra)){install.packages("factoextra")}
library("factoextra")

#dplyr
if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

##################################################
#Setup directory
setwd("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/WorkDirectory-Assignment")

#Load file:
load("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/PROG8430_Clst_21S.RData")
options(scipen=9)

#renaming the dataframe
ExpenseData_HS <- PROG8430_Clst_21S

#remove the previous dataset
rm(PROG8430_Clst_21S)
head(ExpenseData_HS)

#Renaming for easier interpretation
names(ExpenseData_HS) <- c("Food_HS", "Entr_HS", "Educ_HS", "Tran_HS", "Work_HS", "Hous_HS", "Othr_HS")
str(ExpenseData_HS)


##################################################
###             Standardize of Data             ##
##################################################

#Create a quick standardization function
normStnd_HS <- function(x)
{
  return((x-min(x))/max(x)-min(x))
}

#A Different standardization function ()
normDf_HS <- function(x)
{  
  return((x-min(x)/sd(x)))
}

#Standardization Food
ExpenseData_HS$Food_HS_MinMax <- normDf_HS(ExpenseData_HS$Food_HS)
#ExpenseData_HS$Food_HS_Normx <- normStnd_HS(ExpenseData_HS$Food_HS)

#Standardization Entrance
ExpenseData_HS$Entr_HS_MinMax <- normDf_HS(ExpenseData_HS$Entr_HS)
#ExpenseData_HS$Food_HS_Normx <- normStnd_HS(ExpenseData_HS$Entr_HS)

#Standardization Education
ExpenseData_HS$Educ_HS_MinMax <- normDf_HS(ExpenseData_HS$Educ_HS)
#ExpenseData_HS$Educ_HS_Normx <- normStnd_HS(ExpenseData_HS$Educ_HS)

#Standardization Transportation
ExpenseData_HS$Tran_HS_MinMax <- normDf_HS(ExpenseData_HS$Tran_HS)
#ExpenseData_HS$Tran_HS_Normx <- normStnd_HS(ExpenseData_HS$Tran_HS)

#Standardization Work Related Expenses
ExpenseData_HS$WorkRE_HS_MinMax <- normDf_HS(ExpenseData_HS$Work_HS)
#ExpenseData_HS$WorkRE_HS_Normx <- normStnd_HS(ExpenseData_HS$Work_HS)

#Standardization House
ExpenseData_HS$House_HS_MinMax <- normDf_HS(ExpenseData_HS$Hous_HS)
#ExpenseData_HS$House_HS_Normx <- normStnd_HS(ExpenseData_HS$Hous_HS)

#Standardization Other Expenses
ExpenseData_HS$Other_HS_MinMax <- normDf_HS(ExpenseData_HS$Othr_HS)
#ExpenseData_HS$Other_HS_Normx <- normStnd_HS(ExpenseData_HS$Othr_HS)

head(ExpenseData_HS)
summary(ExpenseData_HS)
str(ExpenseData_HS)

#Create graphical summaries of the data
boxplot(ExpenseData_HS)


##################################################
##                 Clustering                   ##
##################################################


#Two variables as your centroids (Hous and Entr)
ExpenseDataClrData_HS <- ExpenseData_HS[c(9,13)]
str(ExpenseDataClrData_HS)

#Creating the first cluster
ClstDen_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 2, nstart = 10)
ClstDen_HS


#create clusters with k=2
ClstDen2_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 2, nstart = 10)
ClstDen2_HS

#create clusters with k=3
ClstDen3_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 3, nstart = 10)
ClstDen3_HS

#create clusters with k=4
ClstDen4_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 4, nstart = 10)
ClstDen4_HS

#create clusters with k=5
ClstDen5_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 5, nstart = 10)
ClstDen5_HS

#create clusters with k=6
ClstDen6_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 6, nstart = 10)
ClstDen6_HS


#Adding cluster tag to the variables
ExpenseData_HS$cluster <- factor(ClstDen2_HS$cluster)
head(ExpenseData_HS)

center_HS <-data.frame(cluster=factor(1:2), ClstDen_HS$centers)
head(center_HS)

##################################################
##           Plotting the Clusters              ##
##################################################
ggplot(data=ExpenseData_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax, color=cluster)) + geom_point()

ggplot(data=ExpenseData_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax, color=cluster, shape=cluster)) +
  geom_point(alpha=.8)+ 
  geom_point(data=center_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax), size=2, stroke=2)


ExpenseDataSUM_HS <-ExpenseData_HS %>%
  group_by(cluster) %>%
  summarise(Food_HS=mean(Food_HS), Entr_HS=mean(Entr_HS), Educ_HS=mean(Educ_HS), 
            Tran_HS=mean(Tran_HS), 
            Work_HS=mean(Work_HS), Hous_HS= mean(Hous_HS), 
            Othr_HS=mean(Othr_HS), N=n())

ExpenseDataSUM_HS

##################################################
##           Evaluation of Clusters             ##
##################################################
#ClstDenTest_HS <- kmeans(ExpenseDataClrData_HS, iter.max = 10, centers = 1, nstart = 10)
#ClstDenTest_HS

#K=2
#For K-1:
ggplot(data=ExpenseData_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax, color=cluster)) + geom_point()

ggplot(data=ExpenseData_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax, color=cluster, shape=cluster)) +
  geom_point(alpha=.8)+ 
  geom_point(data=center_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax), size=1, stroke=2)


#K=3
#For K+1:
ggplot(data=ExpenseData_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax, color=cluster)) + geom_point()

ggplot(data=ExpenseData_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax, color=cluster, shape=cluster)) +
  geom_point(alpha=.8)+ 
  geom_point(data=center_HS, aes(x=House_HS_MinMax, y=Entr_HS_MinMax), size=3, stroke=2)
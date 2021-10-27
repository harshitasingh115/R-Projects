##################################################
###                   PROG8430                  ##
###     Multivariate Linear Regression          ## 
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

#pastecs
if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

#corrgram
if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##################################################
#Setup directory
setwd("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/WorkDirectory-Assignment")

#Load file:
load("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/Assignment -3/PROG8430_Assign_MLR_21S.RData")
options(scipen=9)

#renaming the dataframe
surveyDataSet_HS <- PROG8430_Assign_MLR_21S
#remove the previous dataset
rm(PROG8430_Assign_MLR_21S)

#structure of the data frame
str(surveyDataSet_HS)
head(surveyDataSet_HS)

#List of all objects
ls(surveyDataSet_HS)
names(surveyDataSet_HS)

#renaming the variables
names(surveyDataSet_HS) <- c("UserID_HS","TreatmentControlGroup_HS", "Graduates_HS","Nationality_HS",
                                    "Gender_HS", "Age_HS", "MaritalStatus_HS", "PoliticalAffiliation_HS",
                                    "ChildrenNumber_HS", "AnnualHouseholdIncome_HS", "Food_HS", "Housing_HS",
                                    "OtherExpenses_HS","Score_HS", "StandardizedScore_HS", "Time1_HS",
                                    "Time2_HS", "Time3_HS", "Pol_HS")

##################################################
###             Description of Data             ##
##################################################
#Nation Field - Convert to index (Dummy) variable
NationDummy_HS <- model.matrix(~Nationality_HS -1, data=surveyDataSet_HS)
head(NationDummy_HS)

#Treat/Control Field - Convert to index (Dummy) variable
TreatmentControlGroupDummy_HS <- model.matrix(~TreatmentControlGroup_HS -1, data=surveyDataSet_HS)
head(TreatmentControlGroupDummy_HS)

#Graduate Field - Convert to index (Dummy) variable
GraduateDummy_HS <- model.matrix(~Graduates_HS -1, data=surveyDataSet_HS)
head(GraduateDummy_HS)

#Gender Field - Convert to index (Dummy) variable
GenderDummy_HS <- model.matrix(~Gender_HS -1, data=surveyDataSet_HS)
head(GenderDummy_HS)

#Martial Status Field - Convert to index (Dummy) variable
MaritalStatusDummy_HS <- model.matrix(~MaritalStatus_HS -1, data=surveyDataSet_HS)
head(MaritalStatusDummy_HS)

#Political Affiliation Field - Convert to index (Dummy) variable
PoliticalAffiliationDummy_HS <- model.matrix(~PoliticalAffiliation_HS -1, data=surveyDataSet_HS)
head(PoliticalAffiliationDummy_HS)

#Remove unnecessary column
summary(surveyDataSet_HS)
surveyDataSet_HS <- surveyDataSet_HS[-c(1)]
head(surveyDataSet_HS)
summary(surveyDataSet_HS)

#Combine the datasets again
surveyDataSet_HS <- cbind(surveyDataSet_HS, NationDummy_HS, TreatmentControlGroupDummy_HS, GraduateDummy_HS,GenderDummy_HS, MaritalStatusDummy_HS,PoliticalAffiliationDummy_HS)
head(surveyDataSet_HS)
str(surveyDataSet_HS)
summary(surveyDataSet_HS)

names(surveyDataSet_HS) <- c("TreatmentControlGroup_HS", "Graduates_HS", "Nationality_HS",
                             "Gender_HS",
                             "Age_HS","MaritalStatus_HS","PoliticalAffiliation_HS",
                             "ChildrenNumber_HS", "AnnualHouseholdIncome_HS","Food_HS", 
                             "Housing_HS", "OtherExpenses_HS","Score_HS",
                              "StandardizedScore_HS", "Time1_HS",
                             "Time2_HS", "Time3_HS", "Pol_HS",
                            "Asia_HS","Europe_HS","NorthAmerica_HS", "Southern_HS",
                            "Control_HS", "Treat_HS",
                            "No_Graduates_HS","Yes_Graduates_HS",
                            "Female_HS", "Male_HS",  
                            "Divorced_HS","Married_HS", "Never_HS", "Widowed_HS",
                            "Conservative_HS", "Liberal_HS", "NewDemocrat_HS", "Other_HS")

head(surveyDataSet_HS)
summary(surveyDataSet_HS)

#Remove the factor values and combine the dataset with the numerice values
surveyResultNum_HS <- surveyDataSet_HS[sapply(surveyDataSet_HS,is.numeric)]
surveyResultNum_HS
head(surveyResultNum_HS)


#Remove the Age where it's -10
surveyResultNum_HS <- surveyResultNum_HS[!surveyResultNum_HS$Age_HS== -10,]


par(mar=rep(2,4))
par(mfrow=c(3,3))  #Fit more graphs in
#Histogram for all variables loop over column names instead of actual columns

sapply(names(surveyResultNum_HS), function(cname){
  if(is.numeric(surveyResultNum_HS[[cname]]))
    print(hist(surveyResultNum_HS[[cname]], main=cname))
})

par((mfrow = c(1,1)))

##################################################
###            Reduce Dimensionality            ##
##################################################

#Question 2 - Part 1(Apply the Missing Value Filter to remove appropriate columns of data.)
MissingValue_HS <- colnames(surveyResultNum_HS)[ apply(surveyResultNum_HS, 2, anyNA) ]
MissingValue_HS
#Time1_HS to be removed
surveyResultNum_HS <- surveyResultNum_HS[-c(9)]

stat.desc(surveyResultNum_HS)  #Consider coef of var
summary(surveyResultNum_HS)

#Question 2 - Part 2(Apply the Low Variance Filter to remove appropriate columns of data.)
#Based on the above coef.var, histogram and  Children seem likely. Let's check!
hist(surveyResultNum_HS$ChildrenNumber_HS)
boxplot(surveyResultNum_HS$ChildrenNumber_HS)
surveyResultNum_HS <- surveyResultNum_HS[-c(2)]


#Question 2 - Part 3(Apply the High Correlation Filter to remove appropriate columns of data.)
cor(surveyResultNum_HS, method="spearman")
#Time2_HS and TIme3_HS are highly correlated; getting rid of Time3_HS(Second option)
surveyResultNum_HS <- surveyResultNum_HS[-c(9)]

##################################################
#Question 3 - Part 1(Create boxplots of all relevant variables (i.e. non-binary) to determine outliers.)

par(mar=rep(2,4))
par(mfrow=c(3,3))  #Fit more graphs in
#Histogram for all variables loop over column names instead of actual columns
sapply(names(surveyResultNum_HS), function(cname){
  if(is.numeric(surveyResultNum_HS[[cname]]))
    print(boxplot(surveyResultNum_HS[[cname]], main=cname))
})

par((mfrow = c(1,1)))


##################################################
#Exploratory Analysis
#Question 4 - Part 1(Correlations: Create both numeric and graphical correlations ()
surveyNrm_HS <- lapply(surveyResultNum_HS, shapiro.test)
surveyNrm_HS
str(surveyNrm_HS[[4]])

surveyRes_HS <- sapply(surveyNrm_HS, `[`, c("statistic", "p.value"))
surveyRes_HS

surveyRest_HS <- t(surveyRes_HS)
surveyRest_HS

#Graphical Coorelation
par(mfrow=c(3,3))
#Boxplot for all variables loop over column names instead of actual columns
sapply(names(surveyResultNum_HS), function(cname){
  if(is.numeric(surveyResultNum_HS[[cname]]))
    qqnorm(surveyResultNum_HS[[cname]], main=cname)
  qqline(surveyResultNum_HS[[cname]])
})
par((mfrow = c(1,1)))


##################################################
##          Checking Correlation                ##
##################################################
library(corrgram)

corrgram(surveyResultNum_HS, order=TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,main="Survey Result Stats")

#Relatively Highly correlated; it's a strong linear relationship since the answer: 0.73
res_HS <- cor(surveyResultNum_HS, method="spearman")
round(res_HS, 2)

cor(surveyResultNum_HS$Pol_HS, surveyResultNum_HS$Treat_HS, method="spearman")

###################################################
## Simple Linear Regression                      ##
###################################################
#Question 5 - Part 1(Create a simple linear regression model using
#Pol as the dependent variable and age as the independent. )
SimpleModelPolAge_HS <-lm(surveyResultNum_HS$Pol ~ surveyResultNum_HS$Age )
summary(SimpleModelPolAge_HS)



#Create a scatter plot of the two variables and overlay the regression line.
#Independent variables X=age; Dependent variable Y= Pol.
plot(surveyResultNum_HS$Age, surveyResultNum_HS$Pol, main="Scatterplot of Pol Vs Age", xlab ="Age", ylab="Pol")
cor(surveyResultNum_HS$Age, surveyResultNum_HS$Pol, method="spearman")
abline(SimpleModelPolAge_HS)
abline(SimpleModelPolAge_HS, col=2, lwd=3)

#Question 5 - Part 2
#Create a simple linear regression model using Pol as the dependent variable 
#and income as the independent. 
#Independent variables X=Pol; Dependent variable Y= Income.
SimpleModelPolIncome_HS <-lm(surveyResultNum_HS$Pol ~ surveyResultNum_HS$AnnualHouseholdIncome_HS)
SimpleModelPolIncome_HS


#Create a scatter plot of the two variables and overlay the regression line.
plot(surveyResultNum_HS$AnnualHouseholdIncome_HS, surveyResultNum_HS$Pol, main="Scatterplot of Pol Vs Income", xlab ="Income", ylab="Pol")
cor(surveyResultNum_HS$AnnualHouseholdIncome_HS , surveyResultNum_HS$Pol, method="spearman")
abline(SimpleModelPolIncome_HS)
abline(SimpleModelPolIncome_HS, col=2, lwd=3)

anova(SimpleModelPolAge_HS,SimpleModelPolIncome_HS)

###################################################
## Creating Baseline Model                       ##
###################################################
#Model Development - Multivariate
#Question 6 - Part 1(Full) 
#Model 1: All Variables included
#set.seed(0)
surveyBM_HS=lm(Pol_HS ~ Age_HS + AnnualHouseholdIncome_HS + Food_HS + 
                 Housing_HS + OtherExpenses_HS + Score_HS + StandardizedScore_HS +
                 Time2_HS +  Asia_HS + Europe_HS + NorthAmerica_HS + Southern_HS+
                Control_HS + Treat_HS + No_Graduates_HS + Yes_Graduates_HS + Female_HS+
               Male_HS + Divorced_HS + Married_HS + Never_HS + Widowed_HS + Conservative_HS +
                 Liberal_HS + NewDemocrat_HS + Other_HS ,
               data=surveyResultNum_HS, na.action = na.omit)
surveyBM_HS
summary(surveyBM_HS)

#Model 2: Backward Selection Model
surveyBSM_HS = step(surveyBM_HS, direction="backward", details=TRUE)
surveyBSM_HS
summary(surveyBSM_HS)

#Model 3: Forward Selection Model(Perform the exactly opposite)
modelFWS_HS <- lm(Pol_HS ~ 1, data=surveyResultNum_HS, na.action = na.omit)
surverFWSM_HS = step(modelFWS_HS, direction="forward",
                     scope=(Pol_HS ~ Age_HS + AnnualHouseholdIncome_HS + Food_HS + 
                              Housing_HS + OtherExpenses_HS + Score_HS + StandardizedScore_HS +
                              Time2_HS +  Asia_HS + Europe_HS + NorthAmerica_HS + Southern_HS+
                              Control_HS + Treat_HS + No_Graduates_HS + Yes_Graduates_HS + Female_HS+
                              Male_HS + Divorced_HS + Married_HS + Never_HS + Widowed_HS + Conservative_HS +
                              Liberal_HS + NewDemocrat_HS + Other_HS),
                     details=TRUE)
surverFWSM_HS
summary(surverFWSM_HS)

###################################################
## Creating Criteria Selection Model             ##
###################################################

surveyLM_HS = lm(Pol_HS ~ Age_HS + AnnualHouseholdIncome_HS + Food_HS + 
                   Housing_HS + OtherExpenses_HS + Score_HS + StandardizedScore_HS +
                   Time2_HS +  Asia_HS + Europe_HS + NorthAmerica_HS + Southern_HS+
                   Control_HS + Treat_HS + No_Graduates_HS + Yes_Graduates_HS + Female_HS+
                   Male_HS + Divorced_HS + Married_HS + Never_HS + Widowed_HS + Conservative_HS +
                   Liberal_HS + NewDemocrat_HS + Other_HS,
               data=surveyResultNum_HS, na.action=na.omit)
stp_surveyLM_HS <-step(surveyLM_HS)
stp_surveyLM_HS
summary(stp_surveyLM_HS)

###################################################
##     Creating Model and Residual vectors       ##
###################################################

surveyMFit_HS <-predict(surveyBM_HS)
surveyMRes <- residuals(surveyBM_HS)

surveyBSMFit_HS <-predict(surveyBSM_HS)
surveyBSMRes_HS <-residuals(surveyBSM_HS)

#surverFWSMFit_HS <- predict(surverFWSM_HS)
#surverFWSMRes_HS <- residuals(surverFWSM_HS)

#stp_surveyLMFit_HS <- predict(stp_surveyLM_HS)
#stp_surveyLMRes_HS <- residuals(stp_surveyLM_HS)


#Numerically
shapiro.test(surveyMRes)
shapiro.test(surveyBSMRes_HS)
#shapiro.test(surverFWSMRes_HS)
#shapiro.test(stp_surveyLMRes_HS)

#Graphically
par(mfrow = c(2,2))
plot(surveyBM_HS)
par(mfrow =c(1,1))

par(mfrow = c(2,2))
plot(surveyBSM_HS)
par(mfrow =c(1,1))

#par(mfrow = c(1,3))
#plot(surverFWSM_HS)
#par(mfrow =c(1,1))

#par(mfrow = c(2,2))
#plot(stp_surveyLM_HS)
#par(mfrow =c(1,1))

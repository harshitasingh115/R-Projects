##################################################
###                   PROG8430                  ##
###                 Classification              ## 
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
if(!require(pROC)){install.packages("pROC")}
library("pROC")

#corrgram
if(!require(MASS)){install.packages("MASS")}
library("MASS")


#corrgram
if(!require(klaR)){install.packages("klaR")}
library("klaR")


##################################################
#Setup directory
setwd("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/WorkDirectory-Assignment")

#Load file:
load("C:/Users/Nirmoh/Desktop/Harshita/Data Analysis/Tumor_21S.RData")
options(scipen=9)

#renaming the dataframe
Tumor_21S_HS <- Tumor_21S

#remove the previous dataset
rm(Tumor_21S)
head(Tumor_21S_HS)

#Renaming for easier interpretation
names(Tumor_21S_HS) <- c("Age_HS", "Sex_HS", "Bone_HS", "Marrow_HS", "Lung_HS", "Pleura_HS", "Liver_HS", "Brain_HS", "Skin_HS", "Neck_HS", "Supra_HS", "Axil_HS", "Media_HS", "Out_HS")
str(Tumor_21S_HS)


#Remove the factor values and combine the dataset with the numerice values
#tumorResultNum_HS <- Tumor_21S_HS[sapply(Tumor_21S_HS,is.numeric)]
#tumorResultNum_HS
#head(tumorResultNum_HS)

##################################################
###             Description of Data             ##
##################################################



#Question 1 - Part 1(the data is free from outliers or unnecessary data)
boxplot(Tumor_21S_HS)$out

par(mar=rep(2,4))
par(mfrow=c(3,3))  #Fit more graphs in
#Histogram for all variables loop over column names instead of actual columns
sapply(names(Tumor_21S_HS), function(cname){
  if(is.numeric(Tumor_21S_HS[[cname]]))
    print(boxplot(Tumor_21S_HS[[cname]], main=cname))
})

par((mfrow = c(1,1)))

summary(Tumor_21S_HS)


##################################################
###             Exploratory Analysis            ##
##################################################
#Question 2 - Part 1(Correlations: Create both numeric and graphical correlations ()

TumorCorr_HS <- Filter(is.numeric, Tumor_21S_HS)
str(TumorCorr_HS)

TumorCorr_HS <- cor(Tumor_21S_HS, method="spearman")
round(TumorCorr_HS,2)


#Check the Chi Squared Test - Note Removal of Yate's Continuity Correction
#Strong relationship between Out_HS and Age_HS
#chisqRct_HS <- chisq.test(Tumor_21S_HS$Out_HS, Tumor_21S_HS$Age_HS, correct = FALSE)
#chisqRct_HS

#chisqRct_HS$observed
#chisqRct_HS$expected

#Strong relationship between Out_HS and Skin_HS
#chisqRct1_HS <- chisq.test(Tumor_21S_HS$Out_HS, Tumor_21S_HS$Skin_HS, correct = FALSE)
#chisqRct1_HS

#chisqRct1_HS$observed
#chisqRct1_HS$expected

#Strong relationship between Out_HS and Supra_HS
chisqRct1_HS <- chisq.test(Tumor_21S_HS$Out_HS, Tumor_21S_HS$Supra_HS, correct = FALSE)
chisqRct1_HS


chisqRct1_HS$observed
chisqRct1_HS$expected

Tbl_Rct_HS <- table( Tumor_21S_HS$Out_HS, Tumor_21S_HS$Supra_HS, dnn = list("Tumor", "Supra"))
Tbl_Rct_HS
prop.table(Tbl_Rct_HS, 2)

#Bar Chart
barplot(prop.table(Tbl_Rct_HS, 2), xlab='Supra', ylab = 'Out', main="Tumor by Supra",
        col = c("Green", "Red"), legend= row.names(Tbl_Rct_HS), args.legend = list(x="topleft"))

#Strong relationship between Out_HS and Brain_HS  
chisqRct2_HS <- chisq.test(Tumor_21S_HS$Out_HS, Tumor_21S_HS$Brain_HS, correct = FALSE)
chisqRct2_HS

#what we observed
chisqRct2_HS$observed
#No relationship 
chisqRct2_HS$expected

Tbl_Rct1_HS <- table( Tumor_21S_HS$Out_HS, Tumor_21S_HS$Brain_HS, dnn = list("Tumor","Brain"))
Tbl_Rct1_HS
prop.table(Tbl_Rct1_HS, 2)

#Bar Chart
barplot(prop.table(Tbl_Rct1_HS, 2), xlab='Brain', ylab = 'Pct', main="Brain Tumor",
        col = c("Green", "Red"), legend= row.names(Tbl_Rct1_HS), args.legend = list(x="topleft"))

###################################################
##            Creating  Model                    ##
###################################################

#Model Development - Multivariate
#Question 3 - Part 1(Full) 
#Model: Forward Selection Model
modelFWS_HS <- lm(Out_HS ~ 1, data=Tumor_21S_HS, na.action = na.omit)
TumorFWSM_HS = step(modelFWS_HS, direction="forward",
                     scope=(Out_HS ~ Age_HS + Sex_HS + Bone_HS + Marrow_HS + Lung_HS
                            + Pleura_HS + Liver_HS  +Brain_HS+ Skin_HS + Neck_HS + Supra_HS 
                            + Axil_HS + Media_HS),
                     details=TRUE)
TumorFWSM_HS
summary(TumorFWSM_HS)

#Question 3 - Part 2(Full) 
#Model 1: (Annual - Dropping Supra_HS)
TumorSupra_Annual_HS = glm(Out_HS ~ Age_HS + Sex_HS + Bone_HS + Marrow_HS + Lung_HS
                    + Pleura_HS + Liver_HS  +Brain_HS+ Skin_HS + Neck_HS + Supra_HS
                    + Axil_HS + Media_HS,family = "binomial", data=Tumor_21S_HS, na.action = na.omit)
summary(TumorSupra_Annual_HS)

#Model 2: (Annual - Dropping Brain_HS)
TumorBrain_Annual_HS = glm(Out_HS ~ Age_HS + Sex_HS + Bone_HS + Marrow_HS + Lung_HS
                   + Pleura_HS + Liver_HS  + Skin_HS + Neck_HS + Supra_HS
                   + Axil_HS + Media_HS, family = "binomial", data=Tumor_21S_HS, na.action = na.omit)
summary(TumorBrain_Annual_HS)

###################################################
##                  Model Evaluation             ##
###################################################
#Question 4 - Part 1
#Model 1: create and evaluate the confusion matrix(Dropping Supra_HS)
respSupra_CM_HS <- predict(TumorSupra_Annual_HS, type="response")
head(respSupra_CM_HS)
ClassSupra_CM_HS <- ifelse(respSupra_CM_HS>0.5,1,0)
head(ClassSupra_CM_HS)
TrueSupra_log_HS <-Tumor_21S_HS$Out_HS
T1Supra_HS <- table(TrueSupra_log_HS, ClassSupra_CM_HS, dnn=list("Tumor", "Predicated"))
T1Supra_HS

#Create True Positive, Negatives
TP_HS <- T1Supra_HS[2,2]
TN_HS <-T1Supra_HS[1,1]
FP_HS <- T1Supra_HS[1,2]
FN_HS <- T1Supra_HS[2,1]

#Calculate Accuracy
Acc_HS <- (TP_HS+TN_HS)/sum(T1Supra_HS)
Acc_HS


#Calculate Specify
Sen_HS <-(TN_HS)/sum(TN_HS+FP_HS)
Sen_HS

#Calculate Sensitivity:
Sensitivity_HS <-TP_HS/sum(TP_HS+FN_HS)
Sensitivity_HS

#Calculate Precision:
Precision_HS <- TP_HS/sum(TP_HS+FP_HS)
Precision_HS

#Model 2: create and evaluate the confusion matrix(Dropping Brain_HS)
respBrain_CM_HS <- predict(TumorBrain_Annual_HS, type="response")
head(respBrain_CM_HS)
ClassBrain_CM_HS <- ifelse(respBrain_CM_HS>0.5,1,0)
head(ClassBrain_CM_HS)
TrueBrain_log_HS <-Tumor_21S_HS$Out_HS
T2Brain_HS <- table(TrueBrain_log_HS, ClassBrain_CM_HS, dnn=list("Tumor", "Predicated"))
T2Brain_HS



#Create True Positive, Negatives
TPB_HS <- T2Brain_HS[2,2]
TNB_HS <-T2Brain_HS[1,1]
FPB_HS <- T2Brain_HS[1,2]
FNB_HS <- T2Brain_HS[2,1]

#Calculate Accuracy
AccB_HS <- (TPB_HS+TNB_HS)/sum(T2Brain_HS)
AccB_HS


#Calculate Specify
SenB_HS <-(TNB_HS)/sum(TNB_HS+FPB_HS)
SenB_HS

#Calculate Sensitivity:
SensitivityB_HS <-TPB_HS/sum(TPB_HS+FNB_HS)
SensitivityB_HS

#Calculate Precision:
PrecisionB_HS <- TPB_HS/sum(TPB_HS+FPB_HS)
PrecisionB_HS


#Question 4 - Part 2
#ROC Curve - Supra_HS
plot(roc(Tumor_21S_HS$Out_HS, respSupra_CM_HS, direction="<"),
     col="red", lwd=2, main="ROC Curve for Supra, Tumor Detection")

auc(Tumor_21S_HS$Out_HS, respSupra_CM_HS)

#ROC Curve - Brain_HS
plot(roc(Tumor_21S_HS$Out_HS, respBrain_CM_HS, direction="<"),
     col="red", lwd=2, main="ROC Curve for Brain, Tumor Detection")

auc(Tumor_21S_HS$Out_HS, respBrain_CM_HS)


###################################################
##    Logistic Regression - Stepwise             ##
###################################################
#Part B
#Question 1: use the forward option in the glm function
#Part 1 and 2
startTime_HS <- Sys.time()

Tumor_GLM_HS = glm(Out_HS ~ Age_HS + Sex_HS + Bone_HS + Marrow_HS + Lung_HS
                   + Pleura_HS + Liver_HS  +Brain_HS+ Skin_HS + Neck_HS 
                   + Axil_HS + Media_HS, family = "binomial", data=Tumor_21S_HS, na.action = na.omit)
stp_Tum_GLM_HS <- step(Tumor_GLM_HS)
endTime_HS <- Sys.time()
SWTime_HS <- endTime_HS - startTime_HS

summary(stp_Tum_GLM_HS)

#Check the stepwise model
resp_CM_HS <-predict(stp_Tum_GLM_HS, type="response")
head(resp_CM_HS)
ClassSW_HS <- ifelse(resp_CM_HS>0.5, 1,0)
head(ClassSW_HS)
TrueLog_HS <- Tumor_21S_HS$Out_HS
T1_HS <-table(TrueLog_HS, ClassSW_HS, dnn=list("Tumor", "Predicated"))
T1_HS


#Create True Positive, Negatives
TP1_HS <- T1_HS[2,2]
TN1_HS <-T1_HS[1,1]
FP1_HS <- T1_HS[1,2]
FN1_HS <- T1_HS[2,1]

#Calculate Accuracy
Acc1_HS <- (TP1_HS+TN1_HS)/sum(T1_HS)
Acc1_HS


#Calculate Specify
Sen1_HS <-(TN1_HS)/sum(TN1_HS+FP1_HS)
Sen1_HS

#Calculate Sensitivity:
Sensitivity1_HS <-TP1_HS/sum(TP1_HS+FN1_HS)
Sensitivity1_HS

#Calculate Precision:
Precision1_HS <- TP1_HS/sum(TP1_HS+FP1_HS)
Precision1_HS

###################################################
##          Naïve-Bayes Classification           ##
###################################################
#Question 2:
#Part 1: Transform the variables as necessary for N-B classification
str(Tumor_21S_HS) 
Tumor_21S_HS$Out_HS <- as.factor(Tumor_21S_HS$Out_HS)
Tumor_21S_HS$Out_HS

#Part 2: All variables as necessary for N-B classification
#str(Tumor_21S_HS)
#Tumor_21S_HS$Out_HS <-as.factor(Tumor_21S_HS$Out_HS)
startTimeNB_HS <- Sys.time()
TumorNaive_HS <- NaiveBayes(Out_HS ~ Age_HS + Sex_HS + Bone_HS + Marrow_HS + Lung_HS
                            + Pleura_HS + Liver_HS  +Brain_HS+ Skin_HS + Neck_HS + Supra_HS
                            + Axil_HS + Media_HS, data=Tumor_21S_HS, na.action = na.omit)

endTimeNB_HS <- Sys.time()
NBTime_HS <- endTimeNB_HS - startTimeNB_HS


#Classifies
predBay_HS <- predict(TumorNaive_HS, Tumor_21S_HS)
head(predBay_HS)

head(predBay_HS$class)
#Creates confusion matrix
CF_NB_HS <-table(Actual = Tumor_21S_HS$Out_HS, Predicted=predBay_HS$class)
CF_NB_HS


#Create True Positive, Negatives
TP2_HS <- CF_NB_HS[2,2]
TN2_HS <-CF_NB_HS[1,1]
FP2_HS <- CF_NB_HS[1,2]
FN2_HS <- CF_NB_HS[2,1]

#Calculate Accuracy
Acc2_HS <- (TP2_HS+TN2_HS)/sum(CF_NB_HS)
Acc2_HS


#Calculate Specify
Sen2_HS <-(TN2_HS)/sum(TN2_HS+FP2_HS)
Sen2_HS

#Calculate Sensitivity:
Sensitivity2_HS <-TP2_HS/sum(TP2_HS+FN2_HS)
Sensitivity2_HS

#Calculate Precision:
Precision2_HS <- TP2_HS/sum(TP2_HS+FP2_HS)
Precision2_HS

NBTime_HS
###################################################
##          Linear Discriminant Analysis         ##
###################################################
#Part 1: Transform the variables as necessary for LDA
str(Tumor_21S_HS) 
Tumor_21S_HS$Out_HS <- as.factor(Tumor_21S_HS$Out_HS)
Tumor_21S_HS$Out_HS


#Part 2: All variables as necessary for LD classification
startTimeLD_HS <- Sys.time()
TumorDiscrim_HS <-lda(Out_HS ~ Age_HS + Sex_HS + Bone_HS + Marrow_HS + Lung_HS
                      + Pleura_HS + Liver_HS  +Brain_HS+ Skin_HS + Neck_HS + Supra_HS
                      + Axil_HS + Media_HS, data=Tumor_21S_HS, na.action = na.omit)
endTimeLD_HS <- Sys.time()
LDTime_HS <- endTimeLD_HS - startTimeLD_HS

#Classifies
predLD_HS <- predict(TumorDiscrim_HS, data= Tumor_21S_HS)
head(predLD_HS$posterior)

head(predLD_HS$class)

#Confusion Matrix
CF_LDA_HS <- table(Actual=Tumor_21S_HS$Out_HS, Predicted=predLD_HS$class)
CF_LDA_HS



#Create True Positive, Negatives
TP3_HS <- CF_LDA_HS[2,2]
TN3_HS <-CF_LDA_HS[1,1]
FP3_HS <- CF_LDA_HS[1,2]
FN3_HS <- CF_LDA_HS[2,1]

#Calculate Accuracy
Acc3_HS <- (TP3_HS+TN3_HS)/sum(CF_LDA_HS)
Acc3_HS


#Calculate Specify
Sen3_HS <-(TN3_HS)/sum(TN3_HS+FP3_HS)
Sen3_HS

#Calculate Sensitivity:
Sensitivity3_HS <-TP3_HS/sum(TP3_HS+FN3_HS)
Sensitivity3_HS

#Calculate Precision:
Precision3_HS <- TP3_HS/sum(TP3_HS+FP3_HS)
Precision3_HS

LDTime_HS

#Comparing all three
CF_NB_HS
CF_LDA_HS
T1_HS


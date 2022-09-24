
library(caret)# LOGISTIC MODEL
library(ggplot2)# VISUALIZATION
library(MASS)# VIF CALCULATION
library(car)# VIF CALCULATION
library(mlogit)# LOGISTIC MODEL
library(sqldf)#WOE & IV
library(Hmisc)#WOE & IV
library(aod)#WALD TEST
#library(BaylorEdPsych)#R-SQUARE
library(ResourceSelection)#HOSMER LEMESHOW TEST
library(pROC)#ROC CURVE
library(ROCR)#ROC CURVE
library(caTools)#TRAIN AND TEST SPLIT

------------#SETTTING WORKING DIRECTORY-------------------------------

path<-("E:/Ivyproschool/R/Logistic Regression/Case study -2")
setwd(path)
getwd()

data<-read.csv("Data_for_Logistic_Regression.csv")
data1<-data

-----------#Basic Exploration of Data------------------------------------
str(data1)
summary(data1)
dim(data1)
colnames(data1)
-------------#Changing variables into factor--------

colum_name<-c("Status_Checking_Acc","Credit_History","Purposre_Credit_Taken", "Savings_Acc","Years_At_Present_Employment"
              ,"Inst_Rt_Income","Marital_Status_Gender","Other_Debtors_Guarantors","Current_Address_Yrs","Property",
              "Other_Inst_Plans","Housing","Num_CC","Job","Dependents","Telephone","Foreign_Worker","Default_On_Payment")

data1[colum_name]<-lapply(data1[colum_name],factor)

data1<-subset(data1,select=-c(Customer_ID,Count))

--------#Outliers and missing value treatment------

data.frame(colSums(is.na(data1)))

library(graphics)

boxplot(data1$Credit_Amount)
quantile(data1$Credit_Amount,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
bench<-3972.25+1.5*IQR(data1$Credit_Amount) #3972.25 is the 75th quantile value of credit amount colum
#----values above 7882.375 are outliers so we will replace above values with 7882.375
#but from histogram its clear that values >than 12000 are extreme values hence we will replace those values with 12000
library(dplyr)
data2<-filter(data1,Credit_Amount>12000)
data2<-filter(data1,Age>64.5)
bench<-42+1.5*IQR(data1$Age)

data1$Credit_Amount<-ifelse(data1$Credit_Amount>12000,12000,data1$Credit_Amount)

mean(data1$Age)
boxplot(data1$Age)
#Age is skewd data and we will not remove outliers from it

library(ggplot2)
theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
library(ggplot2)
g <- ggplot(data1, aes(Credit_Amount))
g + geom_histogram(aes(fill=Credit_Amount),binwidth = 2)

library(ggplot2)
g <- ggplot(data1, aes(Age))
g + geom_histogram(aes(fill=Age),binwidth = 1)


#--------------------------Splitting the data into training and test data set------------------------#

set.seed(144)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data1$Default_On_Payment, 0.7)
train = subset(data1, spl == TRUE)
str(train)
dim(train)


test = subset(data1, spl == FALSE)
str(test)
dim(test)

#---------------------------Logistic Regression model building------------
#Iteration 1 --- Taking all variables-----

model1 <- glm(Default_On_Payment~., data=train, family=binomial())
summary(model1)


#Iteration 2 --- Remove: Other_Inst_PlansA142

model2 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
          +Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
          +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
          +I(Purposre_Credit_Taken=="A45")+I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
          +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")
          +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="2")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
          +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
          +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A122")+I(Property=="A123")+I(Property=="A124")
          +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="3")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
          +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model2)


#Iteration 3 --- Remove: (Purposre_Credit_Taken=="A45")

model3 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="2")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A122")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="3")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model3)

#Iteration 4 --- Remove:+I(Inst_Rt_Income=="2")

model4 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A122")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="3")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model4)
#Iteration 5 --- Remove:+I(Property=="A122")

model5 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="3")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model5)

#Iteration 6 --- Remove:+I(Num_CC=="3")

model6 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model6)

#Iteration 7 --- Remove:+I(Credit_History=="A31")

model7 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model7)


#Iteration 8 --- Remove:+I(Years_At_Present_Employment=="A72")

model8 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Num_CC=="4")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model8)

#Iteration 9 --- Remove:+I(Num_CC=="4")

model9 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model9)

#Iteration 10 --- Remove:+I(Marital_Status_Gender=="A92")

model10 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
              +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A93")+I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model10)

#Iteration 11 --- Remove:+I(Marital_Status_Gender=="A94")

model11 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
               +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A123")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model11)
#Iteration 12 --- Remove:+I(Property=="A123")

model12 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
               +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model12)
#Iteration 13 --- Remove:+I(Savings_Acc=="A63")

model13 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
               +I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model13)
#Iteration 14 --- Remove:+I(Purposre_Credit_Taken=="A46")

model14 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")+I(Job=="A174")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model14)
#Iteration 15 --- Remove:+I(Job=="A174")

model15 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")+I(Job=="A172")+I(Job=="A173")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model15)

#Iteration 16 --- Remove:+I(Job=="A172") & +I(Job=="A173")

model16 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")
               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model16)

#Iteration 17 --- Remove:+I(Purposre_Credit_Taken=="A44")

model17 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(model17)

#--------Final Train Model--------------------------------------------

final <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
summary(final)

vif(final) # housing==A153 has the highest vif

# #Removing Housing==A153
# final1 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
#              +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
#              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
#              +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
#              +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
#              +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
#              +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
#              +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
#              +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Num_CC=="2")
#              +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
# summary(final1)
# 
# #Removing insignificant variable property=A124
# final2 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
#               +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
#               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
#               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
#               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
#               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
#               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
#               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")
#               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Num_CC=="2")
#               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
# summary(final2)
# vif(final2)#Credit history==A32 has high vif
# 
# #Removing Credit_history==A32
# final3 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
#               +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
#               +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
#               +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
#               +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
#               +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
#               +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
#               +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")
#               +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Num_CC=="2")
#               +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=train, family=binomial())
# summary(final3)
# 
# vif(final3)

#------------------------------Checking the overall fitness of the model----------------------------#

#--------------->using Wald Test
wald.test(b=coef(final), Sigma= vcov(final), Terms=1:19)

#since p value is less than 0.001 hence we reject the null hypotheses that all Bi=0

#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)
#it compares our model with baselineline model and gives p values
#Ho=Final model=Baselinemodel
#Ha=Final model<> baseline model
#hence we want low pvalue to reject null hypothesis


# Difference between null deviance and deviance
modelChi <- final$null.deviance - final$deviance
modelChi
# 1158.526

#Finding the degree of freedom for Null model and model with variables
chidf <- final$df.null - final$df.residual
chidf
#37

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)
#p value is 0
#hence we accept alternate hypothesis that our final model is better than baseline model


#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(final) # deviance residuals
residuals(final, "pearson") # pearson residuals
sum(residuals(final, type = "pearson")^2)
deviance(final)

#########Larger p value indicate good model fit
1-pchisq(deviance(final), df.residual(final))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies

#####################################################################################################################
# Coefficients (Odds)
final$coefficients
# Coefficients (Odds Ratio)
exp(final$coefficients)#Interpret 

# Variable Importance of the model
varImp(final)

# Predicted Probabilities
prediction <- predict(final,newdata = train,type="response")#Classification Model
#prediction <- predict(model,newdata = data.train,probability=TRUE)
#model output is always in terms of probability
prediction

write.csv(prediction,"pred.csv")


rocCurve   <- roc(response = train$Default_On_Payment, predictor = factor(prediction, ordered =TRUE), 
                  levels = rev(levels(train$Default_On_Payment)))
train$Default_On_Payment <- as.factor(train$Default_On_Payment)
#Metrics - Fit Statistics

threshold<-as.numeric(coords(rocCurve,"best")[1])
#0.3272686

#predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
predclass <-ifelse(prediction>threshold,1,0)
#df<-data.frame(train,predclass)
Confusion <- table(Predicted = predclass,Actual = train$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
#0.7754286
Gini <-2*auc(rocCurve)-1
#0.6745572
#Gini Coefficient is the area under the Lorenz Curve (Similiar ROC Curve where final model compared to baseline model)
#Range 0.5 - 0.8

Confusion
auc(rocCurve)
#Area under the curve: 0.8373
plot(rocCurve)

#########################################################################################################################
### KS statistics calculation
train$m1.yhat <- predict(final, train, type = "response")
m1.scores <- prediction(train$m1.yhat, train$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7
#0.5255711
#----------------------------Checking the model on test data---------------------------------------

#Iteration 1
test1 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test1)

#Iteration 2 remove:+I(Years_At_Present_Employment=="A75")
test2 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test2)

#Iteration 3 remove:+I(Credit_History=="A32")
test3 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Dependents=="2")+I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test3)
#Iteration 4 remove:+I(Dependents=="2")
test4 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +Age+I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test4)

#Iteration 5 remove:+Age
test5 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test5)


#Iteration 6 remove: +I(Purposre_Credit_Taken=="A48")
test6 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Telephone=="A192")+I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test6)

#Iteration 7 remove: +I(Telephone=="A192")
test7 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test7)

#Iteration 8 remove:+I(Years_At_Present_Employment=="A73")
test8 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test8)
#Iteration 9 remove:+I(Savings_Acc=="A62")
test9 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Current_Address_Yrs=="2")+I(Current_Address_Yrs=="3")+I(Current_Address_Yrs=="4")+I(Property=="A124")
             +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test9)
#Iteration 10 remove:+I(Current_Address_Yrs=="4"),+I(Current_Address_Yrs=="3"),+I(Current_Address_Yrs=="2")
test10 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
             +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
             +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
             +I(Purposre_Credit_Taken=="A49")
             +Credit_Amount+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
             +I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
             +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A102")+I(Other_Debtors_Guarantors=="A103")
             +I(Property=="A124")
             +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
             +I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test10)

#Iteration 11 remove:+I(Other_Debtors_Guarantors=="A102")
test11 <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
              +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
              +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
              +I(Purposre_Credit_Taken=="A49")
              +Credit_Amount+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
              +I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
              +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A103")
              +I(Property=="A124")
              +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
              +I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(test11)

#------------Final test model--------------------------------------

final_test<-glm(Default_On_Payment~I(Status_Checking_Acc=="A12") +I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")
                +Duration_in_Months+I(Credit_History=="A33")+I(Credit_History=="A34")
                +I(Purposre_Credit_Taken=="A41")+I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")
                +I(Purposre_Credit_Taken=="A49")
                +Credit_Amount+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")
                +I(Years_At_Present_Employment=="A74")+I(Inst_Rt_Income=="3")+I(Inst_Rt_Income=="4")
                +I(Marital_Status_Gender=="A93")+I(Other_Debtors_Guarantors=="A103")
                +I(Property=="A124")
                +I(Other_Inst_Plans=="A143")+I(Housing=="A152")+I(Housing=="A153")+I(Num_CC=="2")
                +I(Foreign_Worker=="A202"),data=test, family=binomial())
summary(final_test)

vif(final_test)

#wald test

wald.test(b=coef(final_test), Sigma= vcov(final_test), Terms=1:15)
#Chi-squared test:
#X2 = 258.4, df = 15, P(> X2) = 0.0
#since p value is low we reject H0 that all Bi=0

#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)
#it compares our model with baselineline model and gives p values
#Ho=Final model=Baselinemodel
#Ha=Final model<> baseline model
#hence we want low pvalue to reject null hypothesis


# Difference between null deviance and deviance
modelChi <- final_test$null.deviance - final_test$deviance
modelChi
# 504.534

#Finding the degree of freedom for Null model and model with variables
chidf <- final_test$df.null - final_test$df.residual
chidf
#25

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)
#p value is 0
#hence we accept alternate hypothesis that our final model is better than baseline model

#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(final_test) # deviance residuals
residuals(final_test, "pearson") # pearson residuals
sum(residuals(final_test, type = "pearson")^2)
deviance(final_test)

#########Larger p value indicate good model fit
1-pchisq(deviance(final_test), df.residual(final_test))
#0.9974707--->high p value
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies
#####################################################################################################################
# Coefficients (Odds)
final_test$coefficients
# Coefficients (Odds Ratio)
exp(final_test$coefficients)#Interpret 

# Variable Importance of the model
varImp(final_test)

# Predicted Probabilities
prediction <- predict(final_test,newdata = test,type="response")#Classification Model
#prediction <- predict(model,newdata = data.train,probability=TRUE)
#model output is always in terms of probability
prediction

write.csv(prediction,"pred_test.csv")


rocCurve   <- roc(response = test$Default_On_Payment, predictor = factor(prediction, ordered =TRUE), 
                  levels = rev(levels(test$Default_On_Payment)))
test$Default_On_Payment <- as.factor(test$Default_On_Payment)
#Metrics - Fit Statistics

threshold<-as.numeric(coords(rocCurve,"best")[1])
# 0.2600929

predclass <-ifelse(prediction>threshold,1,0)
df<-data.frame(test,predclass)

confusionMatrix(as.factor(test$Default_On_Payment), as.factor(predclass))
sensitivity(as.factor(test$Default_On_Payment), as.factor(predclass))
#0.8996372
specificity(as.factor(test$Default_On_Payment), as.factor(predclass))
# 0.5438336
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
#0.74

Gini <-2*auc(rocCurve)-1
#0.6787872
#Gini Coefficient is the area under the Lorenz Curve (Similiar ROC Curve where final model compared to baseline model)
#Range 0.5 - 0.8

Confusion
auc(rocCurve)
#Area under the curve:0.8394
plot(rocCurve)
#########################################################################################################################
### KS statistics calculation
test$m1.yhat <- predict(final_test, test, type = "response")
m1.scores <- prediction(test$m1.yhat, test$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7
#0.523042
########################################################################################################
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}


# Creating a function for Recall
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]
  # false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

# Checking Precision and Recall on Testset
prediction <- predict(final_test,newdata = test,type="response")
predclass <- ifelse(prediction > threshold, 1, 0)
table_mat <- table(as.factor(test$Default_On_Payment), as.factor(predclass))
#create confusion matrix
confusionMatrix(as.factor(test$Default_On_Payment), as.factor(predclass))

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec
#
#             Reference
#               0   1
# Prediction 0 744 307
#            1  83 366
# 
# Accuracy : 0.74          
# 95% CI : (0.717, 0.762)
# No Information Rate : 0.5513        
# P-Value [Acc > NIR] : < 2.2e-16     
# 
# Kappa : 0.4577        
# 
# Mcnemar's Test P-Value : < 2.2e-16     
#                                         
#             Sensitivity : 0.8996        
#             Specificity : 0.5438        
#          Pos Pred Value : 0.7079        
#          Neg Pred Value : 0.8151        
#              Prevalence : 0.5513        
#          Detection Rate : 0.4960        
#    Detection Prevalence : 0.7007        
#       Balanced Accuracy : 0.7217        
#                                         
#        'Positive' Class : 0    
# precision=0.5438336
#recall=0.8151448

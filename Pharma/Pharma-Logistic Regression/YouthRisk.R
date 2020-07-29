##Objective: Factors leading to Youth Risk

#Gender = Factor with levels M and F
#Grade
#Driver License
#Drunk and Drive

#Reading excel file
library(readxl)
YouthRisk<-read_excel("C:/Users/abc/Documents/R basics/Projects/Logistics Regression/YouthRisk.xlsx")

View(YouthRisk)
YouthRisk$`Sr. No`<-NULL #Not required column
#YouthRisk$Smoker<-NULL
str(YouthRisk) ##structured Data
names(YouthRisk)
attach(YouthRisk)


YouthRisk$`Drunk driver`<-as.factor(YouthRisk$`Drunk driver`)
YouthRisk$Gender<-as.factor(YouthRisk$Gender)
YouthRisk$Smoker<-as.factor(YouthRisk$Smoker)
YouthRisk$DriverLicense<-as.factor(YouthRisk$DriverLicense)
YouthRisk$Grade<-as.numeric(YouthRisk$Grade)
YouthRisk$Age<-as.numeric(YouthRisk$Age)
str(YouthRisk)

#Check for M.V and Outliers
summary(YouthRisk)
sapply(YouthRisk, function(x) sum(is.na(x)))
boxplot(YouthRisk$Age)
boxplot(YouthRisk$Grade)

#Creating Y variable(Youth Risk) with the help of Age, Driver License and DrunkenDrive parameters
YouthRisk$age_15<-as.factor(ifelse(YouthRisk$Age<15 & YouthRisk$`Drunk driver`==1 & DriverLicense==0,1,0)) ##factor is for categorical
YouthRisk$age_17<-as.factor(ifelse(YouthRisk$Age<17 & YouthRisk$`Drunk driver`==1 & DriverLicense==0,1,0))
YouthRisk$age_18<-as.factor(ifelse(YouthRisk$Age>17 & YouthRisk$`Drunk driver`==0 & DriverLicense==1,0,1))

#calculate the bad rate
#0 - Good, 1-Bad
#Standard Benchmark -> 3% - 30%
#to select the value of Y the bad rate should fall under the standard benchmark
table(YouthRisk$age_15) ## calculates the count of each category(good and bad) in the table
306/12282 = 0.0249

table(YouthRisk$age_18)
11026/12282 = 0.89

table(YouthRisk$age_17) ## this value falls under the benchmark
1147/12282 =0.0933

View(YouthRisk)
YouthRisk$age_15<-NULL ## Removing unwanted columns(the ones we created during threshold)
YouthRisk$age_18<-NULL

#Data Partition
library(caret)
Train<-createDataPartition(YouthRisk$age_17, p=0.7, list=FALSE) #list=False means row wise
training<-YouthRisk[Train, ] #VLOOKUP in R [Row, Column]
testing<-YouthRisk[-Train, ] #Negative means not matching


## Simple Logistic Regression
logit<-glm(age_17~Grade+Gender+Smoker, data=training, family='binomial')
summary(logit)

anova(logit,test='Chisq')
Acc(logit)

##Here Y is having two categories so binomial is used
##And glm is for Logistics

summary(logit) ## In summary only the beta values will be found

#anova(logit,test='Chisq')

#To check multicolinearity
library(car)
vif(logit)

#To check Odds ratio
exp(coef(logit))

#Prediction
testing$probs<-predict(logit, testing, type='response')
testing$predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$predict, testing$age_17)
library(caret)
library(e1071)
confusionMatrix(testing$predict, testing$age_17)

#ROC Curve
library(ROCR)

##Make predictions on training set
predictTrain = predict(logit, testing, type="response")

##Prediction function
ROCRpred = prediction(predictTrain, testing$age_17)

##Performance function
ROCRperf = performance(ROCRpred,"tpr","fpr")

##Plot ROC Curve
plot(ROCRperf)
library(ROCR)
pred = prediction(testing$probs,testing$age_17)
as.numeric(performance(pred,"auc")@y.values)



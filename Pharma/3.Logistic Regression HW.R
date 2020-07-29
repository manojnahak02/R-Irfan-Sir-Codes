#Goal of the case study You are required to model the probability of attrition 
#age
#attrition = 1=yes/0=no
#BusinessTravel =Travel_Rarely/Travel_Frequently/Non-Travel
#Department = Sales/Research & Development/Human Resources
#EducationField = Life Sciences/Other/Medical/Marketing/Technical Degree/Human Resources
#Gender = Male/Female
#JobLevel = 1 to 5
#JobRole = Healthcare Representative/Research Scientist/Sales Executive/Human Resources
#     Research Director/Laboratory Technician/Manufacturing Director/Sales Representative/Manager
#MaritalStatus = Married/Single/Divorced
#MonthlyIncome = Amount 


train<-read.csv("E:/VBA/Employee1.csv")
names(train)
str(train)
summary(train)

train$Age=as.numeric(train$Age)
train$Attrition=as.factor(train$Attrition)
train$Department=as.factor(train$Department)
train$EducationField=as.factor(train$EducationField)
train$Gender=as.factor(train$Gender)
train$JobLevel<-as.factor(train$JobLevel)
train$JobRole<-as.factor(train$JobRole)
train$DistanceFromHome<-as.numeric(train$DistanceFromHome)
train$Education<-as.factor(train$Education)
train$EmployeeID <-as.numeric(train$EmployeeID)
train$EmployeeCount <-as.numeric(train$EmployeeCount)
train$NumCompaniesWorked<-as.numeric(train$NumCompaniesWorked)
train$PercentSalaryHike<-as.numeric(train$PercentSalaryHike)
train$StandardHours<-as.numeric(train$StandardHours)
train$StockOptionLevel <-as.factor(train$StockOptionLevel)
train$TotalWorkingYears <-as.numeric(train$TotalWorkingYears)
train$YearsAtCompany<-as.numeric(train$YearsAtCompany)
train$TrainingTimesLastYear<-as.numeric(train$TrainingTimesLastYear)
train$YearsSinceLastPromotion<-as.numeric(train$YearsSinceLastPromotion)
train$YearsWithCurrManager<-as.numeric(train$YearsWithCurrManager)
train$EmployeeID<-NULL
train$Over18<-NULL
train$StandardHours<-NULL
train$EmployeeCount<-NULL
str(train)

#missing vvalue and univariate analysis
sapply(train,function(x) sum(is.na(x)))
train$NumCompaniesWorked[is.na(train$NumCompaniesWorked)]= mean(train$NumCompaniesWorked,na.rm=TRUE)
train$TotalWorkingYears[is.na(train$TotalWorkingYears)]= mean(train$TotalWorkingYears,na.rm=TRUE)
sapply(train,function(x) sum(is.na(x)))
#train$Age[is.na(train$Age)]= mean(train$Age,na.rm=TRUE)
names(train)
boxplot(train$Age)
boxplot(train$DistanceFromHome)
#boxplot(train$NumCompaniesWorked) #outliears
boxplot(train$PercentSalaryHike)
boxplot(train$MonthlyIncome) #outliers
boxplot(train$YearsAtCompany) #outliears
boxplot(train$MonthlyIncome)  #out;iears
 
#summary(train$NumCompaniesWorked)
#upper<-4+1.5*IQR(train$NumCompaniesWorked);upper
#train$MonthlyIncome[train$NumCompaniesWorked>upper]<-upper
#boxplot(train$NumCompaniesWorked)
#summary(train$NumCompaniesWorked)

summary(train$DistanceFromHome)
upper<-14+1.5*IQR(train$DistanceFromHome);upper
train$MonthlyIncome[train$DistanceFromHome>upper]<-upper
boxplot(train$DistanceFromHome)
summary(train$DistanceFromHome)

summary(train$MonthlyIncome)
upper<-83800+1.5*IQR(train$MonthlyIncome);upper
train$MonthlyIncome[train$MonthlyIncome>upper]<-upper
boxplot(train$MonthlyIncome)
summary(train$MonthlyIncome)

summary(train$YearsAtCompany)
upper<-9.000+1.5*IQR(train$YearsAtCompany);upper
train$YearsAtCompany[train$YearsAtCompany>upper]<-upper
boxplot(train$YearsAtCompany)
summary(train$YearsAtCompany)


#myltiplye logistic regression

library(caret)
str(train)

summary(train)
#attrition is our Y varivale 
Train<-createDataPartition(train$Attrition,p=0.70,list=FALSE)
training<-train[Train,]
test1<-train[-Train,]
#model building
#glm for logistic
#btake attrition beacuse its our Y
#binomial beacusye Y has onmly 1 And 0
#data from Traininiig

model = glm(Attrition~.,family = 'binomial',data = training)
summary(model)

#variable signioficance selection
reg.model=step(glm(Attrition~.,family = 'binomial',data = training),direction = "both")
summary(reg.model)
anova(reg.model,test='Chisq') 
#Way of giving own refrence (Choolse lowest cont)) #giving count
table(training$Department)
table(training$EducationField)
table(training$Gender)
table(training$JobLevel)
table(training$JobRole)
table(training$MaritalStatus)

#model Building
reg.model=step(glm(Attrition~relevel(Department,ref='Human Resources')
                  +relevel(EducationField,ref = 'Human Resources')
                 +relevel(Gender,ref='Female')
                   +relevel(JobRole,ref='Human Resources')
                   +relevel(MaritalStatus,ref='Divorced')
                 +BusinessTravel+DistanceFromHome+Education+
                 +JobLevel+MonthlyIncome+NumCompaniesWorked
                  +PercentSalaryHike+StockOptionLevel
                  +TotalWorkingYears+TrainingTimesLastYear+YearsAtCompany
                 +YearsSinceLastPromotion+YearsWithCurrManager,
                   family = 'binomial',data=training)
               ,direction = 'both')
summary(reg.model)
anova(reg.model,test='Chisq')
Acc(reg.model)

#to Check multicollonearitu
library(car)
vif(reg.model) #all value less then 3 there is no inter linearity 
#to get odds Ratio
library(oddsratio)
exp(coef(reg.model))
##prdiction
test1$probs<-predict(reg.model,test1,type="response")
test1$Predict<-as.factor(ifelse(test1$probs>=0.70,1,0))
table(test1$Attrition,test1$Predict)
library(caret)
confusionMatrix(test1$Attrition,test1$Predict)

library(ROCR)
library(ggplot2)
#make prediction on test set
predictTrain = predict(reg.model,test1,type="response")
#Prediction Function
ROCRpred=prediction(predictTrain,test1$Attrition)
#performance Function
ROCRperf= performance(ROCRpred,"tpr","fpr")
# ploting Roc Curve
plot(ROCRperf)
#add color
plot(ROCRperf,colorize= TRUE)

#AUC
pred = prediction(test1$probs,test1$Attrition)
as.numeric(performance(pred,"auc")@y.values)






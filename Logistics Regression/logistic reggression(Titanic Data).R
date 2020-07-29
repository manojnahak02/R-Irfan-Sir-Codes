####Objective:
#To predict the chances of survival people travling on Titanic
#pclass: passenger class(1=1st,2=2nd)
#survival: 0=N,1=Y
#sibsp: Number of siblings/spouses Abroad
#parch:number of parents/children Abroad
#fare: ticket fare(british pound)

train<-read.csv(file.choose())
View(train)
names(train)
str(train)
##Pclass from int to categorical variable

train$pclass =as.factor(train$pclass)
train$survived<-as.factor(train$survived)
train$sibsp<-as.numeric(train$sibsp)
train$parch<-as.numeric(train$parch)
str(train)

#missing value and univariate analysis
summary(train)
sapply(train,function(x) sum(is.na(x)))

#Fill in missing values for age
train$age[is.na(train$age)]=mean(train$age, na.rm= TRUE)
names(train)

#missing value
sapply(train, function(x) sum(is.na(x)))

#treatment of outlier for fare
boxplot(train$fare)
summary(train$fare)
upper<-31+1.5*IQR(train$fare);upper
train$fare[train$fare>upper]<-upper
boxplot(train$fare)
summary(train$fare)

#treatment of outlier for age and business logic
boxplot(train$age)
summary(train$age)
upper<-34+1.5*IQR(train$age);upper
train$age[train$age> upper]<-upper
lower<-22.00-1.5*IQR(train$age);lower
train$age[train$age<lower]<-lower
boxplot(train$age)
summary(train$age)


###multiple logistic reggression
#*Always select data partition on regards to Y
data<-train[-3]#removing names column
library(caret)
Train<-createDataPartition(data$survived, p=0.7, list=FALSE)
training <-data[Train,]
test1<-data[-Train,]

##model building
model=glm(survived~.,family='binomial', data=training)
summary(model)

###Variable significance selection
reg.model=step(glm(survived~.,family='binomial',data=training),direction = "both")
summary(reg.model)
anova(reg.model,test='Chisq')

#Way of giving own reference
table(training$pclass)
table(training$sex)
table(training$embarked)

#Model building
#manual assignment of refrences
#relevel is use to create business model i.e the lowest value will assign as 
#refrence and the highest value we use to take decision
reg.model1 =step(glm(survived~relevel(pclass,ref = 2)+relevel(sex,ref = 'female')
                     +relevel(embarked,ref = 'Q')
                     +age+fare+sibsp+parch,family = 'binomial',data = training),
                direction = 'both')

summary(reg.model1)
anova(reg.model1,test='Chisq')
Acc(reg.model1)

#to check multicolinearity
library(car)
vif(reg.model1)

#All values <5 , there is no inter-relation
#To gets odds ratio
exp(coef(reg.model1))

#elevel(sex, ref = "female")male 
#0.39959648 tTO read this (male has 0.399 times chances to survive then female)       

#Prediction
test1$probs<-predict(reg.model1,test1,type='response')
test1$predict<-as.factor(ifelse(test1$probs>0.70,1,0))
table(test1$survived,test1$predict)
library(caret)
confusionMatrix(test1$survived,test1$predict)

library(ROCR)
library(ggplot2)

#Make predictions on test set
predictTrain=predict(reg.model1,test1,type="response")

#Prediction function use to generate tpr and fpr
ROCRpred=prediction(predictTrain,test1$survived)

#Performance function is used to fetch the data from preditction AUC
ROCRperf= performance(ROCRpred, "tpr", "fpr")

#Ploting ROC Curve
plot(ROCRperf)

#Add colors
plot(ROCRperf,colorize=TRUE)

#AUC
pred=prediction(test1$probs, test1$survived)
as.numeric(performance(pred,"auc")@y.values)

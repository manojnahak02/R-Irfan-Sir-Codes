marketing<-read.csv(file="D:/MAMTA/Data Set/R program/logistic/Marketing dataset.csv")
View(marketing)
summary(marketing)
str(marketing)
marketing<-marketing[-c(9,10,11,13,14,15)]
marketing$age<-as.numeric(marketing$age)
marketing$balance<-as.numeric(marketing$balance)
marketing$duration<-as.numeric(marketing$duration)

#creating dummpy vairiable
marketing$deposit<-as.factor(ifelse(marketing$deposit=="yes",1,0))
marketing$default<-as.factor(ifelse(marketing$default=="yes",1,0))
marketing$housing<-as.factor(ifelse(marketing$housing=="yes",1,0))
marketing$loan<-as.factor(ifelse(marketing$loan=='yes',1,0))
#checking for any missing values
sapply(marketing, function(x)sum(is.na(x)))
anyNA(marketing)
#treatment of outliers $age
boxplot(marketing$age)
summary(marketing$age)
upper<-49.00+1.5*IQR(marketing$age);upper
marketing$age[marketing$age>upper]<-upper
boxplot(marketing$age)
summary(marketing$age)
#treatment of outliers $balance
boxplot(marketing$balance)
summary(marketing$balance)
upper<-1708.00+1.5*IQR(marketing$balance);upper
lower<-122-1.5*IQR(marketing$balance);lower
marketing$balance[marketing$balance>upper]<-upper
marketing$balance[marketing$balance<lower]<-lower
boxplot(marketing$balance)
summary(marketing$balance)
#treatment of outliers $duration
boxplot(marketing$duration)
summary(marketing$duration)
upper<-496+1.5*IQR(marketing$duration)
marketing$duration[marketing$duration>upper]<-upper
boxplot(marketing$duration)
#data partition
library(caret)
train<-createDataPartition(marketing$deposit,p=0.70,list = FALSE)
training<-marketing[train,]
testing<-marketing[-train,]
#model building
marketingdata<-glm(deposit~.,data = training,family = "binomial")
summary(marketingdata)
#to get significant vairiable we use variable selection method
marketingdata1<-step(glm(deposit~.,family = "binomial",data=training),
                     direction = "both")
summary(marketingdata1)
#to check counts for references
table(training$job)
table(training$marital)
table(training$education)
table(training$housing)
table(training$loan)
table(training$poutcome)
table(training$deposit)
#giving our own references 
marketingdata2<-step(glm(deposit~relevel(job,ref="unknown")+
                           relevel(marital,ref="divorced")+
                           relevel(education,ref="unknown")+
                           relevel(housing,ref=1)+relevel(loan,ref="0")+
                           relevel(poutcome,ref= "other")+balance+duration,
                         family = "binomial",data=training),direction = "both")
summary(marketingdata2)
Acc(marketingdata2)
#to check multicollinearity
library(car)

vif(marketingdata2)
#to get Odds ratio
library(oddsratio)
library(vcd)
exp(coef(marketingdata2))
#prediction
testing$probs<-predict(marketingdata2,testing,type='response')
testing$predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$deposit,testing$predict)
#
(1625+904)/(136+904+682+1625)
#

#confuxion matrix
library(caret)
confusionMatrix(testing$deposit,testing$predict)
#ROCR plot and fetching tpr & fpr values
library(ROCR)
library(ggplot2)
#making predictions on test set
predictTrain=predict(marketingdata2,testing,type='response')
#prediction function
ROCRpred=prediction(predictTrain,testing$deposit)
#performance function
ROCRpref=performance(ROCRpred,'tpr','fpr')
#ploting ROC curve
plot(ROCRpref)
#Add colors
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
#AUC
pred=prediction(testing$probs,testing$deposit)
as.numeric(performance(pred,"auc")@y.values)


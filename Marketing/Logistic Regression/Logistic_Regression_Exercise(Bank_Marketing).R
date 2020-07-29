##Objective: The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution. 
#The classification goal is to predict if the client will subscribe a term deposit (variable y)

#Import data
Bank<-read.csv(file.choose())
View(Bank)
str(Bank)
names(Bank)

##STEP-1
#Creating a y variable with datatype as factor 
Bank$output<-as.factor(ifelse(Bank$y=='yes',1,0)) 
table(Bank$output)
Bank$y<-NULL
#0    1 
#3668  451 

##STEP-2
#Converting variables from int to catgorical variable
str(Bank)
Bank$age=as.numeric(Bank$age)
Bank$duration<-as.numeric(Bank$duration)
Bank$campaign<-as.numeric(Bank$campaign)
Bank$pdays<-as.numeric(Bank$pdays)
Bank$previous<-as.numeric(Bank$previous)
Bank$job<-as.factor(Bank$job)
Bank$marital<-as.factor(Bank$marital)
Bank$education<-as.factor(Bank$education)
Bank$default<-as.factor(Bank$default)
Bank$housing<-as.factor(Bank$housing)
Bank$loan<-as.factor(Bank$loan)
Bank$contact<-as.factor(Bank$contact)
Bank$poutcome<-as.factor(Bank$poutcome)
str(Bank)

##STEP-3
#missing value and univariate analysis
summary(Bank)
sapply(Bank, function(x) sum(is.na(x)))

#Check for outliers 
boxplot(Bank) # outliers in exist
boxplot(Bank$age)
boxplot(Bank$duration)
boxplot(Bank$campaign)
boxplot(Bank$pdays)
boxplot(Bank$previous)
boxplot(Bank$cons.conf.idx)

#treatement of outlier 
##age
boxplot(Bank$age)
summary(Bank$age)
upper<-47+1.5*IQR(Bank$age); upper
Bank$age[Bank$age>upper]<-upper
boxplot(Bank$age)
summary(Bank$age)

##duration
boxplot(Bank$duration)
summary(Bank$duration)
upper<-317+1.5*IQR(Bank$duration); upper
Bank$duration[Bank$duration>upper]<-upper
boxplot(Bank$duration)
summary(Bank$duration)

##campaign
boxplot(Bank$campaign)
summary(Bank$campaign)
upper<-3+1.5*IQR(Bank$campaign); upper
Bank$campaign[Bank$campaign>upper]<-upper
boxplot(Bank$campaign)
summary(Bank$campaign)

##pdays
boxplot(Bank$pdays)
summary(Bank$pdays)
#vizorizing technique fails
#therefore replace it with mean
Bank$pdays<-0.2277
View(Bank)

##previous
boxplot(Bank$previous)
summary(Bank$previous)
Bank$previous<-0.1903
View(Bank)

##cons.conf.idx
boxplot(Bank$cons.conf.idx)
summary(Bank$cons.conf.idx)
upper<--36.4+1.5*IQR(Bank$cons.conf.idx); upper
Bank$cons.conf.idx[Bank$cons.conf.idx>upper]<-upper
boxplot(Bank$cons.conf.idx)
summary(Bank$cons.conf.idx)

##STEP-4
##Data partition
library(caret)
Train<-createDataPartition(Bank$output, p=0.7, list=FALSE) #response variable is 'outcome'
training<-Bank[Train,]
testing<-Bank[-Train,]

##STEP-5
##Multiple Logistics regression
#model building
model=glm(output~., family = 'binomial', data = training)
summary(model)

##ACC function
Acc=function(model){
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100
  return(list("Percent Concordance"=PercentConcordance,"Percent Discordance"=PercentDiscordance,"Percent Tied"=PercentTied,"Pairs"=Pairs))
}

Acc(model)
##93% Concordance, hence accuracy=93%

##variable significance selection
model1=step(glm(output~., family='binomial', data = training), direction="both")
summary(model1)
anova(model1, test='Chisq') 
Acc(model1)
##accuracy=93%, no difference

#to check multicollinearity
library(car)
vif(model1)
#VIF values are <5 thus no multicolinearity

##STEP-6
#Finding odds ratio
exp(coef(model1)) 

##STEP-7
##Prediction
testing$probs<-predict(model1, testing, type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$output, testing$Predict) 

##STEP-8
#confusion matrix
library(caret)
confusionMatrix(testing$output, testing$Predict)
#Accuracy: 91%
library(ROCR)
library(ggplot2)

##STEP-9
##ROCR curve
##Make predictions on the test data
predictTrain=predict(model1, testing, type="response") 
#Prediction function
ROCRpred=prediction(predictTrain, testing$output)
#Performance function
ROCRperf=performance(ROCRpred, "tpr", "fpr")
##ploting ROC curve
plot(ROCRperf)
#Add colors
plot(ROCRperf, colorize=TRUE)

##STEP-10
#AUC
pred=prediction(testing$probs, testing$output)
as.numeric(performance(pred,"auc")@y.values)
#93%

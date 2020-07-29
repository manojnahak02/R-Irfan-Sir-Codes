## Objective: To determine if a given bank customer will leave or not?
## Columns
##RowNumber
##CustomerId
##Surname
##CreditScore
##Geography -> France, Germany and Spain
##Gender
##Age
##Tenure
##Balance
##NumOfProducts -> How many accounts, bank account affiliated products the person has
##HasCrCard
##IsActiveMember
##EstimatedSalary
##Exited -> Did they leave the bank after all? 0 - Stayed, 1 - Churned

bankchurn <- read.csv("E:/banking data set/Bank Churn_Modelling.csv")
names(bankchurn)  ## headers of data type
str(bankchurn)  ## structure of data

## Conversion to Factor and numerical Data type
bankchurn$RowNumber <- as.numeric(bankchurn$RowNumber)
bankchurn$CustomerId <- as.numeric(bankchurn$CustomerId)
bankchurn$CreditScore <- as.numeric(bankchurn$CreditScore)
bankchurn$Geography <- as.factor(bankchurn$Geography)
bankchurn$Gender <- as.factor(bankchurn$Gender)
bankchurn$Age <- as.numeric(bankchurn$Age)
bankchurn$Tenure <- as.factor(bankchurn$Tenure)
bankchurn$NumOfProducts <- as.factor(bankchurn$NumOfProducts)
bankchurn$HasCrCard <- as.factor(bankchurn$HasCrCard)
bankchurn$IsActiveMember <- as.factor(bankchurn$IsActiveMember)
bankchurn$Exited <- as.factor(bankchurn$Exited)

str(bankchurn)

## Missing Values check
summary(bankchurn)
sapply(bankchurn, function(x) sum(is.na(x))) ## No missing Values

## Outlier Check and treatment

boxplot(bankchurn$CreditScore) ## Only lower outlier
summary(bankchurn$CreditScore)
lower <- 584 - 1.5*IQR(bankchurn$CreditScore) ; lower
bankchurn$CreditScore[bankchurn$CreditScore < lower] <- lower
boxplot(bankchurn$CreditScore)

boxplot(bankchurn$EstimatedSalary)
boxplot(bankchurn$Balance)
boxplot(bankchurn$Age)

summary(bankchurn$Age)
upper <- 44 + 1.5*IQR(bankchurn$Age) ; upper
bankchurn$Age[bankchurn$Age > upper] <- upper
boxplot(bankchurn$Age)


## removing column 1, 2 and 3 (ROw number and customer ID are unique identifiers while column 3 is surname)
data <- bankchurn[-(1:3)]

## Creating data partition
library(caret)
Train <- createDataPartition(data$Exited, p = 0.7, list = FALSE) 
training <- data[Train,]  
test <- data[-Train,]

# Model Building
model = glm(Exited~., family = 'binomial', data = training)
summary(model) ## As per summary, Credit Score, Geography, Gender, Age, NumofProducts, HasCrCard and is active member has p value less than alpha
anova(model, test ='Chisq')
Acc(model)

# Variable Selection
reg.model <- step(glm(Exited~., family = 'binomial', data = training)
                  ,direction = "both")
summary(reg.model)
anova(reg.model, test ='Chisq')

Acc(reg.model)
# To check Multi Colinearity
library(car)
vif(reg.model)
# to get odds ratio
library(oddsratio)
##library(vcd)
exp(coef(reg.model))

## Readings
## Gender: Probability of Male churing is 0.55 times as compare to female, so female exiting chances are more
# Geography: Probability of Germany customer churing is 2.6 times more than France
## Geography : Probability of Spain customer churing is 1.06 times as France

test$probs <- predict(reg.model, test, type = 'response')
test$Predict <- as.factor(ifelse(test$probs > 0.70,1,0))
table(test$Exited, test$Predict)
library(caret)
confusionMatrix(test$Exited, test$Predict) ## 83% Accuracy

library(ROCR)
library(ggplot2)
## make predictions on test set
predictTrain = predict(reg.model, test, type ="response")
## prediction function
ROCRpred = prediction(predictTrain, test$Exited)
# performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
# plotting ROC curve
plot(ROCRperf)

#AUC
pred = prediction(test$probs, test$Exited)  ## Prediction used to calculate TPR, FPR, AUC etc 
as.numeric(performance(pred, "auc")@y.values)  ## AUC for all the values of Y, calculating area under the curve

## 82.7% Area under curve, so its a good fit model


## Changing the refrence in the model
reg.model1 = step(glm(Exited ~relevel(Gender, ref ="Male")
                      +relevel(Geography, ref = "Germany")
                      +Age+CreditScore+Tenure+Balance+NumOfProducts+HasCrCard+IsActiveMember+EstimatedSalary,
                      family = 'binomial', data = training)
                  , direction = "both")
summary(reg.model1)  ## Embarked p values are less than alpha for C & S, however Q is added in intercept since its refernce
##However, intercept is less than alpha, so embarked will also be significant, since even if one level is less that alpha, it will become significant
anova(reg.model1, test = 'Chisq')
Acc(reg.model1)
library(car)
vif(reg.model)
# to get odds ratio
library(oddsratio)
library(vcd)
exp(coef(reg.model1))

## Readings
## Gender: Probability of Female churing is 1.76 times mpore as compare to male,
# Geography: Probability of France customer churing is 0.397 times more than Germany
## Geography : Probability of Spain customer churing is 0.427 times as Germany

## Applying SVM model on the same data set

svmdata <- bankchurn[-(1:3)]
#creating test and training datasets
library(caret)
svmtrain <- createDataPartition(svmdata$Exited , p = 0.7, list = FALSE)
svmtraining <- svmdata[svmtrain, ]
svmtesting <- svmdata [-svmtrain, ]
library(e1071)
tune.out = tune(svm, Exited ~., data = svmtraining, kernel ="linear",
                ranges = list(cost = c(0.01, 0.1, 0.2,0.3)))
summary(tune.out)

## Best model
bestmod = tune.out$best.model ## best value/ model will get automatically will be by default saved in Best.model
summary(bestmod)

## accuracy of model on training data comes out to be 90%
confusionMatrix(bestmod$fitted, svmtraining$Exited)

#prediction on testing
ypred = predict(bestmod, svmtesting)
## accuracy of model on test data
confusionMatrix(ypred, svmtesting$Exited)
table(predict = ypred , truth = svmtesting$Exited)







## Descriptive statistics
ch<-table(bankchurn$Exited)
slice<-table(bankchurn$Exited)
pct<-round(slice/sum(slice)*100)
lbls<-paste(names(ch),pct,sep=" or ")
lbls<-paste(lbls,"%")
pie(ch,labels=lbls,main="Churned and Not Churned Customers [0:Stayed and 1:Churned]")

library(ggplot2)
ggplot(bankchurn,aes(x=Exited, fill=Geography)) +
  geom_density(col=NA,alpha=0.35)
ggplot(bankchurn,aes(x=Exited, fill=Gender)) +
  geom_density(col=NA,alpha=0.35)

# objective:-to predict the patient is suffering from diabetes or not

data<-read.csv(file.choose())
data$Outcome<-as.factor(data$Outcome)

#data partition
library(caret)
Train<-createDataPartition(data$Outcome,p=.7,list = FALSE)
training<-data[Train,]
testing<-data[-Train,]

#model
library(e1071)
svm_model1<-svm(Outcome~.,data=training)
summary(svm_model1)
training$fitted<-svm_model1$fitted

#accuracy on Training data set
#table (ypred, training$outcome)
confusionMatrix(svm_model1$fitted,training$Outcome)

#accuracy on testing data set 7783
ypred=predict(svm_model1,testing)
#table(ypred,testing$outcomes)
confusionMatrix(ypred,testing$Outcome)

#deleting column
training$fitted<-NULL

#second model
svm_model2 <-svm(Outcome~.,data=training,kernel="linear")
summary(svm_model2)

#accuracy on Training dataset
confusionMatrix(svm_model2$fitted,training$Outcome)

#accuracy on testing data set 7783
ypred1=predict(svm_model2,testing)
#table (ypred1,testing$outomes)
confusionMatrix(ypred1,testing$Outcome)


library(caret)
library(e1071)

heart_df<-read.csv(file.choose())

heart_df$target<-as.factor(heart_df$target)
heart_df$Sex<-as.factor(heart_df$Sex)
heart_df$CP<-as.factor(heart_df$CP)
heart_df$Fbs<-as.factor(heart_df$Fbs)
heart_df$ECG<-as.factor(heart_df$ECG)
heart_df$exang<-as.factor(heart_df$exang)
heart_df$thal<-as.factor(heart_df$thal)
heart_df$Age<-as.factor(heart_df$Age)
heart_df$Trestbps<-as.factor(heart_df$Trestbps)
heart_df$Chol<-as.factor(heart_df$Chol)
heart_df$Thalach<-as.factor(heart_df$Thalach)
heart_df$oldPeak<-as.factor(heart_df$oldPeak)
heart_df$slope<-as.factor(heart_df$slope)
heart_df$ca<-as.factor(heart_df$ca)

str(heart_df)

#creating test and train dataset
intrain<-createDataPartition(heart_df$target,p=.7,list = FALSE)
training<-heart_df[intrain,]
testing<-heart_df[-intrain,]

#summary and checking missing value
summary(training)
sapply(training,function(x) sum(is.na(x)))

#building model with cost
svm_model1<-svm(target~.,data=training,cost=0.01,scale=FALSE)
summary(svm_model1)

#accuracy of model on training data
confusionMatrix(svm_model1$fitted,training$target)

#prediction
testing_pred<-predict(svm_model1,testing)

#accuracy of model on test data
confusionMatrix(testing_pred,testing$target)


#how to increase the accuracy of model =0.1 or 100
#kernel is directly affect the accuracy of model
tune.out=tune(svm,target~.,data=training, kernel="linear",
              ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

#best model 
bestmod=tune.out$best.model
summary(bestmod)

#accuracy of model on traning data
confusionMatrix(bestmod$fitted,training$target)

#prediction on testing data
ypred=predict(bestmod,testing)
table(predict=ypred,truth=testing$target)

#accuracy of model on test data
confusionMatrix(ypred,testing$target)


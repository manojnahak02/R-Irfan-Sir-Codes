data<-read.csv(file.choose())
nrow(data)
library(caret)
Train<-createDataPartition(data$diagnosis,p=0.7,list = FALSE)
training<-data[Train,]
testing<-data[-Train,]
library(e1071)
levels(data$diagnosis)
model<- naiveBayes(diagnosis~.,data=training) 
pred<-predict(model,testing)
#Predicted Y count
table(pred)
pred 
#Actual Y count
table(testing$diagnosis)
confusionMatrix(testing$diagnosis,pred)
library(scales)
acc<-percent((105+55)/(105+2+8+55))
acc

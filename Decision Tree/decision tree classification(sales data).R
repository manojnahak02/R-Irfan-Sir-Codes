library(tree)
#create a dependance variable base on sales
saledata<-read.csv(file.choose())
summary(saledata)
saledata$High=as.factor(ifelse(saledata$Sales<=8,0,1))
saledata$Sales<-NULL
data<-saledata
rm(saledata)
sapply(data, function(x)sum(is.na(x)))
#data partition
library(caret)
split<-createDataPartition(y=data$High,p=0.7,list = FALSE)
train<-data[split,]
test<-data[-split,]

#building model
tree.data=tree(High~.,data=train)
plot(tree.data)
text(tree.data,pretty=0)
tree.data
tree.pred=predict(tree.datatest,type="class")
table(tree.pred,test$High)
library(caret)
confusionMatrix(tree.pred,test$High)
#the function cv.tree()perform cross validation in order to
#cv.tree()
#determine the optiml level of tree complexity
#cost complexity pruning is used in oreder to select a 
#sequence of trees for consideration.
#we use to argument FUN=prune.misclass in order to indices
#that we want the
#classification error rate to guide the cross validation
cv.data=cv.tree(tree.data,FUN=prune.misclass)
names(cv.data)
plot(cv.data$size,cv.data$dev,type = "b")
prune.data<-prune.misclass(tree.data,best=16)
plot(prune.data)
text(prune.data,pretty=0)
tree.pred=predict(prune.data,test,type="class")
table(tree.pred,test$High)
confusionMatrix(tree.pred,test$High)

#changing the cost function and check the performance
prune.data1<-prune.misclass(tree.data,best=9)
plot(prune.data1)
text(prune.data1,pretty=0)
tree.pred1=predict(prune.data1,test,type="class")
confusionMatrix(tree.pred1,test$High)




      
library(knitr)
library(tree)
baseball<-read.csv(file="D:\\MAMTA\\Data Set\\R program\\decision tree\\")
summary(baseball)
sapply(baseball, function(x)sum(is.na(x)))
#model building
tree.fit<-tree(Salary~Hits+Year,data=baseball)
summary(tree.fit)

#the argument pretty=0 instruct R to include the category names
#for any qualitative prediction
text(tree.fit,pretty=0)
tree.fit

#prediction

yhat<-predict(tree.fit,baseball)
Hit<-data.frame(baseball,yhat)
mean((Hit$Salary-Hit$yhat)^2)

#another way manual way
Hit$residual<-Hit$Salary-hit$yhat
Hit$residual1<-(Hit$residual)^2
sum(Hit$residual1)/263
mean((Hit$Salary-Hit$yhat)^2)

#how to increase the accuracy of the model and decrease the mse 
#log transfer salary to make it a bit more nrmally distributed
baseball<-read.csv(file.choose())
hist(baseball$Salary)
baseball$Salary<-log(baseball$Salary)
hist(baseball$Salary)
tree.fit<-tree(Salary~Hits+Years,data=baseball)
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty=0)
tree.fit

#prediction
yhat<-predict(tree.fit,baseball)
Hit1<-data.frame(baseball,yhat)
mean((Hit1$Salary-Hit$yhat)^2)


#another way of doing analysis
#tree pruing
#log transfermation salary to make it a bit more normally distributed
baseball<-read.csv(file.choose())
hist(baseball$Salary)
library(caret)
split<-createDataPartition(y=baseball$Salary,p=0.7,list=FALSE)
train<-baseball[split,]
test<-baseball[-split,]

#create tree model
trees<-tree(Salary~.,data=train)
plot(trees)
text(trees,pretty=0)

#prediction
yhat<-predict(trees,test)
Hit1<-data.frame(test,yhat)
mean((Hit1$Salary- Hit1$yhat)^2)

#cross validation to see whether pruning the tree will improve performance
cv.trees<-cv.tree(trees)
#Hit1<-data.frame(test,yhat)
#mean((Hit1$Salary- Hit1$yhat)^2)
plot(cv.trees$size,cv.trees$dev,type = "b")
prune.trees<-prune.tree(trees,best=4)
plot(prune.trees)
text(prune.trees,pretty=0)

#prediction
yhat<-predict(prune.trees,test)
Hit2<-data.frame(test,yhat)
mean((Hit2$Salary- Hit2$yhat)^2)

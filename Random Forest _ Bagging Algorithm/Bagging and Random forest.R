library(randomForest)
library(tree)
library(MASS)

#data importing
Boston<- read.csv("E:\\Imarticus DSP 24\\R Programs\\Bagging and Random forest\\Boston.csv")

#Data partition
library(caret)
split<- createDataPartition(y=Boston$medv, p=0.7, list=FALSE)
train<- Boston[split,]
test<- Boston[-split,]

# #Bagging Model
# When you specify mtry (say 10), it takes 10 random variables 
# from your data set and examines them for one tree. So the next tree 
# would take 10 more random variables, examine them, so on and so for
# the until it runs through the ntrees that you specify and then returns 
# the average estimates for the best/most important variables
bag.boston = randomForest(medv~., data = train, mtry=13, importance=TRUE)

bag.boston
plot(bag.boston)

#Prediction
yhat<- predict(bag.boston, test)
Hit<- data.frame(test, yhat)
mean((Hit$medv-Hit$yhat)^2)

#manually specifying no. of tree should be grown
bag.boston1 = randomForest(medv~., data= train, mtry=13, ntree=3)
plot(bag.boston1)

#Prediction
yhat1<- predict(bag.boston1, test)
Hit1<-data.frame(test, yhat1)
mean((Hit1$medv-Hit1$yhat1)^2)


# #By default, random forest() uses p/3 variables
# when building a random forest of regression trees,
# and sqrt(p) variables when building a random forest of classification trees.
#Here we use mtry = 5.

rf.boston = randomForest(medv~., data= train, mtry=5, importance= TRUE)
rf.boston
yhat.rf = predict(rf.boston, test)
Hit3 <- data.frame(test, yhat.rf)
mean((Hit3$medv-Hit3$yhat.rf)^2)
a<-data.frame(importance(rf.boston))
a<-a[order(-a$X.IncMSE),];a
varImpPlot(rf.boston, colors='Red')







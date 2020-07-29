my<-read.csv("F:/R and Data Science/Bayes and Navie bayes theorem/Stolen data.csv")
library(e1071)
a<-naiveBayes(Stolen~., data=my)
a
b<-read.csv("F:/R and Data Science/Bayes and Navie bayes theorem/Stolen  data test.csv")
ab<-predict(a,b,"raw")
b1<-data.frame(cbind(ab[,2],b))


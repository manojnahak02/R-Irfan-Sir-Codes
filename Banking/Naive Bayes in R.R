naive <- read.xlsx("E:/Paras - DSP 24 Data/Naive Bayes.xlsx")
names(naive)
str(naive)  

## Missing Values check
summary(naive)
sapply(naive, function(x) sum(is.na(x))) ## No missing Values
library(e1071)
attach(naive)
a<-naiveBayes(Loan.sanctioned~.,data = naive)
a

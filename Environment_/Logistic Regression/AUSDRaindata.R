#Objective - Is to predict whether there would be chances of rainfall tomorrow or not
data<-read.csv(file.choose())
names(data)
str(data)

data=na.omit(data)


data$WindGustDir<-as.numeric(data$WindGustDir)
str(data)

data$WindGustSpeed<-as.numeric(data$WindGustSpeed)
str(data)
data$Date<-NULL
data$Location<-NULL
data$RainToday<-NULL

#data$RainToday<-as.factor(ifelse(data$RainToday=="Yes",1,0))
#data$RainToday<-as.numeric(data$RainToday)
data$RainTomorrow<-as.factor(ifelse(data$RainTomorrow=="Yes",1,0))
str(data)

sapply(data,function(x) sum(is.na(x)))

data$MinTemp[is.na(data$MinTemp)] = mean(data$MinTemp, na.rm = TRUE)
data$MaxTemp[is.na(data$MaxTemp)] = mean(data$MaxTemp, na.rm = TRUE)
data$Rainfall[is.na(data$Rainfall)] = mean(data$Rainfall, na.rm = TRUE)
data$Evaporation[is.na(data$Evaporation)] = mean(data$Evaporation, na.rm = TRUE)
data$Sunshine[is.na(data$Sunshine)] = mean(data$Sunshine, na.rm = TRUE)
data$WindGustDir[is.na(data$WindGustDir)] = mean(data$WindGustDir, na.rm = TRUE)
data$WindGustSpeed[is.na(data$WindGustSpeed)] = mean(data$WindGustSpeed, na.rm = TRUE)
#data$RainToday[is.na(data$RainToday)] = mean(data$RainToday, na.rm = TRUE)
names(data)
str(data)
sapply(data,function(x) sum(is.na(x)))

boxplot(data$MinTemp)
summary(data$MinTemp)
upper<-17+1.5*IQR(data$MinTemp);upper
data$MinTemp[data$MinTemp>upper]<-upper
lower<-7.90-1.5*IQR(data$MinTemp);lower
data$MinTemp[data$MinTemp<lower]<-lower
boxplot(data$MinTemp)
summary(data$MinTemp)


boxplot(data$MaxTemp)
summary(data$MaxTemp)
upper<-28.80+1.5*IQR(data$MaxTemp);upper
data$MaxTemp[data$MaxTemp>upper]<-upper
lower<-18.20-1.5*IQR(data$MaxTemp);lower
data$MaxTemp[data$MaxTemp<lower]<-lower
boxplot(data$MaxTemp)
summary(data$MaxTemp)



boxplot(data$Rainfall)
summary(data$Rainfall)
upper<-0.60+1.5*IQR(data$Rainfall);upper
data$Rainfall[data$Rainfall>upper]<-upper
boxplot(data$Rainfall)
summary(data$Rainfall)

boxplot(data$Evaporation)
summary(data$Evaporation)
upper<-5.87+1.5*IQR(data$Evaporation);upper
data$Evaporation[data$Evaporation>upper]<-upper
lower<-5.00-1.5*IQR(data$Evaporation);lower
data$Evaporation[data$Evaporation<lower]<-lower
boxplot(data$Evaporation)
summary(data$Evaporation)





boxplot(data$Sunshine)
summary(data$Sunshine)
upper<-7.27+1.5*IQR(data$Sunshine);upper
data$Sunshine[data$Sunshine>upper]<-upper
lower<-7.27-1.5*IQR(data$Sunshine);lower
data$Sunshine[data$Sunshine<lower]<-lower
boxplot(data$Sunshine)
summary(data$Sunshine)



boxplot(data$WindGustDir)
boxplot(data$WindGustSpeed)
summary(data$WindGustSpeed)
upper<-46.00+1.5*IQR(data$WindGustSpeed);upper
data$WindGustSpeed[data$WindGustSpeed>upper]<-upper
lower<-31.00-1.5*IQR(data$WindGustSpeed);lower
data$WindGustSpeed[data$WindGustSpeed<lower]<-lower
boxplot(data$WindGustSpeed)
summary(data$WindGustSpeed)



#boxplot(data$RainTomorrow)

library(caret)
Train<-createDataPartition(data$RainTomorrow,p=0.7,list=FALSE)
training<-data[Train,]
testing<-data[-Train,]

reg.model = step(glm(RainTomorrow~.,family='binomial',data=training ), direction="both")
summary(reg.model)
anova(reg.model,test='Chisq')
Acc(reg.model)

library(car)
vif(reg.model) #if value is lt 5 means there is no inter co relation

library(oddsratio)
library(vcd)
exp(coef(reg.model))

testing$probs<-predict(reg.model,testing,type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))
#confusion matrix creation y and dat prediction
table(testing$RainTomorrow,testing$Predict)
library(caret)
#the count of 00 and 11 should match the total count 
confusionMatrix(testing$RainTomorrow,testing$Predict)



library(ROCR)
library(ggplot2)

#Make prediction on test set
predictTrain=predict(reg.model,testing,type="response")
#Prediction function
ROCRpred = prediction(predictTrain,testing$RainTomorrow)
#Performance function
ROCRpref=performance(ROCRpred,"tpr","fpr")

#ploting ROCR curve
plot(ROCRpref)
#Add colors
plot(ROCRpref,colorize=TRUE)

#AUC
pred=prediction(testing$probs,testing$RainTomorrow)
as.numeric(performance(pred,"auc")@y.values)




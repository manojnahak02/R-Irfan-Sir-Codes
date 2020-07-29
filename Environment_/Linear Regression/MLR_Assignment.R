## OBJECTIVE: To find out the whether there is a relation between PM10 and other pollutants.

# To select the file
data1<-read.csv(file.choose())
#To show the data
View(data1)
#To show the header of each column
names(data1)
#Check the data type for each column
str(data1)
#converting Zn to num as it's data type is int
data1$Zn<-as.numeric(data1$Zn)
str(data1)

#to check outlier for each variables
boxplot(data1$Pb)
boxplot(data1$Cd)
boxplot(data1$Cu)
boxplot(data1$Cr)
boxplot(data1$Zn)
boxplot(data1$NOx)
boxplot(data1$SO2)
boxplot(data1$PM10)

#treatment of outlier for PM
summary(data1$PM10)

upperPM<-89.82+1.5*IQR(data1$PM10);upperPM
data1$PM10[data1$PM10>upperPM]<-upperPM
boxplot(data1$PM10)
summary(data1$PM10)


#treatment of outlier for Pb
summary(data1$Pb)

upperPb<-0.96+1.5*IQR(data1$Pb);upperPb
data1$Pb[data1$Pb>upperPb]<-upperPb
boxplot(data1$Pb)
summary(data1$Pb)


#treatment of outlier for Cd
summary(data1$Cd)

upperCd<-0.00+1.5*IQR(data1$Cd);upperCd
data1$Cd[data1$Cd>upperCd]<-upperCd
boxplot(data1$Cd)
summary(data1$Cd)

#treatment of outlier for Cu
summary(data1$Cu)

upperCu<-0.53+1.5*IQR(data1$Cu);upperCu
data1$Cu[data1$Cu>upperCu]<-upperCu
boxplot(data1$Cu)
summary(data1$Cu)


#treatment of outlier for Cr
summary(data1$Cr)

upperCr<-0.58+1.5*IQR(data1$Cr);upperCr
data1$Cr[data1$Cr>upperCr]<-upperCr
boxplot(data1$Cr)
summary(data1$Cr)

#treatment of outlier for Nox
summary(data1$NOx)

upperNOx<-54.70+1.5*IQR(data1$NOx);upperNOx
data1$NOx[data1$NOx>upperNOx]<-upperNOx
boxplot(data1$NOx)
summary(data1$NOx)

#To check if the outlier can be removed with quantile method for NOx
outputQuantile<-quantile(data1$NOx,seq(0,1,by=0.05))
outputQuantile
cbind(outputQuantile)
qn = quantile(data1$NOx,c(0.01,0.99),na.rm = TRUE)
df = within(data1,{NOx = ifelse(NOx>qn[2],qn[2],NOx)})
summary(data1)
summary(df)


#As quantile method didn't worked we replace the outlier by mean
NoxMean=mean(data1$NOx)
NoxMean


for (i in 1:length(data1$NOx)) {
  if(data1$NOx[i]=="98.14"){
    data1$NOx[i] <- 42.50
    
  } 
}

summary(data1)

#As the outlier 98.14 value has been replaced by the mean of NOx, we would again run treatment for NOx outlier
upperNOx<-53.09+1.5*IQR(data1$NOx);upperNOx
data1$NOx[data1$NOx>upperNOx]<-upperNOx
boxplot(data1$NOx)
summary(data1$NOx)

#To remove Cd and Zn column as it has value 0 and wouldn't have any effect on data
data1$Cd<-NULL
data1$Zn<-NULL

#data partition
library(caret)

Train<-createDataPartition(data1$PM10,p=0.70,list=FALSE)
training<-data1[Train,]
testing<-data1[-Train,]



#check collinearity
library(corrplot)
corrplot(cor(training),method='number')
model<-lm(PM10~.,data=training)
summary(model)
#hist(training$PM10)


#transformation to have bell shaped curve as the graph was left skewed
#hist((1/training$PM10))
#hist(log(training$PM10))



library(lmtest)
library(car)
model2<-step(lm(log(PM10)~.,data = data1),direction = "both")

summary(model2)
vif(model2)
par(mfrow=c(2,2))
plot(model2)

dwtest(model2)

#library(car)
ncvTest(model2)

#Prediction
#predict as we have took log for charges so to do antilog ww use exp function - we always submit the origibnal value
#testing$fitted<-predict(model2,testing)
#testing$original<-exp(testing$fitted)

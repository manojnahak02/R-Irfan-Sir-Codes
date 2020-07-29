## OBJECTIVE :- IS to predict whether the CPU performance is dependent on its Machine cycle time ,
##, main memory,cache memory & channels 



#reading data
data <-read.csv('C:/Users/Admin/Desktop/DSP_R Studio/MLR_exm2_test.csv')

#Step1.1: Data type
summary(data)
names(data)  #Returns Columnname
str(data)    #Check datatype...For MLR check whether all var. have number datatype

#All var. are of type int so convert it to numeric
data$Performance <- as.numeric(data$Performance)
data$Machine.Cycle.Time.ns <- as.numeric(data$Machine.Cycle.Time.ns)
data$Main.Memory.kb <- as.numeric(data$Main.Memory.kb)
data$Cache.Memory.kb <- as.numeric(data$Cache.Memory.kb)
data$Channels <- as.numeric(data$Channels)
str(data)

#Step1.2
#Identification of x & y
#Here y - performance
# x - Machine.Cycle.Time.ns,Main.Memory.kb,Cache.Memory.kb,Channels

#Step 1.3
## H0 :- There is no linear relationship between CPU performance and its Machine cycle time ,
##, main memory,cache memory & channels

##H(alt1) :- There is no linear relationship between CPU performance and its Machine cycle time ,
##, main memory,cache memory & channels



#Step2 - Data processing 
#step2.1 - check null/missing values,outliers
boxplot(data)  
boxplot(data$Machine.Cycle.Time.ns) 
boxplot(data$Main.Memory.kb)
boxplot(data$Cache.Memory.kb)
boxplot(data$Channels)
boxplot(data$Performance)

#step2.2 - Treatment of outliers
#1.Machine.Cycle.Time.ns 
summary(data$Machine.Cycle.Time.ns)

# upper <- Q3+1.5*IQR(Q3-Q1)
upper <- 225.0 +1.5*IQR(data$Machine.Cycle.Time.ns);upper

#if Machine.Cycle.Time.ns> upper value it will be replaced with upper value
#and then it is stored in column "Machine.Cycle.Time.ns" itself
data$Machine.Cycle.Time.ns[data$Machine.Cycle.Time.ns > upper] <- upper
boxplot(data$Machine.Cycle.Time.ns)
summary(data$Machine.Cycle.Time.ns)

#2.Main.Memory.kb
summary(data$Main.Memory.kb)
upper <- 16000 +1.5*IQR(data$Main.Memory.kb);upper
data$Main.Memory.kb[data$Main.Memory.kb > upper] <- upper
boxplot(data$Main.Memory.kb)
summary(data$Main.Memory.kb)

#3.Cache.Memory.kb
summary(data$Cache.Memory.kb)
upper <- 32 +1.5*IQR(data$Cache.Memory.kb);upper
data$Cache.Memory.kb[data$Cache.Memory.kb > upper] <- upper
boxplot(data$Cache.Memory.kb)
summary(data$Cache.Memory.kb)

#4.Channels
summary(data$Channels)
upper <- 24 +1.5*IQR(data$Channels);upper
data$Channels[data$Channels > upper] <- upper
boxplot(data$Channels)
summary(data$Channels)

#5.Performance
summary(data$Performance)
upper <- 113.0 +1.5*IQR(data$Performance);upper
data$Performance[data$Performance > upper] <- upper
boxplot(data$Performance)
summary(data$Performance)

#Step 3 : Data partition - Training(70%) & Testing(30%)
library(caret)
#Train <- createDataPartition(dataset$columnname,% of data to be taken,partitioning data row wise)
Train <- createDataPartition(data$Performance , p = 0.70 ,list = FALSE)
training <-  data[Train ,] 
testing <- data[-Train ,]

#Step 4 : Correlation matrix
## checking corelation(x & y)/colinearity(x & x)/multicolinearity(>1 Colinearity) 

#In this eg. there is no colinearity 
cor(training)


#Step 5 : Run model and analyse summary
model <- lm(Performance~. , data = training)
summary(model)

#rEMOVING mACHINE CYCLE TIME AS ITS VALUES IS LESS THAN 0.05
model1 <- lm(Performance~.-Machine.Cycle.Time.ns , data = training)
summary(model1)

#Check R-squared and adjusted R-squared for accuracy...>0.7 indicates accurate/good model
#Then check p-value...whether it is < 0.05

## We observe that Machine cycle time is NOT relevant as p value is greater than 0.05
# we perform varibale selection method: Backward



#Step 6:Vif(<=5)
#Vif is used to remove colinearity/multicolinearity
#Here there is no colinearity/multicolinearity so just to check

library(car)
vif(model )

#Step 7: model assumption - Linearity,Normality,Constant Variance,Model error
#Normality(if most of the residuals are along the line) 
#& Constant vairance(there is no U-shape formed)

par(mfrow = c (2,2))
plot(model2)

library(lmtest)
#dwtest to check autocorrelation
dwtest(model2) #pvalue <0.05-H1 there is an autocorrelation
library(car)

#numeric Constant variance test
ncvTest(model2) ##p value < 0.05 - H1 variance is not constant













#Check whether data follows bell curve
hist(data$Performance)
## applying transformation to make it in normal form(bell shape curve) (niot right or left skewed)

hist((1/data$Performance)) ##reciprocal transformation
hist(log(data$Performance))##log transformation
##Step 8: prediction on Test data
testing$fitted <- predict(model2,testing) ##predict the model , log value
testing$original <- exp(testing$fitted)#anti logging (exp conversion)


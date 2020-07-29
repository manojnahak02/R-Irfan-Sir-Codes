##OBJECTIVE :to predict if the customer will buy from Retail store or malls.
#Distributor/Manufacturer -> Wholesaler -> Retailer ->consumer

#Variable description
#1) FRESH: annual spending (m.u.) on fresh products (Continuous); 
#2)	MILK: annual spending (m.u.) on milk products (Continuous); 
#3)	GROCERY: annual spending (m.u.)on grocery products (Continuous); 
#4)	FROZEN: annual spending (m.u.)on frozen products (Continuous) 
#5)	DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
#6)	DELICASSEN: annual spending (m.u.)on and delicatessen products (Continuous); 
#7)	CHANNEL: customers Channel - Retail store and Malls (Nominal) 
#8) REGION : Region (East, west,south)

train <- read.csv('C:/Users/Admin/Desktop/DSP_R Studio/Logistic_Reg_Exm.csv')
names(train)
str(train)

train$Channel <- as.factor(ifelse(train$Channel == 'Mall' ,1,0))
train$Fresh <- as.numeric(train$Fresh)
train$Milk <- as.numeric(train$Milk)
train$Grocery <- as.numeric(train$Grocery)
train$Frozen <- as.numeric(train$Frozen)
train$Detergents_Paper <- as.numeric(train$Detergents_Paper)
train$Delicassen <- as.numeric(train$Delicassen)

str(train)
##table(train$Channel)
#train$sum<- rowSums(train[2:7],na.rm = TRUE)

#checking missing values
summary(train)
sapply(train, function(x) sum(is.na(x))) #no missing values

#checking outlier
boxplot(train$Fresh)
boxplot(train$Milk)
boxplot(train$Grocery)
boxplot(train$Frozen)
boxplot(train$Detergents_Paper)
boxplot(train$Delicassen)
#all cols have outlier

#treating Outlier --- WINZORIZING TECHNIQUE
#1. Fresh
boxplot(train$Fresh)
summary(train$Fresh)
upper<- 16934+1.5*IQR(train$Fresh) ;upper
train$Fresh [train$Fresh > upper] <- upper
boxplot(train$Fresh)
summary(train$Fresh)

#2. Milk
boxplot(train$Milk)
summary(train$Milk)
upper<- 7190+1.5*IQR(train$Milk) ;upper
train$Milk [train$Milk > upper] <- upper
boxplot(train$Milk)
summary(train$Milk)

#3. Grocery
boxplot(train$Grocery)
summary(train$Grocery)
upper<- 10656+1.5*IQR(train$Grocery) ;upper
train$Grocery [train$Grocery > upper] <- upper
boxplot(train$Grocery)
summary(train$Grocery)

#4. FRozen
boxplot(train$Frozen)
summary(train$Frozen)
upper<- 3554.2+1.5*IQR(train$Frozen) ;upper
train$Frozen [train$Frozen > upper] <- upper
boxplot(train$Frozen)
summary(train$Frozen)

#5.Detergents_Paper)
boxplot(train$Detergents_Paper)
summary(train$Detergents_Paper)
upper<- 3922.0+1.5*IQR(train$Detergents_Paper) ;upper
train$Detergents_Paper [train$Detergents_Paper > upper] <- upper
boxplot(train$Detergents_Paper)
summary(train$Detergents_Paper)

#6. Delicassen
boxplot(train$Delicassen)
summary(train$Delicassen)
upper<- 1820.2+1.5*IQR(train$Delicassen) ;upper
train$Delicassen [train$Delicassen > upper] <- upper
boxplot(train$Delicassen)
summary(train$Delicassen)

##multiple logistic regression

library(caret)
Train <- createDataPartition(train$Channel , p=0.7 ,list = FALSE )
training <-train[Train,]
test1 <-train[-Train,]

#model building
model = glm(Channel~. , family = 'binomial', data = training) 
summary(model) ## check p values from either summary() or anova() , mostly refer anova result for p value, as in summary() one of the level/variable gets added in the intercept value as referene
anova(model , test = 'Chisq')

##variable significance selection
reg.model = step(glm(Channel~.,family = 'binomial',data = training) ,direction = "both")

#after running the variable seletion , we saw that the variable which was significant(Fresh) also got removed along with the insignificant variable(Delicassen) ,
##which implies there was a relation bwtween the two.

anova(reg.model , test = 'Chisq') ## check for significance of the variables and decide which variables are significant 

#summary(reg.model) #from summary we will only take B0 and B1 ## a NOTE return in notebook for Embarked C & embarked S


#checkng corcondance
Acc(reg.model)

## to check multicollinearity

library(car)
vif(reg.model)#GVIF as y is categorical

#to get the Odds ratio # We cannot run odds ratio bcz all my x values are numeric , if one of my x would have been categorical we would habe run odds ratio .
exp(coef(reg.model)) ##REgionEast is getting added to intercept

##prediction on testing data
test1$probs <- predict(reg.model,test1,type = 'response')
test1$predict <- as.factor(ifelse(test1$probs >0.70,1,0))
table(test1$Channel ,test1$predict) ## table 2* matrix of actual & predicted values

library(caret)
confusionMatrix(test1$Channel,test1$predict)

#plotting ROC curve
library(ROCR)
library(ggplot2)

#make prediction on test set
predictTrain = predict(reg.model,test1,type = "response")

#prediction function
ROCRpred = prediction(predictTrain ,test1$Channel)#generate TPR , FPR, TN , FP , FN , PN

#performacne prediction
ROCRperf = performance(ROCRpred,"tpr","fpr")

##ploting ROC curve
plot(ROCRperf)

#Add colors
plot(ROCRperf,colorize =TRUE)

#AUC
pred = prediction(test1$probs, test1$Channel)
as.numeric(performance(pred,"auc")@y.values) ## @y means for all the vaues of y


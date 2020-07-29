#Objective: The goal of the model is to establish the relationship between Chance of admission 
##and response variables such as GRE, TOEFL, SOP, LOR, CGPA, University Rating, Research Paper

#import the data in R
sample1<-read.csv(file="C:/Users/Kriti Dua/Desktop/admission_prediction_LR.csv") 
library(car)
input<-sample1
names(input)

#checking the data type
str(input)
#converting into numeric
input$GRE_Score<-as.numeric(input$GRE_Score)
input$TOEFL_Score<-as.numeric(input$TOEFL_Score)
input$University_Rating<-as.numeric(input$University_Rating)
input$Research<-as.numeric(input$Research)
str(input)
#deleting serial no from the list
input$Serial_No<-NULL

# checking outliers if any
boxplot(input$GRE_Score)
boxplot(input$TOEFL_Score)
boxplot(input$University_Rating)
boxplot(input$SOP)
boxplot(input$LOR) # outlier exists
boxplot(input$CGPA)
boxplot(input$Research)
boxplot(input$Chance_of_Admit)  # outlier exists

## removing outlier from LOR
summary(input$LOR)
lower<-3-1.5*IQR(input$LOR)
input$LOR[input$LOR<lower]<-lower
boxplot(input$LOR)
summary(input$LOR)

## removing outlier from Y variable
summary(input$Chance_of_Admit)
lower1<-0.63-1.5*IQR(input$Chance_of_Admit)
input$Chance_of_Admit[input$Chance_of_Admit<lower1]<-lower1
boxplot(input$Chance_of_Admit)
summary(input$Chance_of_Admit)



#data partition
library(caret)
Train<-createDataPartition(input$Chance_of_Admit, p=0.70, list = FALSE)   
training<-input[Train,]
testing<-input[-Train,]


#correlation check
cor(training) #multicolinearity exists

#model run - 1st iteration
model<-lm(Chance_of_Admit~.,data = training)
summary(model)
vif(model) 


##using stepwise to create model
model1<-step(lm(Chance_of_Admit~., data = training), direction = "both")
summary(model1)
vif(model1) 


##using stepwise without University rating
model2<-step(lm(Chance_of_Admit~.-University_Rating-SOP, data = training), direction = "both")
summary(model2)
vif(model2) 

hist(training$Chance_of_Admit)
#Model assumptions
par(mfrow=c(2,2))
plot(model2)
#library(lmtest)
#dwtest(model2)  # Null Hyp: No autocorrelation exists
#ncvTest(model1) # Null Hyp: Variance is constant


#Prediction
testing$fitted<-predict(model2,testing) #adding the new fitted/estimated values
View(testing)



## Predict the sales

library(ISLR) ## To import carseats from ISLR library
View(Carseats)  
input <- Carseats
names(input)
# Conversion of String variables to numeric variables using ifelse loop
input$UrbanFlag <- ifelse(input$Urban =="Yes",1,2)
input$USFlag <- ifelse(input$US =="Yes",1,2)
input$ShelveLocFlag <- ifelse(input$ShelveLoc =="Good",1,ifelse(input$ShelveLoc =="Medium",2,3))
input$UrbanFlag <- as.numeric(input$UrbanFlag)
input$USFlag <-as.numeric(input$USFlag)
input$ShelveLocFlag <- as.numeric(input$ShelveLocFlag)
str(input) # To check variable type

# To remove 0 values with mean
##input$Advertising <- ifelse(input$Advertising == 0, mean(input$Advertising), input$Advertising)

# Outlier Check (Sales, CompPrice, Advertising and Price has outliers)
boxplot(input$Sales)
boxplot(input$CompPrice)
boxplot (input $Income)
boxplot(input$Advertising)
boxplot(input$Population)
boxplot(input$Price)
boxplot(input$Age)
boxplot(input$Education)

# Outlier Treatment for sales
summary(input$Sales)
up <- 9.320 +1.5*IQR(input$Sales) ; up
input$Sales[input$Sales > up] <- up
boxplot(input$Sales)

# Outlier Treatment for Advertising
##summary(input$Advertising)
##upper <- 12 +1.5*IQR(input$Advertising) ; upper
##input$Advertising[input$Advertising > upper] <- upper
##boxplot(input$Advertising)

# Outlier Treatment for CompPrice
summary(input$CompPrice)
upper1 <- 135 +1.5*IQR(input$CompPrice) ; upper1
input$CompPrice[input$CompPrice > upper1] <- upper1
boxplot(input$CompPrice)

lower <- 115 - 1.5*IQR(input$CompPrice) ; lower
input$CompPrice[input$CompPrice < lower] <- lower
boxplot(input$CompPrice)

# Outlier Treatment for Price
summary(input$Price)
upper2 <- 131 +1.5*IQR(input$Price) ; upper2
input$Price[input$Price > upper2] <- upper2

lower1 <- 100 -1.5*IQR(input$Price) ; lower1
input$Price[input$Price < lower1] <- lower1
boxplot(input$Sales)


input1 <- input[,-c(5,8,9, 12,13, 7,10,11)]

#data partition
library(caret)
Train <- createDataPartition(input1$Sales, p=0.70, list = FALSE) 
training <- input1[Train, ]
testing <- input1 [-Train, ]

cor(training)
model <- lm(Sales~., data = training)
summary(model) 

hist(training$Sales)
vif(model)

#assumption
par(mfrow=c(2,2))
plot(model)
library(lmtest)
dwtest(model)
library(car)
ncvTest(model)
#Prediction
testing$fitted <- predict(model,testing)


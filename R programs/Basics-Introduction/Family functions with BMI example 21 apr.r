Age<-c(56,34,67,33,25,28)
Weight<-c(78,67,56,44,56,89)
Height<-c(165,171,167,167,166,181)

BMI_df<-data.frame(Age, Weight, Height)
BMI_df

#row wise sum up of dataframe using apply function in R
apply(BMI_df,1,sum) #1 stands for row

#column wise sum up of data frame using apply function in R
apply(BMI_df,2,sum) #2 stands for column

#column wise mean up of data frame using apply function in R
apply(BMI_df,2,mean)

#lapply function in R (list apply gives output in rows)
#lapply function takes list, vector or data frame as input and returns only list as output.
lapply(BMI_df,mean)

#sapply function in R (output in column)
sapply(BMI_df, mean)

#tapply higher functin in R
#tapply() is powerful function that lets you break a vector
#and then apply some function to each of the pieces.
str(mtcars$cyl)
levels(as.factor(mtcars$cyl))
tapply(mtcars$mpg,mtcars$cyl,mean)

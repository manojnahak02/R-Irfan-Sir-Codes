#sorting ex using the mtcars dataset
data()
mtcars <- mtcars
mtcars2<- mtcars
names(mtcars)
# its is used to fix dataset
attach(mtcars)
View(mtcars)
head(mtcars) #bydefault top 6 rows will appear
head(mtcars,10) #top 10 rows
tail(mtcars)
summary(mtcars) #univariant analysis
boxplot(mtcars) # to identify outliars
boxplot(mtcars$hp) # to see boxplot for a column name hp

summary(mtcars$mpg) #summary for a  column name mpg

#sort by mpg
newdata <- mtcars[order(mpg),]
newdata <- mtcars[order(mpg,cyl),]
newdata <- mtcars[order(-mpg),]
detach(mtcars)

myvars <- c("mpg","cyl","disp")
dim(mtcars)

newdata1 <- mtcars[myvars] # here myvars becomes column as it is by default
newdata2 <- mtcars[c(1,5:10)] #here we are calling column no 1 & 5:10

#dropping variables excluding (DROPPING) variables

#delete one variable
newdata$mpg <- NULL #here null stand for delete and we deleted mpg column

newdata <- mtcars

#exclude 3rd and 5th variable
newdata1 <- newdata[c(-3,-5)] #deleting multiple columns

#using subset function
newdata2 <- subset(mtcars, mpg >= 20 & wt < 10, select = c(gear,qsec))
#if mpg is >= 20 and wt is < 10 then select two column gear and qsec




mtcars <- mtcars
# in R factor means data is categorical
mtcars$hpcat <- as.factor(ifelse(mtcars$hp > 70, c("older"),c("younger")))

levels(mtcars$hpcat)
rm(mtcars)

#another example : create 3 hp categories
mtcars <- mtcars
attach(mtcars)
mtcars$hpcat[hp > 75] <- "Elder"
mtcars$hpcat[hp > 60 & hp <= 75] <- "Middle hpd"
mtcars$hpcat[hp <= 60] <- "Young"
str(mtcars)
str(mtcars$hpcat) #str is used to see structure of dataset
mtcars$hpcat <- as.factor(mtcars$hpcat) 
#by default data is char (text) we convert it to factor(categorical)
str(mtcars)
levels(mtcars$hpcat)
rm(mtcars)

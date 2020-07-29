#Data import and export
data1<- read.csv(file.choose()) #first method of importing file
data2<- read.csv(file="C:/Users/Manoj Nahak/Desktop/mtcars.csv") #importing files from path
data3<- read.csv("mtcars.csv") #third step to import files after setting the working directory

#data export into csv
data<- faithful
write.csv(data,file = "C:/Users/Manoj Nahak/Desktop/export.csv")

library(xlsx)
#library(clsxjars)
#library(openxlsx)
#data importing
#it has limit upto 1 lakh row for importing and exporting
#1 stands for sheet no.
#1=sheet no.1 and 2=sheetno.2
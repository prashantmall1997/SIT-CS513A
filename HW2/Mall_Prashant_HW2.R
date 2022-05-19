#################################################
#  Company    : Stevens 
#  Project    : R Homework Assignment 02
#  Purpose    : Perform EDA analysis on given data set
#  First Name : Prashant Pramodkumar  
#  Last Name  : Mall
#  Id			    : 10459371
#  Date       : 15th Feb 2022
#  Comments   : A quick brown fox jumps over the lazy dog

rm(list=ls())
df <- read.csv("/Users/prashantmall1997/Coding/HW2/breast-cancer-wisconsin.csv", na.strings = "?", header=TRUE, stringsAsFactors=FALSE)

#Summarizing each column (e.g. min, max, mean )
summary(df)

#Identifying missing values
is.na(df) 
View(df)

#Replacing the missing values with the “mean” of the column
for(cell in 1:ncol(df)){
  df[is.na(df[,cell]), cell] <- mean(df[,cell], na.rm = TRUE)
}
View(df)

#Displaying the frequency table of “Class” vs. F6
dfClassVsF6 <- table(df$Class, df$F6)
ftable(dfClassVsF6)

#Displaying the scatter plot of F1 to F6, one pair at a time
plot(df[2:7], main = "Scatter Plot - F1 to F6 (One pair at a time)", ph = 10, col = 2)

#Show histogram box plot for columns F7 to F9
boxplot(df[8:10], main = "Histogram Box Plot - Columns F7 to F9",xlab="X axis", ylab="Y axis")

#Delete all the objects from your R- environment
rm(list=ls())

#Reload the “breast-cancer-wisconsin.data.csv” from canvas into R
df <- read.csv("/Users/prashantmall1997/Coding/HW2/breast-cancer-wisconsin.csv", na.strings = "?", header=TRUE, stringsAsFactors=FALSE)

#Remove any row with a missing value in any of the columns
df[,1:11][df[,1:11]=="?"] <- NA
df <- na.omit(df)

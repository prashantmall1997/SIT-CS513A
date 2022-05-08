#CS513-MidTerm
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#Topic: Midterm Q2

#Clear Variables
rm(list = ls())

#Load the “IBM_attrition_v3” dataset:
IbmData <- read.csv(file = "/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/Homework Assignments/MidTerm/IBM_Attrition_v3.csv",header=TRUE, sep=",")
View(IbmData)

#I.Summarizing each column (e.g., min, max, mean)
summary(IbmData)

#II.Identifying missing values
missingData <- is.na(IbmData)
View(missingData)

#III.Replacing the numerical missing values with the “mean” of the corresponding columns
for(i in 1:ncol(IbmData)){
  IbmData[is.na(IbmData[,i]), i] <- mean(IbmData[,i], na.rm = TRUE)
}
View(IbmData)

#IV.Displaying the scatter plot of “Age”, “MonthlyIncome” and “YearsAtCompany”, one pair at a time 
df <- data.frame(IbmData)
pairs(df[,c(1,4,5)], main = "Scatter Plot of Age, MonthlyIncome and YearsAtCompany", pch = 0, col = "blue")

#V.Showing box plots for columns: “Age”, “MonthlyIncome”, and “YearsAtCompany” 
boxplot(df[, c("Age","MonthlyIncome","YearsAtCompany")], col = c('Blue', 'Red', 'Green'))
title("Box Plot for Age, MonthlyIncome and YearsAtCompany")

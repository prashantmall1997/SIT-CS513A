#CS513-MidTerm
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#Topic: Midterm Q4

#Clear Variables
rm(list = ls())

##Importing the libraries 
library(e1071)
library(class) 

#Load the “IBM_attrition_v2” dataset:
IbmData <- read.csv(file = "/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/Homework Assignments/MidTerm/IBM_Attrition_v2.csv",header=TRUE, sep=",")
View(IbmData)

#Remove the missing values.
IbmData <- na.omit(IbmData)
View(IbmData)

#Discretize the “MonthlyIncome” into “up to3,000”, “3000 up to 5,000.00”, “5000 up to 8,500” and “8500 or more” per months.
IbmData$MonthlyIncome <- ifelse(IbmData$MonthlyIncome<=3000,"up to 3000", ifelse(IbmData$MonthlyIncome>3000&IbmData$MonthlyIncome<=5000, "3000 to 5000", ifelse(IbmData$MonthlyIncome>5000&IbmData$MonthlyIncome<=8500,"5000 to 8500","8500 or more")))
View(IbmData)

#Discretize the age into “less than 31”, “31 up to 38”, “38 up to 48”, and “48 or over”. 
IbmData$Age <- ifelse(IbmData$Age<=31,"less than 31", ifelse(IbmData$Age>31&IbmData$Age<=38, "31 up to 38", ifelse(IbmData$Age>38&IbmData$Age<=48,"38 up to 48","48 or more")))
View(IbmData)

#Split into training and testing
sampleData<-sort(sample(nrow(IbmData),as.integer(.70*nrow(IbmData))))
training<-IbmData[sampleData,]
testing<-IbmData[-sampleData,]

#Construct a Naïve Bayes model to classify attrition (attrition=’yes’) based on the other variables

nBayes_class <- naiveBayes(Attrition~., data =training)

#Predict Target Class for the Validation set
predict_naive <- predict(nBayes_class, testing)

#Confusion Matrix
confussionMatrix <- table(Prediction=predict_naive,Class=testing$Attrition)
print(confussionMatrix)

#Output of Naive Bayes Classifier
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confussionMatrix)


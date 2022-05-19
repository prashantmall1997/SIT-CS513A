#CS513-MidTerm
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#Topic: Midterm Q5

#Clear Variables
rm(list = ls())

library(class)

#Load the “IBM_attrition_v2” dataset:
IbmData <- read.csv(file = "/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/Homework Assignments/MidTerm/IBM_Attrition_v2.csv",header=TRUE, sep=",")
View(IbmData)

#Remove Missing Values
IbmData <- na.omit(IbmData)
View(IbmData)

IbmData$Attrition <- ifelse(IbmData$Attrition=="Yes",1,2)

#Eliminating Rows with Missing Data
missingIbmData = IbmData[!complete.cases(IbmData),]

IbmData <- na.omit(IbmData)

#Split Dataset into Training and Testing 
sampleData<-sort(sample(nrow(IbmData),as.integer(.30*nrow(IbmData))))
training<-IbmData[-sampleData,]
testing<-IbmData[sampleData,]

#Classification
classifier_knn <- knn(training, testing, IbmData[-sampleData,4], k=3)

#Confusion Matrix
confusion_matrix <- table (classifier_knn, testing[,4])
confusion_matrix

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)


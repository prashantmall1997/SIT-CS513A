#CS513-HW4
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#HW Topic: Naïve Bayes

rm(list=ls())

#Install Naive Bayes Classifier and class package
install.packages("e1071")
install.packages("class")
library(e1071)
library(class)

#Read File
df <- read.csv("/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/Homework Assignments/HW4/breast-cancer-wisconsin.csv",header=TRUE, sep=",")

#Remove missing values rows
df <- na.omit(df)

#Non-numeric characters data type in the column
df$F6 <- as.integer(df$F6)

#Convert labels to factor class
df$Class<- factor(df$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

subdata = df[2:11]

#70% sample data size
sampleSize <- floor(0.70 * nrow(subdata))

#Set Seed
set.seed(123)
train <- sample(seq_len(nrow(subdata)), size = sampleSize)

#Load 70% record in training data set
trainingDS <- subdata[train, ]

#Load 30% in test data set
testDS <- subdata[-train, ]

#Implementing Naive Bayes
NB<- naiveBayes(Class ~ ., data = trainingDS)

#Predicting target class for the Validation set
predictNB <- predict(NB, testDS)

conf_matrix <- table(predictNB=predictNB,class=testDS$Class)
print(conf_matrix)

#Naive Bayes Classifier
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)

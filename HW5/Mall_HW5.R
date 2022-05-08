#CS513-HW5
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#HW Topic: Dtree

rm(list=ls())

library(class)
library(rpart)

#Read File
df <- read.csv("/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/Homework Assignments/HW4/breast-cancer-wisconsin.csv",header=TRUE, sep=",")
head(df, n=5)
#Summary of each column
n <- as.numeric(as.character(df$F6))
df$F6 <- n
summary(df, na.rm = TRUE)

#Remove rows with missing values
df <- na.omit(df)

#Labels to Factor Class
df$Class<- factor(df$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
is.factor(df$Class)

#Train and Test - ratio 70% to 30%
df<- df[2:11]
size <- floor(0.70 * nrow(df))

#Set Seed 
set.seed(123)
random <- sample(seq_len(nrow(df)), size = size)

#70% Data - Train 
train <- df[random, ]

#30% Data - Test 
test <- df[-random, ]

#CART 
cart <- rpart(Class ~ ., data = train, method = "class")

#Predicting Class - Test Set
predicted <- predict(cart, test, type = "class")
print(length(predicted))
print(length(test$Class))

#Confusion Matrix
conf_matrix <- table(predicted,test$Class)
print(conf_matrix)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_matrix)


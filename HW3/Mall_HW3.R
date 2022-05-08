#CS513-HW3
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#HW Topic: KNN

rm(list=ls())

df <- read.csv("/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/Homework Assignments/HW3/breast-cancer-wisconsin.csv",header=TRUE, sep=",")

#Non-numeric characters data type in the column
n <- as.numeric(as.character(df$F6))
df$F6 <- n

#Remove missing values rows
df <- na.omit(df)

#Convert labels to factor class
df$Class<- factor(df$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#Use 30% test 70% training data
size <- sample(1:nrow(df), 0.7 * nrow(df)) 
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

#Normalization - First 4 Columns - Predictors
norm <- as.data.frame(lapply(df[,c(2,3,4,5,6,7,8,9,10)], nor))

dfTwo = df['Class']
#Train
train <- norm[size,] 
cl_train <- dfTwo[size,]
#Test
test <- norm[-size,] 
cl_test <- dfTwo[-size,]

#Load Package Class
library(class)

#KNN function for k = 3
clf <- knn(train,test,cl=cl_train,k=3)
#Confusion Matrix
conf_matrix <- table(clf, cl_test)
print(conf_matrix)
#Accuracy
accThree <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accThree(conf_matrix)

#KNN function for k = 5
clf <- knn(train,test,cl=cl_train,k=5)
#Confusion Matrix
conf_matrix <- table(clf, cl_test)
print(conf_matrix)
#Accuracy
accFive <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accFive(conf_matrix)

#KNN function for k = 10
clf <- knn(train,test,cl=cl_train,k=5)
#Confusion matrix
conf_matrix <- table(clf, cl_test)
print(conf_matrix)
#Accuracy
accTen <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accTen(conf_matrix)

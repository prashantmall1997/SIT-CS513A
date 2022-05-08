#CS513-HW8
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371

rm(list=ls())

#load the file
df<-read.csv("/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/HW9/wisc_bc_ContinuousVar.csv",na.strings = '?')

#Delete 1st Row
df1 = subset(df, select = -c(id) )

#Factorize Diagnosis
df1$diagnosis <- factor(df1$diagnosis, levels = c('M','B'),labels = c(1,2))

#Train & Test Data
index<-sort(sample(nrow(df),as.integer(.70*nrow(df))))
train<-df1[index,]
test<-df1[-index,]

#SVM 
library(e1071)
svm.model <- svm(diagnosis~ ., data = train)
svm.pred <- predict(svm.model,  test )

#Confusion Matrix 
confMatrix <- table(predictSvm=svm.pred,class=test$diagnosis)
print(confMatrix)

#Accuracy 
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confMatrix)
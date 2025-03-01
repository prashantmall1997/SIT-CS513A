#CS513-HW5
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#HW Topic: Random Forest & C5.0



rm(list = ls())
library(randomForest)
library(class)
library(C50)


df <- read.csv("D:/Prashant Mall/CS 513 KDD/HW_06/breast-cancer-wisconsin.csv",header=TRUE, sep=",")

#sumary of Data Frame
summary(df)

# F6 is a type of character, need to convert into the number
df$F6<-as.numeric(as.character((df$F6)))
summary((df))

# count and remove NA's from the dataframe
sum(is.na(df))
df<-na.omit(df)
sum(is.na(df))

# convert Class into factor class
df$Class<-factor(df$Class, levels = c("2","4"), labels = c("Benign","Malignant"))
is.factor(df$Class)

# discard the sample/1st column from dataFrame

df<-df[2:11]
View(df)

# Split Train and Test data 70-30 ratio
split_size<-floor(0.70*nrow(df))


#set.seed(111)
random_sample<-sample(seq_len(nrow(df)), size = split_size)

train<-df[random_sample,]
test<-df[-random_sample,]

#Creating Accuracy function
accuracy<-function(x){
  sum(diag(x)/sum(rowSums(x)))*100
}

#Implementing C50
C50<-C5.0(Class~.,train)

plot(C50)

#Preddiction

pred_C50<-predict(C50,test,type = "class")
length(pred_C50)
length(test)
#confusionMatric
confMat_C50<-table(test$Class,pred_C50)
print(confMat_C50)


#Accuracy of C50
accuracy(confMat_C50)


####  Implementing Random Forest ####
RF<-randomForest(Class~.,train, importance=TRUE, ntree=1000)
importance(RF)
varImpPlot(RF)
# Prediction for Random Forest

pred_RF<-predict(RF,test,type = "class")
length(pred_RF)
length(test)
#confusionMatric
confMat_RF<-table(test$Class,pred_RF)
print(confMat_RF)


# Accuracy
accuracy(confMat_RF)

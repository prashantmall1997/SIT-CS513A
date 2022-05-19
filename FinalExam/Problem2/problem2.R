#CS513 Final Exam
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371

rm(list=ls()) 

#Library
library(randomForest)

#Load File
Attrition_Modified <- read.csv("/Users/prashantmall1997/Coding/KDD/Problem1/Attrition_Modified.csv", na.strings = '?')
View(Attrition_Modified)

#Set Column 'Attrition' as factor
Attrition_Modified$Attrition<- factor(Attrition_Modified$Attrition , labels = c("yes","no"))
View(Attrition_Modified)

class(Attrition_Modified$Attrition)
print(is.factor(Attrition_Modified$Attrition))

#Setting seed
set.seed(111)

#Split Test and Train Data
index<-seq(1, nrow(Attrition_Modified), by=4)

# Building trainData and testData datset
trainData<-Attrition_Modified[-index,]

testData<-Attrition_Modified[index,]

#Random Forest Classification Model
rForest <- randomForest( factor(Attrition)~., data=trainData, importance=TRUE, ntree=1000)
importance(rForest)
dev.off()

prediction <- predict(rForest, testData)
prediction

result <- table(actual=testData$Attrition,prediction)

#Accuracy 
accuracy <-(sum(diag(result))/(sum(rowSums(result)))*100)
print(accuracy)

#Error Rate
error_rate<- 1 - as.double(accuracy/100)
print(error_rate)


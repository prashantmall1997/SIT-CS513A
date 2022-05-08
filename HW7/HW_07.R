#CS513-HW5
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371
#HW Topic: ANN

rm(list=ls())

#Neuralnet Library
library(neuralnet)

df<-read.csv("/Users/prashantmall1997/Library/CloudStorage/OneDrive-Personal/Coding/Stevens-Courses/CS513A/HW7/wisc_bc_ContinuousVar.csv",na.strings = '?')

df$diagnosis <- factor(df$diagnosis, levels = c('M','B'),labels = c(1,2))

#Split Data
index<-sort(sample(nrow(df),as.integer(.70*nrow(df))))

#Train and Test data
trainData<-df[index,]
testData<-df[-index,]

#Model
model<- neuralnet(diagnosis~.,trainData[-1], hidden=5, threshold=0.01)

#Plotting neural network
plot(model)

#ANN
ann <-compute(model,testData)
ann$net.result
anncat<-ifelse(ann$net.result <1.5,1,2)

#Length
length(anncat)
length(testData$diagnosis)

#Error Rate
wrong<- (testData$diagnosis!=anncat)
errorRate<-sum(wrong)/length(wrong)
errorRate


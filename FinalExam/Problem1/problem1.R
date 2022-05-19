#CS513 Final Exam
#First Name: Prashant Pramodkumar
#Last Name: Mall
#CWID: 10459371

rm(list=ls())

#load the file
df<-read.csv("/Users/prashantmall1997/Coding/KDD/IBM_Attrition_v3.csv",na.strings = '?')

#Total Rows
nrow(df)
#Delete all the rows with missing value.
df_cleanNa <- na.omit(df)
View (df_cleanNa)
summary(df_cleanNa)

#Create four categories (income1, income2, income3, income4) based on monthly income (“MonthlyIncome”)
df_cleanNa['MonthlyIncomeModified'] <- NA
df_cleanNa$MonthlyIncomeModified <- ifelse(df_cleanNa$MonthlyIncome <= 2900, 
                                "income1", 
                                ifelse(df_cleanNa$MonthlyIncome > 2900 & df_cleanNa$MonthlyIncome<=5000, 
                                       "income2",
                                       ifelse(df_cleanNa$MonthlyIncome > 5000 & df_cleanNa$MonthlyIncome<=8500, 
                                              "income3", 
                                              "income4")))

#Create two categories (senior, not-senior) for years at the company (“YearsAtCompany”)
df_cleanNa['YearsAtCompanyModified'] <- NA
df_cleanNa$YearsAtCompanyModified <- ifelse(df_cleanNa$YearsAtCompany <= 6, "not-senior", "senior")

#Create two categories (young, mature) for age
df_cleanNa['AgeModified'] <- NA
df_cleanNa$AgeModified <- ifelse(df_cleanNa$Age <= 37, "young", "mature")

#Drop the original columns: MonthlyIncome, YearsAtCompany and Age 
df_cleanNa <- subset(df_cleanNa,select = -c(MonthlyIncome, YearsAtCompany, Age))
df_cleanNa <- df_cleanNa[c("AgeModified", "JobSatisfaction", "MaritalStatus", 
               "MonthlyIncomeModified", "YearsAtCompanyModified", "Attrition")]
colnames(df_cleanNa) <- c("Age","JobSatisfaction","MaritalStatus","MonthlyIncome","YearsAtCompany","Attrition")

write.csv(df_cleanNa,"/Users/prashantmall1997/Coding/KDD/Problem1/Attrition_Modified.csv", row.names = FALSE)

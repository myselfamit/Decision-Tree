#Objective: To predict whether a bank customer will exit or not using Decision Tree

bankcust<-read.csv("E:/R files/Decision Tree/Customer_Exit.csv")

head(bankcust,10)


#Datatype
str(bankcust)

#Remove Variables which are not required for Model
bankcust$RowNumber<-NULL
bankcust$CustomerId<-NULL
bankcust$Surname<-NULL
str(bankcust)


#Convert the Y variable to Categorical Variable
bankcust$Exited_Flag<-as.factor(bankcust$Exited)

#ifelse(bankcust$Exited_Flag==2,0,1)
bankcust$Exited<-NULL
str(bankcust)

table(bankcust$Exited_Flag)
#Bad Rate
2037/10000 #Bad Rate is 20%

library(ggplot2)
library(gridExtra)

summary(bankcust)


#Treatment of Outliers
boxplot(bankcust$CreditScore)

#Remove Outliers
summary(bankcust$CreditScore)
lower<-584-1.5*IQR(bankcust$CreditScore)
lower
bankcust$CreditScore[bankcust$CreditScore<lower]<-lower
boxplot(bankcust$CreditScore,col="light green")

boxplot(bankcust$Age,col="light blue")
summary(bankcust$Age)
upper<-44+1.5*IQR(bankcust$Age)
upper
bankcust$Age[bankcust$Age>upper]<-upper
boxplot(bankcust$Age,col="light blue")

table(bankcust$Tenure<=0)
boxplot(bankcust$Tenure)
bankcust$Tenure<-ifelse(bankcust$Tenure<=0,median(bankcust$Tenure),bankcust$Tenure)
table(bankcust$Tenure)
summary(bankcust$Tenure)

table(bankcust$Balance<=0)
bankcust$Balance<-ifelse(bankcust$Balance<=0,mean(bankcust$Balance),bankcust$Balance)
hist(bankcust$Balance)
boxplot(bankcust$Balance)
summary(bankcust$Balance)
upper<-127644+1.5*IQR(bankcust$Balance)
upper
bankcust$Balance[bankcust$Balance>upper]<-upper

boxplot(bankcust$NumOfProducts)
summary(bankcust$NumOfProducts)
upper<-2+1.5*IQR(bankcust$NumOfProducts)
upper
bankcust$NumOfProducts[bankcust$NumOfProducts>upper]<-upper
boxplot(bankcust$NumOfProducts)

boxplot(bankcust$EstimatedSalary)
str(bankcust)

#Data Partition
library(caret)
train<-createDataPartition(bankcust$Exited_Flag,p=0.7,list=FALSE)
training<-bankcust[train,]
testing<-bankcust[-train,]

# Building Model
library(rpart)
Model =rpart(Exited_Flag~.,data=training )
rpart.plot.version1(Model, main = "Model Before Pruning",
                    type = 5, extra = 1,cex = 0.5,
                    faclen = 0,varlen = 0) # to get full name 

# Prediction on Training data 
training$Predicted=predict (Model,training,type ="class")
library(caret)
confusionMatrix(training$Predicted,training$Exited_Flag)


# Doing Pre-Pruning 
c<-rpart.control(minsplit =10, minbucket = 5, maxdepth = 3)
training$Predicted<-NULL

# Re-Building Model & Plotting Model
tune_fit <- rpart(Exited_Flag~.,data=training,method ="class", control =c)
rpart.plot.version1(tune_fit, main = "Model After Pruning",
                    type = 5, extra = 1,cex = 0.5,
                    faclen = 0,varlen = 0)

# Prediction on Testing data 
testing$Predicted=predict (Model,testing,type ="class")
library(caret)
confusionMatrix(testing$Predicted,testing$Exited_Flag)


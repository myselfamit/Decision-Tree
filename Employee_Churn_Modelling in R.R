#Objective: To predict whether a employee will exit or not using Decision Tree


################### Importing data ##################
bankcust<-read.csv("E:/R files/Decision Tree/employee (1).csv")

################### To check Data Type ##################
str(bankcust)
names(bankcust)

#################### Just taking a subset of column for model building ##################
bankcust = subset(bankcust, select = -c(7,9,10,27,22))
str(bankcust)

################## Data Conversion#############################

# All int variable to convert to numeric
bankcust1 = subset(bankcust, select = -c(2,3,5,7,9,13,15,19))
names(bankcust1)
str(bankcust1)
bankcust1<-data.frame(apply(bankcust1, 2, as.numeric))
str(bankcust1)

# All int variable to convert to numeric
bankcust2 = subset(bankcust, select = c(2,3,5,7,9,13,15,19))
str(bankcust2)

bankcust<-data.frame(bankcust2,bankcust1)
str(bankcust)
names(bankcust)

##############333#Data Partition#789#######################
set.seed(231)
library(caret)
train<-createDataPartition(bankcust$Attrition,p=0.7,list=FALSE)
training<-bankcust[train,]
testing<-bankcust[-train,]

################# Building Model & Plotting Model#################
library(rpart)

Model =rpart(Attrition~.,data=training,method = "class" )

rpart.plot.version1(Model, main = "Model Before Pruning",
                    type = 5, extra = 1,cex = 0.5,
                    faclen = 0,varlen = 0) # to get full name 

#plot(Model, uniform = TRUE, compress = TRUE, branch = .2)
#text(Model, cex = .8, xpd = NA)
#title("Model Before Pruning")

# Prediction on Training data 
training$Predicted=predict (Model,training,type ="class")
library(caret)
confusionMatrix(training$Predicted,training$Attrition)

# Doing Pre-Pruning 
c<-rpart.control(minsplit =10, minbucket = 5, maxdepth = 3)
training$Predicted<-NULL

# Re-Building Model & Plotting Model
tune_fit <- rpart(Attrition~.,data=training,method ="class", control =c)
rpart.plot.version1(tune_fit, main = "Model After Pruning",
                    type = 5, extra = 1,cex = 0.5,
                    faclen = 0,varlen = 0)

#plot(tune_fit, uniform = TRUE, compress = TRUE, branch = .2)
#text(tune_fit,cex = .8, xpd = NA)
#title("Model After Pruning")

# Prediction on Training data 
training$Predicted=predict (tune_fit,training,type ="class")
library(caret)
confusionMatrix(training$Attrition,training$Predicted)


# Prediction on Testing data 
testing$Predicted=predict(tune_fit,testing,type ="class")
library(caret)
confusionMatrix(testing$Predicted,testing$Attrition)

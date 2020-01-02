library (tree)
## Creating a Dependance Variabel base on sales 
salesdata<-read.csv("E:/R files/Decision Tree/Sales data.csv")

# Behaviour of data
summary(salesdata$Sales)
salesdata$High=as.factor(ifelse(salesdata$Sales<=8,0,1))

# deleting column sales
salesdata$Sales<-NULL
data<-salesdata
rm(salesdata)
sapply(data,function(x)sum(is.na(x)))

##Data Partition
set.seed(54)
library(caret)
split <- createDataPartition(y=data$High, p=0.7, list=FALSE)
train <- data[split,]
test <- data[-split,]

# Building Model
# reduction deviance or gini
Model =tree(High~.,data=train,split = "gini")
Model

# Plotting Model 
plot(Model)
text(Model,pretty =0)

# Prediction on Train data set & Checking Performance(Accuracy)
output_train=predict(Model,train,type="class")
table(output_train ,train$High)
confusionMatrix(output_train,train$High)

# Prediction on Test data set  & Checking Performance(Accuracy)
output_test=predict (Model,test,type ="class")
table(output_test ,test$High)
library(caret)
confusionMatrix(output_test,test$High)


#The function cv.tree() performs cross-validation in order to
#cv.tree()
#determine the optimal level of tree complexity; 
#cost complexity pruning is used in order to select a 
#sequence of trees for consideration.
#We use the argument FUN=prune.misclass in order to indicate 
#that we want the
#classification error rate to guide the cross-validation 
#and pruning process,

# Pruning the Model with help of Cv 
cv.data =cv.tree(Model ,FUN=prune.misclass )
names(cv.data )
plot(cv.data$size ,cv.data$dev ,type="b")
Prune.Model =prune.misclass (Model ,best =8)

# Plotting Model
plot(Prune.Model)
text(Prune.Model,pretty =0)

# Prediction on Train data set  & Checking Performance(Accuracy)
output_Prune_train=predict(Prune.Model,train,type="class")
table(output_Prune_train ,train$High)
confusionMatrix(output_Prune_train,train$High)

# Prediction on Test data set & Checking Performance(Accuracy)
output_Prune_test=predict(Prune.Model,test,type="class")
table(output_Prune_test ,test$High)
confusionMatrix(output_Prune_test,test$High)

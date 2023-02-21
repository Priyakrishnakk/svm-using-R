data <- read.csv("C:/Users/user/Downloads/Imarticus related/R programming/Class/6 SVM/DataSet/bank-additional-full.csv", sep=";")
View(data)
summary(data)
data[,c(2:10,15,21)]<-lapply(data[c(2:10,15,21)], as.factor)

table(data$job,data$marital)

levels(data$day_of_week)<-c(5,1,4,2,3)
levels(data$job)<-c(1:13)
levels(data$marital)<-c(1:4)
levels(data$education)<-c(1:8)
levels(data$default)<-c(1:3)
levels(data$housing)<-c(1:3)
levels(data$loan)<-c(1:3)
levels(data$contact)<-c(1,2)
levels(data$month)<-c(4,8,12,7,6,3,5,11,10,9)
levels(data$poutcome)<-c(1:3)
unique(data$euribor3m)

#job and subscription numbers
x<-table(data$job,data$y)

#Consumer price index looks to be almost the same - it might not be useful- take a call later
aggregate(cons.price.idx~month,data =data,mean)

anyNA(data)

data[,c(2:10,15)]<-lapply(data[c(2:10,15)], as.numeric)

set.seed(123)
train.rows<- createDataPartition(y= data$y, p=0.8, list = FALSE)
unique(data_train$job)
data_train<- data[train.rows,]
data_test<-data[-train1.rows,]


bank_m1<-svm(y~.,data=data_train,kernel="linear")
pred<-predict(bank_m1,data_test)
require(caret)
confusionMatrix(pred,data_test$y)

bank_m2<-svm(y~.,data=data_train,kernel="sigmoid")
pred_m2<-predict(bank_m2,data_test)
require(caret)
confusionMatrix(pred_m2,data_test$y)

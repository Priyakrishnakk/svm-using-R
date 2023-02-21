library(e1071)
plot(iris)
plot(iris$Sepal.Length,iris$Sepal.Width,col = iris$Species)
plot(iris$Petal.Length,iris$Petal.Width,col = iris$Species)
cols<-c("Petal.Length","Petal.Width","Species")
set.seed(234)
train<-sample(nrow(iris),100)
iris_train<-iris[train,cols]
iris_test<-iris[-train,cols]

m1<-svm(Species~.,data=iris_train,kernel = "linear",cost = 0.1)

#Cost means the cost of misclassification - Higher costs - high penality for the data points in the margin - overfits the model, all training data points get classified perfectly
# If lower costs or 0 cost - less penality for the data poitns in the margin - underfitting. Thus, Find the ideal cost
summary(m1) 
plot(m1,iris_train)

p1<-predict(m1,iris_test,type="class")
p2<-predict(m1,iris_train,type="class")
require(caret)
confusionMatrix(p1,iris_test[,3])
confusionMatrix(p2,iris_train[,3])


m2<-tune(svm,Species~.,data=iris_train,kernel="linear",ranges = list(cost = seq(0.1,1,.001)))
m3<-tune(svm,Species~.,data=iris_train,kernel="linear",ranges = list(cost = c(0.001,0.05,0.25,1,1.25,1.3,1.5,2.3)))
m4<-tune(svm,Species~.,data=iris_train,kernel="linear",ranges = list(cost = c(0.001,.001,0.005)))
summary(m3)
plot(m3) #Plot against cost and error rate

p3<-predict(m3$best.model,iris_test,type="class")
confusionMatrix(p3,iris_test[,3])

trcont<-trainControl(method="repeatedcv",number=10,repeats=3)
model1<- train(Species~.,data=iris_train,method = "svmLinear",trControl=trcont,preProcess=c("center","scale"))
model1
pred1<-predict(model1,iris_test)
confusionMatrix(pred1,iris_test[,3])

grid<-expand.grid(C = c(0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2.5))
model2<- train(Species~.,data=iris_train,method = "svmLinear",trControl=trcont,preProcess=c("center","scale"),tuneGrid=grid)
model2
plot(model2) #Plot against cost and accuracy
pred2<-predict(model2,iris_test)
confusionMatrix(pred2,iris_test[,3])
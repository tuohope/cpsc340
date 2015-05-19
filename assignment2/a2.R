setwd("/Users/tuo/Desktop/cpsc340/assignment2")
getwd()
library(rpart)
library(tm)
library(caret)
library(SnowballC)

data.train <- read.table("2014CensusTraining.csv",sep=",",header=T)

F5 <- rpart(class ~ age  + workclass + fnlwgt + education + education.num, data.train)
print(F5)
plot(F5)
text(F5)
F10 <- rpart(class ~ age  + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex, data.train)
print(F10)
plot(F10)
text(F10)
F14 <- rpart(class ~ age  + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours.per.week + native.country, data.train)
print(F14)
plot(F14)
text(F14)

data.half <- read.table("2014HalfCensusTraining.csv",sep=",",header=T)

H5 <- rpart(class ~ age  + workclass + fnlwgt + education + education.num, data.half)
print(H5)
plot(H5)
text(H5)
H10 <- rpart(class ~ age  + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex, data.half)
print(H10)
plot(H10)
text(H10)
H14 <- rpart(class ~ age  + workclass + fnlwgt + education + education.num + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours.per.week + native.country, data.half)
print(H14)
plot(H14)
text(H14)

#############
data.test <- read.table("2014NewCensusTest.csv",sep=",",header=T)

predF5 <- predict(F5,data.test,type="class")
sensitivity(predF5, data.test$class)
specificity(predF5, data.test$class)
accuracy(predF5)

predF10 <- predict(F10,data.test,type="class")
sensitivity(predF10, data.test$class)
specificity(predF10, data.test$class)
accuracy(predF10)


predF14 <- predict(F14,data.test,type="class")
sensitivity(predF14, data.test$class)
specificity(predF14, data.test$class)
accuracy(predF14)


predH5 <- predict(H5,data.test,type="class")
sensitivity(predH5, data.test$class)
specificity(predH5, data.test$class)
accuracy(predH5)



predH10 <- predict(H10,data.test,type="class")
sensitivity(predH10, data.test$class)
specificity(predH10, data.test$class)
accuracy(predH10)



predH14 <- predict(H14,data.test,type="class")
sensitivity(predH14, data.test$class)
specificity(predH14, data.test$class)
accuracy(predH14)


accuracy = function (a) {
  a = table(a,data.test$class)
  rownames(a) <- c('r1', 'r2')
  colnames(a) <- c('c1', 'c2')
  tp = a['r1','c1']
  tn = a['r2','c2']
  return((tp+tn)/sum(a))
}



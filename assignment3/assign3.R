##load data
setwd("/Users/tuo/Desktop/cpsc340/assignment3")
getwd()
train<- read.table("Assign3trainMissingValues.csv", sep = ",", header = TRUE)
test <- read.table("Assign3Test.csv", sep = ",", header = TRUE)
true <- read.table("Assign3TrueValues.csv", sep = ",", header = TRUE)
head(train)
head(test)
head(true)

##helper function
perf = function(cut, mod, y)
{
  yhat = (mod$fit>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  out = t(as.matrix(c(sensitivity, specificity)))
  colnames(out) = c("sensitivity", "specificity")
  return(out)
}

##part 1 
train.full <- train[complete.cases(train),]
MClean <- glm(default10yr ~ income + age + loan, data = train.full, family="binomial")

perf(0.2,MClean,train.full$default10yr)

sensetivity=c()
specifity=c()

for(i in seq(0.1, 0.9, by=0.1)){
  sensetivity <- c(sensetivity,perf(i,MClean,train.full$default10yr)[1])
  specifity <- c(specifity,perf(i,MClean,train.full$default10yr)[2])
}

#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensetivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)


predictClean <- ifelse(predict(MClean, newdata=test,type="response")>0.2, 1, 0)
confusionmatrixClean <- table(predict1, test$default10yr)
confusionmatrixClean

##part2
train.NA <- train[!complete.cases(train),]
RIncome <- lm(loan ~ income, data = train.full)
RAge <- lm(loan ~ age, data = train.full)
RBoth <- lm(loan ~ income + age, data = train.full)

predictionIncome <- predict(RIncome,interval="prediction",newdata=train.NA)
predictionAge <- predict(RAge,interval="prediction",newdata=train.NA)
predictionBoth <- predict(RBoth,interval="prediction",newdata=train.NA)
predictionIncome <- as.data.frame(predictionIncome)
predictionAge <- as.data.frame(predictionAge)
predictionBoth <- as.data.frame(predictionBoth)
errorIncome <- sum(((true$loan) - (predictionIncome$fit))^2)
errorIncome
errorAge <- sum(((true$loan) - (predictionAge$fit))^2)
errorAge
errorBoth <- sum(((true$loan) - (predictionBoth$fit))^2)
errorBoth

##part3
train.new <- within(train.NA, loan <- predictionIncome$fit)
train.final <- rbind(train.full, train.new)

MIncome <- glm(default10yr ~ income + loan, data = train.final, family="binomial")
perf(0.16,MIncome,train.final$default10yr)
sensetivity=c()
specifity=c()
for(i in seq(0.1, 0.9, by=0.1)){
  sensetivity <- c(sensetivity,perf(i,MIncome,train.final$default10yr)[1])
  specifity <- c(specifity,perf(i,MIncome,train.final$default10yr)[2])
}
#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensetivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)


MAge <- glm(default10yr ~ age + loan, data = train.final, family="binomial")
perf(0.175,MAge,train.final$default10yr)
sensetivity=c()
specifity=c()
for(i in seq(0.1, 0.9, by=0.1)){
  sensetivity <- c(sensetivity,perf(i,MAge,train.final$default10yr)[1])
  specifity <- c(specifity,perf(i,MAge,train.final$default10yr)[2])
}
#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensetivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)


MBoth <- glm(default10yr ~ income + age + loan, data = train.final, family="binomial")
perf(0.18,MBoth,train.final$default10yr)
sensetivity=c()
specifity=c()
for(i in seq(0.1, 0.9, by=0.1)){
  sensetivity <- c(sensetivity,perf(i,MBoth,train.final$default10yr)[1])
  specifity <- c(specifity,perf(i,MBoth,train.final$default10yr)[2])
}
#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensetivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)



predictIncome <- ifelse(predict(MIncome, newdata=test,type="response")>0.16, 1, 0)
confusionmatrixIncome <- table(predictIncome, test$default10yr)
confusionmatrixIncome

predictAge <- ifelse(predict(MAge, newdata=test,type="response")>0.175, 1, 0)
confusionmatrixAge <- table(predictAge, test$default10yr)
confusionmatrixAge

predictBoth <- ifelse(predict(MAge, newdata=test,type="response")>0.18, 1, 0)
confusionmatrixBoth <- table(predictBoth, test$default10yr)
confusionmatrixBoth

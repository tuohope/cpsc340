library(e1071)
library(SnowballC)
library(tm)
library(cvTools)
set.seed(340)
getwd()
setwd("/Users/tuo/Desktop/cpsc340/assignment4")


#Load Data

catstrain <- c(list.files("20news-bydate-train/","comp"),list.files("20news-bydate-train/","talk"))
dirstrain <- sprintf("%s/%s","20news-bydate-train", catstrain)
train <- Corpus(DirSource(dirstrain, encoding = "UTF-8"))

catstest <- c(list.files("20news-bydate-test/","comp"),list.files("20news-bydate-test/","talk"))
dirstest <- sprintf("%s/%s","20news-bydate-test", catstest)
test <- Corpus(DirSource(dirstest, encoding = "UTF-8"))

# Preprocess data
#a. Remove XML from the document
removeXML <- function(x) gsub("<.*?>","",x)
test <- tm_map(test, content_transformer(removeXML))
train <- tm_map(train, content_transformer(removeXML))
#b. Remove the author of the message
removeAuthor <- function(x) gsub("F.*?$","",x)
train <- tm_map(train, content_transformer(removeAuthor))
test <- tm_map(test, content_transformer(removeAuthor))
#c. Remove stop words
myStopwords <- c(stopwords('english'))
train <- tm_map(train, removeWords, myStopwords)
test <- tm_map(test, removeWords, myStopwords)
#d. Remove extra spaces
train <- tm_map(train, content_transformer(stripWhitespace))
test <- tm_map(test, content_transformer(stripWhitespace))
#e. Transform all upper cases to lower case
train <- tm_map(train, content_transformer(tolower))
test <- tm_map(test, content_transformer(tolower))
#f. Remove punctuations
train <- tm_map(train, content_transformer(removePunctuation))
test <- tm_map(test, content_transformer(removePunctuation))
#g. Remove numbers
train <- tm_map(train, removeNumbers)
test <- tm_map(test, removeNumbers)

#Create document-term matrix using TFIDF and stemming options in DocumentTermMatrix() function in the tm package.
doc_term_matrix.train<-DocumentTermMatrix(train,control = list(stemming = TRUE,weighting = weightTfIdf))
doc_term_matrix.train <- removeSparseTerms(doc_term_matrix.train,0.921)
m.train <- as.data.frame(inspect( doc_term_matrix.train ))
doc_term_matrix.test<-DocumentTermMatrix(test,control = list(weighting = weightTfIdf,stemming = TRUE))
doc_term_matrix.test <- removeSparseTerms(doc_term_matrix.test,0.9189)
m.test <- as.data.frame(inspect( doc_term_matrix.test ))

library(caret)


classes <- c(rep("comp",2936),rep("talk",1952))                                                                                            
classes.test <- c(rep("comp",1955),rep("talk",1301))               

train_classes <- factor(classes)
test_classes <- factor(classes.test)
SVM <- svm(m.train,train_classes,kernel="linear")
PredictionSVM <- predict(SVM,m.test)
table(PredictionSVM,test_classes)
prop.table(table(test_classes==PredictionSVM))

sensitivity(PredictionSVM, test_classes)
specificity(PredictionSVM, test_classes)

#part2 
#reload data and process
train <- Corpus(DirSource(dirstrain, encoding = "UTF-8"))
test <- Corpus(DirSource(dirstest, encoding = "UTF-8"))

final <- c(test,train)
final <- tm_map(final, content_transformer(removeXML))
final <- tm_map(final,content_transformer(removeAuthor))
final <- tm_map(final, content_transformer(removeWords), c(stopwords("english")))
final <- tm_map(final,content_transformer(stripWhitespace))
final <- tm_map(final, content_transformer(tolower))
final <- tm_map(final, content_transformer(removePunctuation))  
final <- tm_map(final, content_transformer(removeNumbers))

#build class
classall <- c(rep("comp",4891),rep("talk",3253))
classtest <- as.data.frame(classall)
DTM <- DocumentTermMatrix(final,control=list(stemming = TRUE,weighting = weightTfIdf))
DTM <- removeSparseTerms(DTM,0.9199)
DTM.dataframe <- as.data.frame(inspect( DTM ))
p2.withclass <- cbind(DTM.dataframe,classall)

##5 fold CV
folds <- cvFolds(nrow(DTM.dataframe), K = 5, R = 1)
group1 <- p2.withclass[folds$subsets[folds$which == 1,], ]
group2 <- p2.withclass[folds$subsets[folds$which == 2,], ]
group3 <- p2.withclass[folds$subsets[folds$which == 3,], ]
group4 <- p2.withclass[folds$subsets[folds$which == 4,], ]
group5 <- p2.withclass[folds$subsets[folds$which == 5,], ]

class1 <- group1$classall
class2 <- group2$classall
class3 <- group3$classall
class4 <- group4$classall
class5 <- group5$classall
group1$classall <- NULL
group2$classall <- NULL
group3$classall <- NULL
group4$classall <- NULL
group5$classall <- NULL

train1 <- rbind(group2,group3,group4,group5)
train2 <- rbind(group1,group3,group4,group5)
train3 <- rbind(group1,group2,group4,group5)
train4 <- rbind(group1,group2,group3,group5)
train5 <- rbind(group1,group2,group3,group4)
train1.class <- c(class2,class3,class4,class5)
train2.class <- c(class1,class3,class4,class5)
train3.class <- c(class1,class2,class4,class5)
train4.class <- c(class1,class2,class3,class5)
train5.class <- c(class1,class2,class3,class4)
train1.class <- factor(train1.class)
train2.class <- factor(train2.class)
train3.class <- factor(train3.class)
train4.class <- factor(train4.class)
train5.class <- factor(train5.class)

test1 <- group1
test2 <- group2
test3 <- group3
test4 <- group4
test5 <- group5
test1.class <- class1
test2.class <- class2
test3.class <- class3
test4.class <- class4
test5.class <- class5
test1.class <- factor(test1.class)
test2.class <- factor(test2.class)
test3.class <- factor(test3.class)
test4.class <- factor(test4.class)
test5.class <- factor(test5.class)


#First validation====================================
SVM <- svm(train1,train1.class,kernel="linear")
PredictionSVM <- predict(SVM,test1)
table(PredictionSVM,test1.class)
#second validation===================================
SVM <- svm(train2,train2.class,kernel="linear")
PredictionSVM <- predict(SVM,test2)
table(PredictionSVM,test2.class)
#third validation====================================
SVM <- svm(train3,train3.class,kernel="linear")
PredictionSVM <- predict(SVM,test3)
table(PredictionSVM,test3.class)
#forth validation====================================
SVM <- svm(train4,train4.class,kernel="linear")
PredictionSVM <- predict(SVM,test4)
table(PredictionSVM,test4.class)
#fifth validation====================================
SVM <- svm(train5,train5.class,kernel="linear")
PredictionSVM <- predict(SVM,test5)
table(PredictionSVM,test5.class)

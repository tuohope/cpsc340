library(e1071)
library(SnowballC)
library(tm)

#Load Data
catstrain <- list.files("20news-bydate-train/")
dirstrain <- sprintf("%s/%s","20news-bydate-train", catstrain)
train <- Corpus(DirSource(dirstrain, encoding = "UTF-8"))

catstest <- list.files("20news-bydate-test/")
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
doc_term_matrix.train <- removeSparseTerms(doc_term_matrix.train,0.88)
m.train <- as.data.frame(inspect( doc_term_matrix.train ))
doc_term_matrix.test<-DocumentTermMatrix(test,control = list(weighting = weightTfIdf,stemming = TRUE))
doc_term_matrix.test <- removeSparseTerms(doc_term_matrix.test,0.8813)
m.test <- as.data.frame(inspect( doc_term_matrix.test ))

classes <- c(rep("negative",8378), rep("postive",584+591+590+578+593))
classes.test <- c(rep("negative",5577), rep("postive",389+394+392+385+395))


classes <- c(train_test_set$class)
train_df_withoutclass <- head (train_test_dataframe,200)
test_df_withoutclass <- tail (train_test_dataframe,100)
train_classes <- factor(head (classes,200))
test_classes <- factor(tail(classes,100))

train_classes <- factor(classes)
test_classes <- factor(classes.test)
SVM <- svm(m.train,train_classes,kernel="linear")
PredictionSVM <- predict(SVM,m.test)
table(PredictionSVM,test_classes)
prop.table(table(test_classes==PredictionSVM))





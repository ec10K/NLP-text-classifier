library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(wordcloud)
library(gmodels)

text.df1 <- read.csv("/Users/elvis/Desktop/POS_Training_sample_v2.csv")
glimpse(text.df1)
text.df1$POS_ITEM_NAME <- as.character(text.df1$POS_ITEM_NAME)
text.df1$BRAND <- as.character(text.df1$BRAND)
text.df1$FLAVOR <- as.character(text.df1$FLAVOR)
text.df1$PRODUCT <- as.character(text.df1$PRODUCT)
##text.df1$PACKAGING <- as.character(text.df1$PACKAGING)
text.df1$CATEGORY <- as.character(text.df1$CATEGORY)
glimpse(text.df1)

#Randomize
set.seed(1)
text.df1 <- text.df1[sample(nrow(text.df1)), ]
text.df1 <- text.df1[sample(nrow(text.df1)), ]
glimpse(text.df1)

#text.df1$FLAVOR <- as.factor(text.df1$FLAVOR)
text.df1$PACKAGING <- as.factor(text.df1$PACKAGING)


#Tokenization
corpus <- Corpus(VectorSource(text.df1$POS_ITEM_NAME))

#Clean up data by lowercasing + removing numbers & punctuation
corpus.clean <- tm_map(corpus, removeNumbers)
corpus.clean <- tm_map(corpus.clean, tolower)
corpus.clean <- corpus.clean <- tm_map(corpus.clean, removePunctuation)
corpus.clean

  
#Document term matrix 
dtm <- DocumentTermMatrix(corpus.clean)
#inspect(dtm[40:50, 10:15])
glimpse(text.df1)


#Training and testing split
df.train <- text.df1[1:600,]
df.test <- text.df1[601:2209,]



dtm.train <- dtm[1:600,]
dtm.test <- dtm[601:2209,]

corpus.clean.train <- corpus.clean[1:600]
corpus.clean.test <- corpus.clean[601:2209]

wordcloud(corpus.clean.train, min.freq = 10, random.order = FALSE)

#Features
dim(dtm.train)

#5 most frequent words
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))
dim(dtm.train.nb)

#Word frequencies converted to to binary (YES/NO) outcome
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

#Naive Bayes classifier
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)


#Add Laplace smoothing to account for zero probabilities 
system.time( classifier <- naiveBayes(trainNB, df.train$PACKAGING, laplace = 1) )
system.time( pred <- predict(classifier, newdata=testNB) )



##plot table of Predictions vs Actual counts

#table("Predictions"= pred,  "Actual" = df.test$PACKAGING)


c.table <- CrossTable(pred, df.test$PACKAGING,prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))


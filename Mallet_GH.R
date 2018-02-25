## Google Home Mallet - Topic Modelling

## Set Directory
#setwd("C:/Users/Akanksha/Desktop/ISU/Semester 3/LING 410X/Project/Google Home Tweets")

## Retrieve Data
tw <- read.table("Google_Home_Tweets.txt",encoding = "UTF-8")

## Preprocesssing
preprocess <- function(onlytweets)
{
  onlytweets <- iconv(onlytweets, to = "ASCII", sub = " ") #convert everything to ascii
  onlytweets <- tolower(onlytweets)  #lower case everything
  onlytweets <- gsub("@\\w+", " ", onlytweets)  #Remove username mentions in tweets - they start with @ symbol.
  onlytweets <- gsub("http.+ |http.+$", " ", onlytweets)  #Remove Urls
  onlytweets <- gsub("[[:punct:]]", " ", onlytweets)  #Remove punctuation
  onlytweets <- gsub("^ ", "", onlytweets)  #Leading blanks
  onlytweets <- gsub(" $", "", onlytweets)  #Trailing blanks
  onlytweets <- gsub(" +", " ", onlytweets) #More than one space
  onlytweets <- gsub("rt", " ", onlytweets)  #Remove RT so that duplicates are easy to identify
  onlytweets <- gsub("\\d", " ", onlytweets)
  onlytweets <- unique(onlytweets) # Now get rid of duplicates!
}

x=tw$tweets_frame.text
onlytweets = preprocess(x)

tweets_frame$text
## Mallet
library(mallet)
mallet.instances <- mallet.import(as.character(seq(1:length(onlytweets))),as.character(onlytweets),stoplist.file = "stoplist-sowmya.csv")
topic.model <- MalletLDA(num.topics=5) #10 topics as a trial
topic.model$loadDocuments(mallet.instances)
topic.model$train(400) #400 iterations as a starting point

topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
dim(topic.words.m)
vocabulary <- topic.model$getVocabulary()
length(vocabulary)
head(vocabulary)
vocabulary[1:50]

word.freqs <- mallet.word.freqs(topic.model)
topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
dim(topic.words.m)
rowSums(topic.words.m)
topic.words.m[1:3, 1:3]
colnames(topic.words.m) <- vocabulary
mallet.top.words(topic.model, topic.words.m[imp.row,], 10)

keywords <-  c("security", "police")
topic.words.m[, keywords]

imp.row <- which(rowSums(topic.words.m[, keywords]) ==max(rowSums(topic.words.m[, keywords])))
mallet.top.words(topic.model, topic.words.m[imp.row,], 20)
mallet.top.words(topic.model, topic.words.m[1,], 20)

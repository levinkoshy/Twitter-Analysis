setwd("/Users/chinjupaul/Documents/MIS/Spring 2017/LING 410X/Project/Amazon echo")
library(topicmodels)
library(rJava)
library(mallet)
library(RColorBrewer)
library(wordcloud)

mydata<-read.table("Amazonecho_tweets.txt",header = T)
x1<-mydata$tweets_frame.text
mydata2<-read.table("Google_Home_Tweets.txt",header = T)
x<-mydata2$tweets_frame.text


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
  onlytweets <- unique(onlytweets) # Now get rid of duplicates!
}

#Amzon echo
onlytweets = preprocess(x1)
mallet.instances <- mallet.import(as.character(seq(1:length(onlytweets))),as.character(onlytweets),stoplist.file = "stoplist-sowmya.csv")
topic.model <- MalletLDA(num.topics=5) #10 topics as a trial
topic.model$loadDocuments(mallet.instances)
topic.model$train(400) #400 iterations as a starting point
vocabulary <- topic.model$getVocabulary()
length(vocabulary)
head(vocabulary)
vocabulary[1:50]
word.freqs<-mallet.word.freqs(topic.model)
topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
dim(topic.words.m)
rowSums(topic.words.m)
topic.words.m[1:3,1:3]
colnames(topic.words.m) <- vocabulary
keywords <-  c("improves", "nice")
topic.words.m[, keywords]
imp.row <- which(rowSums(topic.words.m[, keywords]) ==max(rowSums(topic.words.m[, keywords])))
mallet.top.words(topic.model, topic.words.m[imp.row,], 10)


#Amazon echo:wordcloud
topic1<-mallet.top.words(topic.model, topic.words.m[1,],100)
topic1$weights<-topic1$weights*10000
topic1_wordcloud <- wordcloud(words = topic1$words, freq = topic1$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic2<-mallet.top.words(topic.model, topic.words.m[2,],100)
topic2$weights<-topic2$weights*10000
topic2_wordcloud <- wordcloud(words = topic2$words, freq = topic2$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic3<-mallet.top.words(topic.model, topic.words.m[3,],100)
topic3$weights<-topic3$weights*10000
topic3_wordcloud <- wordcloud(words = topic3$words, freq = topic3$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic4<-mallet.top.words(topic.model, topic.words.m[4,],100)
topic4$weights<-topic4$weights*10000
topic4_wordcloud <- wordcloud(words = topic4$words, freq = topic4$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic5<-mallet.top.words(topic.model, topic.words.m[5,],100)
topic5$weights<-topic5$weights*10000
topic5_wordcloud <- wordcloud(words = topic5$words, freq = topic5$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

alldata<-merge(topic1, topic2, by=c("words"), all =TRUE)
alldata2<-merge(topic3, topic4, by=c("words"), all =TRUE)
alldata2<-merge(alldata2, topic5, by=c("words"), all =TRUE)
alldata<-merge(alldata, alldata2, by=c("words"), all =TRUE)
alldata[is.na(alldata)] <- 0
colnames(alldata) <- c("word","topic1", "topic2","topic3","topic4","topic5")
rownames(alldata)<- alldata$word
alldata<-subset(alldata,select = c(topic1,topic2,topic3,topic4,topic5))
comparison.cloud(alldata, random.order=FALSE, colors = c("#00B2FF","violet","orange","green","red"), title.size=1.5, max.words=500)

#Google home
onlytweets1 = preprocess(x)
mallet.instances1 <- mallet.import(as.character(seq(1:length(onlytweets1))),as.character(onlytweets1),stoplist.file = "stoplist-sowmya.csv")
topic.model1 <- MalletLDA(num.topics=5) #10 topics as a trial
topic.model1$loadDocuments(mallet.instances1)
topic.model1$train(400) #400 iterations as a starting point
vocabulary1 <- topic.model$getVocabulary()
length(vocabulary1)
head(vocabulary1)
vocabulary1[1:50]
word.freqs1<-mallet.word.freqs(topic.model1)
topic.words.n1<- mallet.topic.words(topic.model1,smoothed=TRUE,normalized=TRUE)
dim(topic.words.n1)
rowSums(topic.words.n1)
topic.words.n1[1:3,1:3]
colnames(topic.words.n1) <- vocabulary1
keywords <-  c("improves", "nice")
topic.words.n1[, keywords]
imp.row <- which(rowSums(topic.words.n1[, keywords]) ==max(rowSums(topic.words.n1[, keywords])))
mallet.top.words(topic.model1, topic.words.n1[imp.row,], 10)

#wordcloud for google home
topic1a<-mallet.top.words(topic.model1, topic.words.n1[1,],100)
topic1a$weights<-topic1a$weights*10000
topic1a_wordcloud <- wordcloud(words = topic1a$words, freq = topic1a$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic2a<-mallet.top.words(topic.model1, topic.words.n1[2,],100)
topic2a$weights<-topic2a$weights*10000
topic2a_wordcloud <- wordcloud(words = topic2a$words, freq = topic2a$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic3a<-mallet.top.words(topic.model1, topic.words.n1[3,],100)
topic3a$weights<-topic3a$weights*10000
topic3a_wordcloud <- wordcloud(words = topic3a$words, freq = topic3a$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic4a<-mallet.top.words(topic.model1, topic.words.n1[4,],100)
topic4a$weights<-topic4a$weights*10000
topic4a_wordcloud <- wordcloud(words = topic4a$words, freq = topic4a$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

topic5a<-mallet.top.words(topic.model1, topic.words.n1[5,],100)
topic5a$weights<-topic5a$weights*10000
topic5a_wordcloud <- wordcloud(words = topic5a$words, freq = topic5a$weights, min.freq = 10,
                              max.words=100, random.order=FALSE, rot.per=0.35, 
                              colors=brewer.pal(8, "Dark2"))

alldata<-merge(topic1a, topic2a, by=c("words"), all =TRUE)
alldata2<-merge(topic3a, topic4a, by=c("words"), all =TRUE)
alldata2<-merge(alldata2, topic5a, by=c("words"), all =TRUE)
alldata<-merge(alldata, alldata2, by=c("words"), all =TRUE)
alldata[is.na(alldata)] <- 0
colnames(alldata) <- c("word","topic1", "topic2","topic3","topic4","topic5")
rownames(alldata)<- alldata$word
alldata<-subset(alldata,select = c(topic1,topic2,topic3,topic4,topic5))
comparison.cloud(alldata, random.order=FALSE, colors = c("#00B2FF","violet","orange","green","red"), title.size=1.5, max.words=500)

#comparison cloud between amazon echo and google home

alldatax<-merge(topic1, topic1a, by=c("words"), all =TRUE)
alldatax[is.na(alldatax)] <- 0
colnames(alldatax) <- c("word","topic1_amazon", "topic1_google")
rownames(alldatax)<- alldatax$word
alldatax<-subset(alldatax,select = c(topic1_google,topic1_amazon))
comparison.cloud(alldatax, random.order=FALSE, colors = c("#00B2FF","red"), title.size=1.5, max.words=500)

alldatay<-merge(topic4, topic3a, by=c("words"), all =TRUE)
alldatay[is.na(alldatay)] <- 0
colnames(alldatay) <- c("word","topic5_amazon", "topic4_google")
rownames(alldatay)<- alldatay$word
alldatay<-subset(alldatay,select = c(topic4_google,topic5_amazon))
comparison.cloud(alldatay, random.order=FALSE, colors = c("violet","orange"), title.size=1.5, max.words=500)

alldataz<-merge(topic5, topic4a, by=c("words"), all =TRUE)
alldataz[is.na(alldataz)] <- 0
colnames(alldataz) <- c("word","topic4_amazon", "topic5_google")
rownames(alldataz)<- alldataz$word
alldataz<-subset(alldataz,select = c(topic5_google,topic4_amazon))
comparison.cloud(alldataz, random.order=FALSE, colors = c("green","red"), title.size=1.5, max.words=500)







  
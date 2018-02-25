library(ROAuth)
library(twitteR)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
my_api_key <- "GkqKxCRjecJQy5DvWcCRHI79L"
my_api_secret <-"aHcYBLAOoz6T5ZabUcBiH60V27mBDNWr0ixXTH1tRLHnIprGeJ"
my_oauth <- OAuthFactory$new(consumerKey=my_api_key, consumerSecret=my_api_secret, requestURL=requestURL, 
                             accessURL=accessURL, authURL=authURL)
save(my_oauth, file="oauth_token.Rdata")
accessToken = '847455640528183312-VPCd1CRTsP3pcR5LidqzJUNgj2Mj0tR'
#Enter your access token

accessSecret = 'NoH6pTxNNUVpnl1A881dTYozVJMKiRmML7rt6IcgAzCJ1'
#enter your access token secret.

setup_twitter_oauth(consumer_key=my_api_key, consumer_secret=my_api_secret, access_token=accessToken, access_secret=accessSecret)
tweets <- searchTwitter("Google Home",n=3200,since="2017-04-23")
tweets
tweets_frame <- twListToDF(tweets)
tweets_frame$text
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

onlytweets = preprocess(tweets_frame$text)
mallet.instances <- mallet.import(as.character(seq(1:length(onlytweets))),as.character(onlytweets),stoplist.file = "stoplist-sowmya.csv")
topic.model <- MalletLDA(num.topics=10) #10 topics as a trial
topic.model$loadDocuments(mallet.instances)
topic.model$train(400) #400 iterations as a starting point

topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
dim(topic.words.m)
vocabulary <- topic.model$getVocabulary()
length(vocabulary)
head(vocabulary)
vocabulary[1:50]

colnames(topic.words.m) 

word.freqs <- mallet.word.freqs(topic.model)
topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
dim(topic.words.m)
rowSums(topic.words.m)
topic.words.m[1:3, 1:3]
colnames(topic.words.m) <- vocabulary
mallet.top.words(topic.model, topic.words.m[imp.row,], 10)
##keywords <- c("california", "ireland")


keywords <-  c("improves", "nice")
topic.words.m[, keywords]

imp.row <- which(rowSums(topic.words.m[, keywords]) ==max(rowSums(topic.words.m[, keywords])))
mallet.top.words(topic.model, topic.words.m[imp.row,], 10)
mallet.top.words(topic.model, topic.words.m[3,], 10)
clouds <- function(topic.words.m)
{
  for(i in 1:10)
  {
    topic.top.words <- mallet.top.words(topic.model,topic.words.m[i,], 100)
    wordcloud(topic.top.words$words,topic.top.words$weights,c(4,.8), rot.per=0, random.order=F)
  }
}

par(mfrow=c(4,3))
clouds(topic.words.m)


df <- data.frame(tweets_frame$text)
df[1]
write.table(df,"C:/Users/Akanksha/Desktop/ISU/Semester 3/LING 410X/Project/output_23Apr_GH.txt")
tw <- read.table("C:/Users/Akanksha/Desktop/ISU/Semester 3/LING 410X/Project/output_23Apr_GH.txt")
nrow(tw)


--------------------------------------
  tweets_frame <- twListToDF(tweets)
tweets_frame
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
onlytweets = preprocess(tweets_frame$text)
onlytweets
library(mallet)
mallet.instances <- mallet.import(as.character(seq(1:length(onlytweets))),as.character(onlytweets),stoplist.file ="Akanksha_StopWords.csv")
topic.model <- MalletLDA(num.topics=10) #10 topics as a trial
topic.model$loadDocuments(mallet.instances)
topic.model$train(400)
topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
colnames(topic.words.m) <- topic.model$getVocabulary()
colnames(topic.words.m)
topic.words.m[1:10, 1:10]
mallet.top.words(topic.model, topic.words.m[8,], 10)
library(wordcloud)
topic.top.words <- mallet.top.words(topic.model,topic.words.m[3,], 10)
wordcloud(topic.top.words$words,topic.top.words$weights,c(5,0.5), rot.per=0, random.order=FALSE)



library(wordcloud)
topic.top.words <- mallet.top.words(topic.model,topic.words.m[1,], 100)
wordcloud(topic.top.words$words,topic.top.words$weights,c(5,0.5), rot.per=0.35, random.order=F)
clouds <- function(topic.words.m)
{
  for(i in 1:10)
  {
    topic.top.words <- mallet.top.words(topic.model,topic.words.m[i,], 100)
    wordcloud(topic.top.words$words,topic.top.words$weights,c(4,.8), rot.per=0, random.order=F)
  }
}
par(mfrow=c(4,3))
clouds(topic.words.m)


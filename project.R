#Source: https://github.com/pablobarbera/social-media-workshop/blob/master/01-twitter-data-collection.r
#setwd("/Users/chinjupaul/Documents/MIS/Spring 2017/LING 410X/week 11")
install.packages("base64enc")
install.packages("httr", dependencies = TRUE)
library(ROAuth)
library(twitteR)
library(base64enc)
library(RCurl)
library(bitops)
library(mallet)
library(wordcloud)
library(NLP)
library(tm)
library(topicmodels)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_api_key <- "GTDBupan7qqGE1wZiGhXz0YF7"
#Enter your API KEY

my_api_secret <- "Y7Y4R5WQ4qDS8X062jDqSLBP1fi9xr9jVA0vDCXBJwyJay2LTZ"
#Enter your API secret

my_oauth <- OAuthFactory$new(consumerKey=my_api_key, consumerSecret=my_api_secret, requestURL=requestURL, 
                             accessURL=accessURL, authURL=authURL)

#I did not have to do these following lines on my laptop. Check if you have to do these.
#my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#This will show a message and direct us to our twitter account and gives  us a PIN once we authorize
#Type the PIN:

save(my_oauth, file="oauth_token.Rdata")
#This will store this information on this computer so that you don't have to repeat this process later.

accessToken = '136255059-7y3TTbXpN7YsMVUmxvl3SkSx95enGfgCBtNUyAAX'
#Enter your access token

accessSecret = 'XWmPPLEloEt7lLPrQBe6XkF6wMcuju1ZrPZMi2FH0ObwG'
#enter your access token secret.

setup_twitter_oauth(consumer_key=my_api_key, consumer_secret=my_api_secret, access_token=accessToken, access_secret=accessSecret)

#search by a keyword for most recent tweets.
tweets <- searchTwitter("Google Home",n=1200,since="2017-04-13",until="2017-04-14")
tweets
tweets_frame <- twListToDF(tweets)
tweets_frame$text
df <- data.frame(tweets_frame$text)
df
write.table(df,"C:/Users/Levin/ISU/Spring/Ling 410x/Project/GoogleHome_13Apr14Apr.txt")
tw <- read.table("C:/Users/Levin/ISU/Spring/Ling 410x/Project/GoogleHome_13Apr14Apr.txt")
tw



tweets <- searchTwitter("under armor", n=1000)
tweets
## from a Windows machine:
# searchTwitter("Iowa", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#windows users need to add this cainfo= string for all these functions, apparently.

tweets_frame <- twListToDF(tweets)
tweets_frame$text

#converts tweets to a table like structure which is easy to work with.
corpus<- Corpus(VectorSource(tweets_frame$text))

my_corpus <- tm_map(corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, PlainTextDocument)
my_corpus
my_corpus<-Corpus(VectorSource(my_corpus))
myDtm <- DocumentTermMatrix(my_corpus)
myDtm

lda<-LDA(myDtm, k=10)
term<-terms(lda,10)
(term<-apply(term,MARGIN = 2,paste, collapse=", "))
topics<-topics(lda)
topics


#Pre-processing of tweets
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

#Add tweets to mallet. With what ids? 
onlytweets = preprocess(tweets_frame$text)
mallet.instances <- mallet.import(as.character(seq(1:length(onlytweets))),as.character(onlytweets),stoplist.file = "stoplist-sowmya.csv")
topic.model <- MalletLDA(num.topics=10) #10 topics as a trial
topic.model$loadDocuments(mallet.instances)
topic.model$train(400) #400 iterations as a starting point

topic.words.m <- mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)
colnames(topic.words.m) <- topic.model$getVocabulary()
topic.words.m[1:10,1:10]
colnames(topic.words.m)

mallet.top.words(topic.model, topic.words.m[1,], 10)
mallet.top.words(topic.model, topic.words.m[2,], 10)
mallet.top.words(topic.model, topic.words.m[10,], 10)
#Function to plot a word cloud for all topics.
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

#ToDo: prepare a better stopword list. Play with topics. Compare two locations interms of topics. Search by keywords or hashtags. 
#Change pre-processing to what you want. and so on. 












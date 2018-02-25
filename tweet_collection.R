library(ROAuth)
library(twitteR)

setwd("/Users/chinjupaul/Documents/MIS/Spring 2017/LING 410X/Project/Amazon echo")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_api_key <- "xxx" 
#Enter your API KEY

my_api_secret <- "xxx"
#Enter your API secret

my_oauth <- OAuthFactory$new(consumerKey=my_api_key, consumerSecret=my_api_secret, requestURL=requestURL, 
                             accessURL=accessURL, authURL=authURL)

#I did not have to do these following lines on my laptop. Check if you have to do these.
#my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#This will show a message and direct us to our twitter account and gives  us a PIN once we authorize
#Type the PIN:

save(my_oauth, file="oauth_token.Rdata")
#This will store this information on this computer so that you don't have to repeat this process later.

accessToken = 'xxx'
#Enter your access token

accessSecret = 'xxx'
#enter your access token secret.

setup_twitter_oauth(consumer_key=my_api_key, consumer_secret=my_api_secret, access_token=accessToken, access_secret=accessSecret)

#search by a keyword for most recent tweets.

tweets <- searchTwitter("Amazon Echo", n=3200, since = "2017-05-01",until = "2017-05-02")
#tweets <- searchTwitter("Google Home", n=3200, since = "2017-04-19",until = "2017-04-20")
tweets

head(tweets)
## from a Windows machine:
# searchTwitter("Iowa", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#windows users need to add this cainfo= string for all these functions, apparently.

tweets_frame <- twListToDF(tweets)
names(tweets_frame)
df<-data.frame(tweets_frame$text)
write.table(df,"/Users/chinjupaul/Documents/MIS/Spring 2017/LING 410X/Project/Amazon echo/echomay1_2.txt")
#converts tweets to a table like structure which is easy to work with.


t1<-read.table("output_11Apr12Apr.txt",header = T)
t2<-read.table("april_13,14.txt",header = T)
t3<-read.table("output_15Apr16Apr.txt",header = T)
t4<-read.table("output_17Apr18Apr.txt",header = T)
t5<-read.table("output_19Apr20Apr.txt",header = T)
t6<-read.table("echoapril_20,21.txt",header = T)
mydata<-rbind(t1,t2,t3,t4,t5,t6)
write.table(mydata,"/Users/chinjupaul/Documents/MIS/Spring 2017/LING 410X/Project/Amazon echo/Amazonecho_tweets.txt")

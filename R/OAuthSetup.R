setwd("~/data")
auth <-read.csv(file = "~/data/twitterapp.csv", header = TRUE, stringsAsFactors = FALSE)
ck <- auth$consumer.key
cs <- auth$consumer.secret

#Using ROAuth - Now Deprecated in twitteR library
library(ROAuth)

#OAuth Setup for R Script App in twitter.com/timalosi

# Download "cacert.pem" file
# Windows only
#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
cred <- OAuthFactory$new(consumerKey= ck,
                         consumerSecret= cs,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="/etc/ssl/certs/ca-certificates.crt")

#save for later use for Windows
save(cred, file="~/data/twitter authentication.Rdata")


#Testing
library(twitteR)
#load("~/data/twitter authentication.Rdata")
#registerTwitterOAuth(cred)



#Test using setup_twitter_oauth

#Testing
library(twitteR)

twitteR::setup_twitter_oauth(ck, cs)

search.string <- "#nba"
no.of.tweets <- 100

tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
tweets

avail_trends=availableTrendLocations()
head(avail_trends)

#United States / United States = 23424977
getTrends(23424977)






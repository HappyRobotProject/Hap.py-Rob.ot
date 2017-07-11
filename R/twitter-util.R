#
# Twitter Helper Functions
#
setwd("/Users/tim/data")
# Load libraries
library(stringr)


#Simple function to allow another script to check if this is loaded
twitterutilversion <- function(){
  return("0.0.1")
}

twPath <- function(){
  return("/Users/tim/data")
}


# Extract authentication keys from a csv file
twkeys <- function(){
  fileName <- paste(twPath(),"/auth/twitter-HRP.csv", sep = "")
  auth <-read.csv(file = fileName, header = TRUE, stringsAsFactors = FALSE)
  ck <- auth$consumer.key
  cs <- auth$consumer.secret
  at <- auth$access.token
  as <- auth$access.secret
  an <- auth$appname
  return (list(consumer_key=ck,consumer_secret=cs,access_token=at,access_secret=as,app_name=an))
}

twSetRtweetToken <- function(token){
  fileName <- paste(twPath(),"/auth/rtweet-token-HRP.rds", sep = "")
  ## save token
  saveRDS(token, file = fileName)
}
twGetRtweetToken <- function(){
  fileName <- paste(twPath(),"/auth/rtweet-token-HRP.rds", sep = "")
  ## read token
  token <- readRDS(file = fileName)
}

twRemoveUserNames <- function(tweets){
  #Check for char array?
  results <- str_replace_all(tweets,"@\\w+", "")
  return(results)
}

twExtractUserNames <- function(tweets){
  #Check for char array?
  results <- str_extract_all(tweets,"@\\w+")
  return(results)
}

twRemoveHashtags <- function(tweets){
  #Check for char array?
  results <- str_replace_all(tweets,"#\\w+", "")
  return(results)
}

twExtractHashtags <- function(tweets){
  #Check for char array?
  results <- str_extract_all(tweets,"#\\w+")
  return(results)
}

twRemoveLinks <- function(tweets){
  #Check for char array?
  #results <- str_replace_all(tweets,"http://\\w+", "")
  #results <- str_replace_all(results,"https://\\w+", "")
  results <- str_replace_all(tweets," ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "")
  #results <- str_replace_all(tweets," ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")
  return(results)
}

twPrepTweetsForCloud <- function(tweets){
  results <- twRemoveHashtags(tweets)
  results <- twRemoveUserNames(results)
  results <- twRemoveLinks(results)
  #remove remaining non-printing characters
  results <- str_replace_all(results, "[^[:alnum:]///' ]", "")
  
  return(results)
}

twPrepTweetsForSentiment <- function(tweets){
  results <- twRemoveHashtags(tweets)
  results <- twRemoveUserNames(results)
  results <- twRemoveLinks(results)
  #remove remaining non-printing characters
  results <- str_replace_all(results, "[^[:alnum:]///' ]", "")
  return(results)
}

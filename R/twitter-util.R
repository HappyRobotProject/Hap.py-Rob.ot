#
# Twitter Helper Functions
#

# Load libraries
library(stringr)


#Simple function to allow another script to check if this is loaded
twitterutilversion <- function(){
  return("0.0.1")
}


# Extract authentication keys from a csv file
twkeys <- function(){
  auth <-read.csv(file = "~/data/auth/twitter-HRP.csv", header = TRUE, stringsAsFactors = FALSE)
  ck <- auth$consumer.key
  cs <- auth$consumer.secret
  at <- auth$access.token
  as <- auth$access.secret
  return (list(consumer_key=ck,consumer_secret=cs,access_token=at,access_secret=as))
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

#
#########################################################
# Script Setup
#########################################################
setwd("/Users/tim/data")
db_name <- "/Users/tim/data/HRP-DailyTrends.sqlite"
db_connections <- "/Users/tim/data/HRP-Network.sqlite"
#########################################################
# Librarys
#########################################################
library(stringi)
library(dplyr)
library(RSQLite)
library(rtweet)
library(jsonlite)

# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("/Users/Tim/code/Hap.py-Rob.ot/R/twitter-util.R")

# Get the quote of the day
qod <- fromJSON("https://quotes.rest/qod")
success <- qod$success$total

if (success == 1){
  
  # Process the quote ... paste all necessary parts and then split into lines (tweets)
  content <- qod$contents$quotes
  copyright <- qod$contents$copyright
  quote <- content$quote
  header <- "#QuoteOfDay \""
  author <- content$author
  
  full_text <- paste(header, quote, '" ', author, " - Copyright ", copyright, sep = "")
  
  wrapped <- strwrap(full_text,width = 127)
  
  
  #########################################################
  # Send the tweets
  #########################################################
  #Get a token
  tokn <- twGetRtweetToken()
  
  len <- length(wrapped)
  i <- 1
  for (line in wrapped){
    if (i == 1 & len == 1){
      post_tweet(status = line, token = tokn)
    } else if (i ==1) {
      post_tweet(status = paste(line ,"...", sep = " "), token = tokn)
    } else if (i == len){
      post_tweet(status = paste("...", line, sep = " "), token = tokn)
    }
    else {
      post_tweet(status = paste("...", line ,"...", sep = " "), token = tokn)
    }
    i <- i + 1
    print(i)
  }


} #End of If on success

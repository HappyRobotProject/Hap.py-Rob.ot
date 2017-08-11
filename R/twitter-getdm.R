#
#########################################################
# Script Setup
#########################################################
setwd("/Users/tim/data")
db_name <- "/Users/tim/data/HRP-DailyTrends.sqlite"
db_connections <- "/Users/tim/data/HRP-Network.sqlite"
#########################################################
# Library
#########################################################
library(stringi)
library(dplyr)
library(RSQLite)
library(twitteR)

# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("/Users/tim/code/Hap.py-Rob.ot/R/twitter-util.R")
#source("~/code/Hap.py-Rob.ot/R/twitter-util.R")

#Authenticate for Twitter
keys <- twkeys()
token <- twitteR::setup_twitter_oauth(keys$consumer_key,keys$consumer_secret,keys$access_token,keys$access_secret)

dm_list <- dmGet()
messages <- data.frame(id=character(),text=character(),len=integer(), sender_id = character(), sender_screen_name = character(), stringsAsFactors = FALSE)
for (dm in dm_list){
  text <- dm$text
  len <- nchar(as.character(text))
  if (len < 50){
    senderID <- dm$senderID
    senderSN <- dm$senderSN
    id <- dm$id
    message <- data.frame(id=as.character(id),text=as.character(text),len = len, sender_id = as.character(senderID), sender_screen_name = as.character(senderSN), stringsAsFactors = FALSE)
    messages <- bind_rows(messages, message)
  }
}

#Write out the DM table
if(nrow(messages)>0){
  db <- dbConnect(SQLite(), dbname=db_name)
  dbWriteTable(conn = db, name = "messages", value = messages, row.names = FALSE, append = TRUE)
  dbDisconnect(db)
}

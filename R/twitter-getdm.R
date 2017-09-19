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
db <- dbConnect(SQLite(), dbname=db_name)
dbNetwork <- dbConnect(SQLite(), dbname=db_connections)
#messages <- data.frame(id=character(),text=character(),len=integer(), sender_id = character(), sender_screen_name = character(), created = integer(), stringsAsFactors = FALSE)
for (dm in dm_list){
  text <- as.character(dm$text)
  len <- nchar(text)
  if (len < 50){
    senderID <- as.character(dm$senderID)
    senderSN <- as.character(dm$senderSN)
    id <- as.character(dm$id)
    messageCheck <- dbGetQuery(conn = db, "Select * from messages where id = :id", param = list(id=id))
    followCheck <- dbGetQuery(conn = dbNetwork, "Select * from twitter_connections where screenname = :sn", param = list(sn=twProcessScreenname(senderSN)))
    print(paste("nrow",id,nrow(messageCheck),nrow(followCheck),sep = ":"))
    if(nrow(followCheck) > 0 & nrow(messageCheck) == 0){
      dbExecute(conn = db,"Insert into messages ('id', 'text', 'len','sender_id', 'sender_screen_name', 'created') values (:id, :tx, :len, :sid, :sn, 0)", param = list(id=id,tx=text, len=len, sid=senderID, sn=senderSN))
    }
    #message <- data.frame(id=as.character(id),text=as.character(text),len = len, sender_id = as.character(senderID), sender_screen_name = as.character(senderSN), created = 0, stringsAsFactors = FALSE)
    #messages <- bind_rows(messages, message)
  }
}

dbDisconnect(db)
dbDisconnect(dbNetwork)
updateMessages <- function(senderID, senderSN, id){
  #Select
  #Update or Insert
}

# #Write out the DM table
# if(FALSE & nrow(messages)>0){
#   db <- dbConnect(SQLite(), dbname=db_name)
#   dbWriteTable(conn = db, name = "messages", value = messages, row.names = FALSE, append = TRUE)
#   print("Writing Table")
#   dbDisconnect(db)
# }

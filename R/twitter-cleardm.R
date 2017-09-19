#
# Run this script to destroy DMs that cannot be processed.
#
#########################################################
# Script Setup
#########################################################
setwd("/Users/tim/data")
db_name <- "/Users/tim/data/HRP-DailyTrends.sqlite"
db_connections <- "/Users/tim/data/HRP-Network.sqlite"
number_of_tweets <- 1000
dbug <- FALSE

#########################################################
# Libraries
#########################################################
#Libraries
library(rtweet)
library(twitteR)
library(plyr)
library(dplyr)
library(RSQLite)
library(RColorBrewer)
library(tidyr)
library(stringi)
library(extrafont)
library(useful)
#font_import()
loadfonts()
library("tm")
library("SnowballC")
library("wordcloud")

# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("/Users/Tim/code/Hap.py-Rob.ot/R/twitter-util.R")
# check to see if the plot-util.R script is loaded
if(!exists("plotutilversion", mode="function")) source("/Users/Tim/code/Hap.py-Rob.ot/R/plot-util.R")
#source("~/code/Hap.py-Rob.ot/R/plot-util.R")


clearMessages <- function(message, sender_id, sender_sn, message_id){
  print("In clearMessages")
  result <- 0
  print(message)
  ddmResult <- destroyDM(message_id = message_id)
  if(exists("ddmResult") & ddmResult > 0){
    result <- ddmResult
  } else {
    result <- 0
  }
  print("Completing  clearMessages")
  return(result)
}


destroyDM <- function(message_id){
  print("In destroyDM")
  
  messageIdArg <- paste("-d id=",message_id, sep = "")
  destroyArg <- c("/1.1/direct_messages/destroy.json",messageIdArg)
  destroyResult <- system2(command = "/usr/local/bin/twurl", args = destroyArg, stdout = TRUE)
  
  if(exists("destroyResult")){
    return(1)
  } else {
    return(0)
  }
}

#########################################################
# Read in messages to reply to
#########################################################


main <- function(){
  #Read in Messages that have an issue
  db <- dbConnect(SQLite(), dbname=db_name)
  table_exits <- dbExistsTable(conn = db,name = "messages")
  dbDisconnect(db)
  if(!table_exits){
    stop()
  }
  db <- dbConnect(SQLite(), dbname=db_name)
  #messages <- dbReadTable(conn = db, name = "messages")
  messages <- dbGetQuery(conn = db,"Select * from messages where created = -1")
  dbDisconnect(db)
  
  
  #########################################################
  # Iterate through the messages
  #########################################################
  results <- messages %>%
    dplyr::rowwise() %>%
    dplyr::mutate(created=clearMessages(text,sender_id,sender_screen_name, id))
  
  
  
  #########################################################
  # Delete Messages that we have sent
  #########################################################
  db <- dbConnect(SQLite(), dbname=db_name)
  dbExecute(conn = db,"Delete from messages where created = -1")
  dbDisconnect(db)
  
  return(results)
}


endResult <- main()


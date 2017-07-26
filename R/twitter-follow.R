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

# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("/Users/Tim/code/Hap.py-Rob.ot/R/twitter-util.R")

#Add check for followers
dbConnections <- dbConnect(SQLite(), dbname=db_connections)
to_follow <- dbReadTable(conn = dbConnections,"twitter_to_follow")

followScreenName <- function(screen_name, token){
  print(paste("following",screen_name, sep = ":"))
  dbSendStatement(conn = dbConnections,"Insert into twitter_connections ('screenname', 'following', 'following_on') values (:sn, 1, :ts)", param = list(sn=screen_name, ts=as.character(Sys.Date())))
  result <- post_follow(user = screen_name, token = tokn)
  return(result$status_code)
}

#Get a token
tokn <- twGetRtweetToken()

result <- to_follow %>%
  dplyr::rowwise() %>%
  dplyr::mutate(followed = followScreenName(screen_name)) 

#drop the table
dbRemoveTable(conn = dbConnections, name =  "twitter_to_follow")
dbDisconnect(dbConnections)

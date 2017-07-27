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


#########################################################
# Check followers to follow back
#########################################################
#Get a token
tokn <- twGetRtweetToken()

#check to see if we have any followers to follow
follower_ids <- get_followers("Hap_py_Rob_ot",token=tokn)
follower_data <- lookup_users(follower_ids$user_id, token = tokn)
followers <- dplyr::select(follower_data, screen_name, user_id)

#Check to see if we follow these people all ready
dbConnections <- dbConnect(SQLite(), dbname=db_connections)

findScreenName <- function(screen_name){
  results <- dbGetQuery(conn = dbConnections,"Select * from twitter_connections where screenname = ?", str_replace_all(stri_trans_tolower(screen_name),"@",""))
  return(nrow(results)>0)
}

follow <- followers %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sn_exists = findScreenName(screen_name)) %>%
  dplyr::filter(sn_exists == FALSE) %>%
  select(screen_name)
follow$screen_name <- as.character(follow$screen_name)
follow$screen_name <- str_replace_all(stri_trans_tolower(follow$screen_name),"@","")
f1 <- data_frame(screen_name = follow$screen_name)
dbWriteTable(conn = dbConnections,"twitter_to_follow", f1, append = TRUE)
dbDisconnect(dbConnections)

#########################################################
# Follow new user
#########################################################

#Add check for followers
dbConnections <- dbConnect(SQLite(), dbname=db_connections)
to_follow <- dbReadTable(conn = dbConnections,"twitter_to_follow")

followScreenName <- function(screen_name, token){
  sn <- str_replace_all(stri_trans_tolower(screen_name),"@","")
  print(paste("following",sn, sep = ":"))
  dbSendStatement(conn = dbConnections,"Insert into twitter_connections ('screenname', 'following', 'following_on') values (:sn, 1, :ts)", param = list(sn=sn, ts=as.character(Sys.Date())))
  result <- post_follow(user = sn, token = tokn)
  return(result$status_code)
}

result <- to_follow %>%
  dplyr::rowwise() %>%
  dplyr::mutate(followed = followScreenName(screen_name)) 

#drop the table
dbRemoveTable(conn = dbConnections, name =  "twitter_to_follow")
dbDisconnect(dbConnections)



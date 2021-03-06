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
# helper functions
#########################################################
processScreenname <- function(screen_name){
  return (str_replace_all(stri_trans_tolower(as.character(screen_name)),"@",""))
}

updateFollows <- function(screen_name, following){
  if(is.na(following)){
    # Not currently following Insert a new row
    print("Insert")
    results <- dbSendStatement(conn = dbConnections,"Insert into twitter_connections ('screenname', 'follows', 'follows_on') values (:sn, 1, :ts)", param = list(sn=screen_name, ts=as.character(Sys.Date())))
  } else {
    #Update the row
    print("Update")
    results <- dbSendStatement(conn = dbConnections,"Update twitter_connections set follows = 1, follows_on = :ts where  screenname = :sn", param = list(sn=screen_name, ts=as.character(Sys.Date())))
  }
  print(results)
  return(1)
}

updateToFollow <- function(screenname){
  sn <- str_replace_all(stri_trans_tolower(screenname),"@","")
  print(paste("following",sn, sep = ":"))
  dbSendStatement(conn = dbConnections,"Update twitter_connections set following = 1, following_on = :ts where screenname = :sn", param = list(sn=sn, ts=as.character(Sys.Date())))
  result <- post_follow(user = sn, token = tokn)
  return(result$status_code)
}


#########################################################
# Check followers to follow back
#########################################################
#Get a token
tokn <- twGetRtweetToken()

#check to see if we have any followers to follow
follower_ids <- get_followers("Hap_py_Rob_ot",token=tokn)
follower_data <- lookup_users(follower_ids$user_id, token = tokn)
followers <- dplyr::select(follower_data, screenname = screen_name, user_id)

#Check to see if we follow these people all ready
dbConnections <- dbConnect(SQLite(), dbname=db_connections)
twitter_connections <- dbReadTable(conn = dbConnections, "twitter_connections") %>%
  select(-user_id)

followers <- followers %>%
  mutate(screenname =processScreenname(screenname)) %>%
  dplyr::full_join(twitter_connections, by = c("screenname")) 

followersToUpdate <- followers %>%
  dplyr::filter(!is.na(user_id) & is.na(follows)) 

if (nrow(followersToUpdate) > 0){
  followersToUpdate %>%
    rowwise() %>%
    mutate(follows = updateFollows(screenname, following))
}

filtered_connections <- dbReadTable(conn = dbConnections, "twitter_connections") %>%
  filter(is.na(following))

if (nrow(filtered_connections) > 0){
  result <- filtered_connections %>%
    dplyr::rowwise() %>%
    dplyr::mutate(followed = updateToFollow(screenname)) 
}
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

if(nrow(to_follow)) {
  result <- to_follow %>%
    dplyr::rowwise() %>%
    dplyr::mutate(followed = followScreenName(screen_name)) 
}
#drop the table
dbRemoveTable(conn = dbConnections, name =  "twitter_to_follow")
dbDisconnect(dbConnections)



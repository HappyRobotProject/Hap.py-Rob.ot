

#
#
#
#########################################################
# Script Setup
#########################################################
setwd("/Users/tim/data")
db_name <- "/Users/tim/data/HRP-DailyTrends.sqlite"
db_connections <- "/Users/tim/data/HRP-Network.sqlite"
number_of_tweets <- 20
#########################################################
# Library
#########################################################
library(stringi)
library(dplyr)
library(RSQLite)

#Fonts
info.font <- "Impact"
#Colors
info.background <- "#2d3142"
info.white <- "#ffffff"
info.main <- "#7084a5"
info.main.light <- "#A1CEE0"
info.accent <- "#ef8354"
info.accent.light <- "#C52D41"
info.highlight <- "#bfc0c0"

# Configure Theme
basic_theme <- function(){
  theme(
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.margin = unit(c(0,0,0,0),"npc"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    panel.spacing = unit(c(0,0,0,0), "npc"),
    legend.position = "none"
  )
}
bar_theme <- function(){
  theme(
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.margin = unit(c(-.05,-.05,-.05,-.05),"npc"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    panel.spacing = unit(c(0,0,0,0), "npc"),
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
}

processMessageRequest1 <- function(message, sender_id, sender_sn, message_id){
  #print(message)
  print(sender_id)
  #print(sender_sn)
  #print(message_id)
  return(TRUE)
}


processMessageRequest <- function(message, sender_id, sender_sn, message_id){
  print("In processMessageRequest")
  result <- 0
  print(message)
  #prepare message
  query <- as.character(dmPrepMessageForQuery(message))
  #print(sender_id)
  filename <- paste(twPath(),"/images/", sender_id,".png",sep = "")
  #print(filename)
  len <- nchar(query)
  #print(len)
  
  if (len > 1){
    print("len is good ... go")
    cigResult <- createInfoGraphic(query, filename, sender_sn)
    if (exists("cigResult")){
      result <- cigResult
      if (cigResult == 1){
        db <- dbConnect(SQLite(), dbname=db_name)
        dbExecute(conn = db,"Insert into infographics ('query', 'sender_screen_name', 'date_created') values (:q, :sn, :ts)", param = list(q=query,sn=sender_sn, ts=as.character(Sys.Date())))
        dbExecute(conn = db,"Update messages set created = :res where id = :id", param = list(id=message_id, res=result))
        dbDisconnect(db)
      }
    } else {
      return(result)
    }
    
    sdmResult <- sendDM()
  }
  print("Completing  processMessageRequest")
  return(result)
}

createInfoGraphic <- function(query = NULL, filename = NULL, sender = NULL){
  print("In createInfoGraphic")
  #filename <- "timtest.png"
  #query <- "GOT"
  #sender <- "TimTest"
  #print(query)
  #print(filename)
  result <- 0
  
  #########################################################
  # Twitter Extracts
  #########################################################
  #Get a token
  tokn <- twGetRtweetToken()
  tweets  <- rtweet::search_tweets(query, n = number_of_tweets, parse = TRUE,include_rts = FALSE,retryonratelimit = FALSE, lang = "en", token = tokn)
  tweetCount <- nrow(tweets)
  print(tweetCount)
  print(exists("tweets"))
  if(tweetCount > 0){
    
    print("Tweets Received .. processing")
    print(exists("tweets"))
    #print((tweets))
    #########################################################
    # Sentiment Analysis of the Tweets
    #########################################################
    tweetList <- rtTweetsToSentiment(tweetDataFrame = tweets)
    print("2")
    emotionTotals <- as.data.frame(tweetList$emotionTotals)
    tweetPositivity <- as.data.frame(tweetList$positivityTotals)
    print("3")
    emotionPlot <- pltEmotion(emotionTotals)
    donutPlot <- pltDonut(tweetPositivity)
    #############################################################
    # Process tweets for word cloud
    #############################################################
    # get the tweet text
    print("4")
    tweetText <- twPrepTweetsForCloud(tweets$text)
    tweetWords <- twTweetToDocMatrix(tweetText)
    
    graphic_data <- list(query = query, sender = sender, tweetCount = tweetCount, emotionPlot = emotionPlot, donutPlot = donutPlot, tweetWords = tweetWords)
    print("creating image")
    pltResult <- pltCreateImage(graphic_data,filename)
    if (exists("pltResult")){
      result <- pltResult
    } else {
      result <- 0
    }
  }
  print("completing createInfoGraphic")
  return(result)
}

sendDM <- function(sender_id, filename){
  print("In sendDM")
  library(stringi)
  #argv <- c("/1.1/direct_messages.json")
  #dms <- system2(command = "twurl", args = argv, stdout = TRUE)
  
  #############################################################
  # Upload the image
  #############################################################
  fileArg <- paste("-f", filename, sep = " ")
  #up_args <- c("-H upload.twitter.com", "/1.1/media/upload.json?media_category=dm_image", fileArg, "-F media", "-X POST", "-t")
  up_args <- c("-H upload.twitter.com", "/1.1/media/upload.json?media_category=dm_image", fileArg, "-F media", "-X POST", "-t")
  upload_response <- system2(command = "twurl", args = up_args, stdout = TRUE)
  up_list <- jsonlite::fromJSON(upload_response)
  #############################################################
  # Extract Media Id
  #############################################################
  # Success?
  # Get Media id
  
  #############################################################
  # Send the Direct Message
  #############################################################
  recipArg <- paste('"',sender_id,'"',sep = "")
  mediaArg <- paste('"',media_id,'"',sep = "")
  #twurl -A 'Content-type: application/json' -X POST /1.1/direct_messages/events/new.json -d '{"event": {"type": "message_create", "message_create": {"target": {"recipient_id": "4534871"}, "message_data": {"text": "Hello World!"}}}}'
  #payload <- '{"event": {"type": "message_create", "message_create": {"target": {"recipient_id": "846168259355463680"}, "message_data": {"text": "Hello World!"}}}}'
  payload <- paste('{"event": {"type": "message_create", "message_create": {"target": {"recipient_id": ', recipArg, '}, "message_data": {"text": "picture post", "attachment": {"type": "media","media": {"id": ', mediaArg, '}}}}}}', sep = "")
  payload <- paste("'",payload,"'", sep = "")
  dm_args <- c("-A", "'Content-type: application/json'", "-X POST", "/1.1/direct_messages/events/new.json -d", payload, "-t")
  system2(command = "twurl", args = dm_args)
}

#########################################################
# Libraries
#########################################################
#Libraries
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

# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("/Users/Tim/code/Hap.py-Rob.ot/R/twitter-util.R")
# check to see if the plot-util.R script is loaded
if(!exists("plotutilversion", mode="function")) source("/Users/Tim/code/Hap.py-Rob.ot/R/plot-util.R")
#source("~/code/Hap.py-Rob.ot/R/plot-util.R")


#########################################################
# Read in messages to reply to
#########################################################


main <- function(){
  #Read in todays Trends from database
  db <- dbConnect(SQLite(), dbname=db_name)
  table_exits <- dbExistsTable(conn = db,name = "messages")
  dbDisconnect(db)
  if(!table_exits){
    stop()
  }
  db <- dbConnect(SQLite(), dbname=db_name)
  messages <- dbReadTable(conn = db, name = "messages")
  messages2 <- dbGetQuery(conn = db,"Select * from messages where created = 0")
  dbDisconnect(db)
  
  
  #########################################################
  # Iterate through the messages
  #########################################################
  results <- messages %>%
    dplyr::rowwise() %>%
    dplyr::mutate(created=processMessageRequest(text,sender_id,sender_screen_name, id))
  
  
  
  #########################################################
  # Delete Messages that we have sent
  #########################################################
  db <- dbConnect(SQLite(), dbname=db_name)
  dbExecute(conn = db,"Delete from messages where created = 1")
  dbDisconnect(db)
  
  return(results)
}





#########################################################
# Sentiment Plots for the infographic
#########################################################



#########################################################
# Positive / Negative Donut Chart
#########################################################
#########################################################
# Document Creation
#########################################################

endResult <- main()


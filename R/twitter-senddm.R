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


processMessageRequest <- function(message, sender_id, sender_sn, message_id){
  print("In processMessageRequest")
  result <- 0
  print(message)
  mchar <- nchar(message)
  #prepare message
  query <- as.character(dmPrepMessageForQuery(message))
  qchar <- nchar(query)
  if (mchar > 50) {
    result <- -2
  } else if(qchar == 0 & mchar > 0){
    result <- -3
  } else {
    result <- -1
  }
  print(query)
  print(nchar(query))
  #print(sender_id)
  filename <- paste(twPath(),"/images/", sender_id,".png",sep = "")
  #print(filename)
  
  #Set the row to -1 to indicate that we are working on it.
  db <- dbConnect(SQLite(), dbname=db_name)
  dbExecute(conn = db,"Update messages set created = :res where id = :id", param = list(id=message_id, res=result))
  dbDisconnect(db)
  
  if (qchar > 1 & result >= -1){
    #print("len is good ... go")
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
    
    #Send the Direct message
    sdmResult <- sendDM(sender_id = sender_id, filename = filename)
    if(exists("sdmResult") & sdmResult > 0){
      ddmResult <- destroyDM(message_id = message_id)
      if(exists("ddmResult") & ddmResult > 0){
        result <- ddmResult
      } else {
        result <- 0
      }
    }
  }
  print("Completing  processMessageRequest")
  return(result)
}

createInfoGraphic <- function(query = NULL, filename = NULL, sender = NULL){
  print("In createInfoGraphic")
  #filename <- "timtest.png"
  #query <- "#osisoftuc"
  #sender <- "TimTest"
  print(query)
  #print(filename)
  result <- 0
  #print(query)
  
  #########################################################
  # Twitter Extracts
  #########################################################
  #Get a token
  tokn <- twGetRtweetToken()
  tweets  <- rtweet::search_tweets(query, n = number_of_tweets, parse = TRUE,include_rts = FALSE,retryonratelimit = FALSE, lang = "en", token = tokn)
  print(class(tweets))
  if (class(tweets) == "list") {
    #Something came up with the tweets ... most likely different lengths of vectors
    tweetList <- lapply(tweets, function(x) { length(x) <- max(lengths(tweets)); x})
    tweets <-as.data.frame(tweetList)
  }
  tweetCount <- nrow(tweets)
  #print(tweetCount)
  #print(exists("tweets"))

  if(tweetCount > 1){
    
    #print("Tweets Received .. processing")
    #print(exists("tweets"))
    #print((tweets))
    #########################################################
    # Sentiment Analysis of the Tweets
    #########################################################
    tweetList <- rtTweetsToSentiment(tweetDataFrame = tweets)
    #print("2")
    emotionTotals <- as.data.frame(tweetList$emotionTotals)
    tweetPositivity <- as.data.frame(tweetList$positivityTotals)
    #print("3")
    emotionPlot <- pltEmotion(emotionTotals)
    donutPlot <- pltDonut(tweetPositivity)
    #############################################################
    # Process tweets for word cloud
    #############################################################
    # get the tweet text
    #print("4")
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

sendDM <- function(sender_id = "846168259355463680", filename = "/Users/tim/data/images/846168259355463681.png"){
  print("In sendDM")

  #if(dbug){
  #  sender_id <- "846168259355463680"
  #  filename <- "/Users/tim/data/images/846168259355463681.png"
  #}
  
  #keys <- twurlkeys()
  #argv <- c("/1.1/direct_messages.json", authArgs)
  #argtimelin <-  c("/1.1/statuses/home_timeline.json")
  #timeline <- system2(command = "twurl", args = argtimelin, stdout = TRUE)
  #dms <- system2(command = "twurl", args = argv, stdout = TRUE)
  #timelist <- jsonlite::fromJSON(timeline)
  system2(command = "pwd")
  #############################################################
  # Upload the image
  #############################################################
  fileArg <- paste("-f", filename, sep = " ")
  #up_args <- c("-H upload.twitter.com", "/1.1/media/upload.json?media_category=dm_image", fileArg, "-F media", "-X POST", "-t")
  up_args <- c("-H upload.twitter.com", "/1.1/media/upload.json?media_category=dm_image", fileArg, "-F media", "-X POST")
  print("Starting TWURL upload")
  upload_response <- system2(command = "/usr/local/bin/twurl", args = up_args, stdout = TRUE)
  print("Upload Complete")
  if(exists("upload_response")){
    #############################################################
    # Extract Media Id
    #############################################################
    up_list <- jsonlite::fromJSON(upload_response)
    media_id <- up_list$media_id_string
    print(paste("media id: ", media_id, sep = ""))
    #############################################################
    # Send the Direct Message
    #############################################################
    recipArg <- paste('"',sender_id,'"',sep = "")
    mediaArg <- paste('"',media_id,'"',sep = "")
    message_text <- "Here is your infographic from @Hap_py_Rob_ot"
    messageArg <- paste('"',message_text,'"',sep = "")
    #twurl -A 'Content-type: application/json' -X POST /1.1/direct_messages/events/new.json -d '{"event": {"type": "message_create", "message_create": {"target": {"recipient_id": "4534871"}, "message_data": {"text": "Hello World!"}}}}'
    #payload <- '{"event": {"type": "message_create", "message_create": {"target": {"recipient_id": "846168259355463680"}, "message_data": {"text": "Hello World!"}}}}'
    payload <- paste('{"event": {"type": "message_create", "message_create": {"target": {"recipient_id": ', recipArg, '}, "message_data": {"text": ', messageArg, ', "attachment": {"type": "media","media": {"id": ', mediaArg, '}}}}}}', sep = "")
    payload <- paste("'",payload,"'", sep = "")
    dm_args <- c("-A", "'Content-type: application/json'", "-X POST", "/1.1/direct_messages/events/new.json -d", payload)
    dm_response <- system2(command = "/usr/local/bin/twurl", args = dm_args, stdout = TRUE)
    
    if(exists("dm_response")){
      print("Completing sendDM -1")
      return(1)
    } else {
      print("Completing sendDM -2")
      return(0)
    }
  } else {
    print("Completing sendDM -3")
    return(0)
  }
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
  #Read in todays Trends from database
  db <- dbConnect(SQLite(), dbname=db_name)
  table_exits <- dbExistsTable(conn = db,name = "messages")
  dbDisconnect(db)
  if(!table_exits){
    stop()
  }
  db <- dbConnect(SQLite(), dbname=db_name)
  #messages <- dbReadTable(conn = db, name = "messages")
  messages <- dbGetQuery(conn = db,"Select * from messages where created = 0")
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


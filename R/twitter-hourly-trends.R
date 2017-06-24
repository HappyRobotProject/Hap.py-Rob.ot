#
#
#
setwd("~/data")
#Libraries
library(twitteR)
library(plyr)

# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("~/code/Hap.py-Rob.ot/R/twitter-util.R")
#source("~/code/Hap.py-Rob.ot/R/twitter-util.R")

keys <- twkeys()
token <- twitteR::setup_twitter_oauth(keys$consumer_key,keys$consumer_secret,keys$access_token,keys$access_secret)

#Get current trends

#United States / United States = 23424977
trends <- twitteR::getTrends(23424977)

library(RSQLite)
library(sqldf)
db <- dbConnect(SQLite(), dbname="~/data/Test.sqlite")
dbWriteTable(conn = db, name = "trends", value = trends, row.names = FALSE, append = TRUE)
dbDisconnect(db)

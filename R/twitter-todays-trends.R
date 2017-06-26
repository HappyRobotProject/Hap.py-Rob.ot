#
#
#
#########################################################
# Script Setup
#########################################################
setwd("/Users/tim/data")

#########################################################
# Libraries
#########################################################
#Libraries
library(twitteR)
library(plyr)
library(dplyr)
library(RSQLite)
library(RColorBrewer)


# check to see if the twitter-util.R script is loaded
if(!exists("twitterutilversion", mode="function")) source("~/code/Hap.py-Rob.ot/R/twitter-util.R")
#source("~/code/Hap.py-Rob.ot/R/twitter-util.R")

#Read in todays Trends from database
db <- dbConnect(SQLite(), dbname="~/data/Test.sqlite")
todaysTrends <- dbReadTable(conn = db, name = "trends")
#dbRemoveTable(conn = db, name = trends)
dbDisconnect(db)

#########################################################
# Get the Query for the Most Popular trend
#########################################################
queryCount <- as.data.frame(table(unlist(todaysTrends$query)))
queryCount <- arrange(queryCount,desc(Freq))
#Get the most popular trend
query <- as.character(queryCount$Var1[1])

#########################################################
# Get the Top 5 Popular trends
#########################################################
tagCount <- as.data.frame(table(unlist(todaysTrends$name)))
tagCount <- arrange(tagCount,desc(Freq))
#Get the most popular trend
topTags <- as.character(tagCount$Var1[1:5])

#########################################################
# Get the Tweets from the most popular trend
#########################################################
keys <- twkeys()
token <- twitteR::setup_twitter_oauth(keys$consumer_key,keys$consumer_secret,keys$access_token,keys$access_secret)

tweetList <- twitteR::searchTwitter(query, n=500)

tweets <- twitteR::twListToDF(tweetList)
noun <- twRemoveUserNames(tweets$text)
noht <- twRemoveHashtags(tweets$text)
un <- twExtractUserNames(tweets$text)
ht <- twExtractHashtags(tweets$text)

undf <- plyr::ldply(un, rbind)

occurences <- table(unlist(un))
querydf <- as.data.frame(occurences)
querydf <- arrange(querydf,desc(Freq))


#########################################################
# Sentiment Analysis of the Top Trend
#########################################################
sentimentTweets <- twPrepTweetsForSentiment(tweets$text)

#library(stringr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
mySentiment <- get_nrc_sentiment(sentimentTweets)  
sentimentTweets <- cbind(sentimentTweets, mySentiment)

sentimentTotals <- data.frame(colSums(sentimentTweets[,c(2:11)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
#par(bg = med1, fig=c(0.25,0.75,0.6,0.9))
p1 <- ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

library(tidyr)
max <- max(sentimentTotals$count)
sentimentTotals$percent = (sentimentTotals$count / max )* 4 + 3
rad <- select(sentimentTotals,sentiment, percent)
tran <- spread(rad,key=sentiment, value=percent)
tran <- cbind(group = "Sentiment", tran)
p2 <- ggradar(tran, grid.max = 7, grid.min = 0, centre.y = 0, plot.legend = FALSE, font.radar = "Arial", axis.label.size = 3, group.line.width = 1.5, group.point.size = 3)
p2
#########################################################
# Word Cloud
#########################################################
tweettext <- twPrepTweetsForCloud(tweets$text)


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#load the text
#The text is loaded using Corpus() function from text mining ™ package. Corpus is a list of a document (in our case, we only have one document).
#To import the file saved locally in your computer, type the following R code. You will be asked to choose the text file interactively.
#text <- readLines(file.choose())

# Read the text file from internet
# Load the data as a corpus
docs <- Corpus(VectorSource(tweettext))
#VectorSource() function creates a corpus of character vectors

#Transformation is performed using tm_map() function to replace, for example, special characters from the text.
#Replacing “/”, “@” and “|” with space:

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


#the tm_map() function is used to remove unnecessary white space, to convert the text to lower case, to remove common stopwords like ‘the’, “we”.
#The information value of ‘stopwords’ is near zero due to the fact that they are so common in a language. Removing this kind of words is useful before further analyses. For ‘stopwords’, supported languages are danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish and swedish. Language names are case sensitive.
#I’ll also show you how to make your own list of stopwords to remove from the text.
#You could also remove numbers and punctuation with removeNumbers and removePunctuation arguments.
#Another important preprocessing step is to make a text stemming which reduces words to their root form. In other words, this process removes suffixes from words to make it simple and to get the common origin. For example, a stemming process reduces the words “moving”, “moved” and “movement” to the root word, “move”.
#Note that, text stemming require the package ‘SnowballC’.

#The R code below can be used to clean your text :

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("french"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("RT", "twitter", "https","amp","tco")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
#--> docs <- tm_map(docs, stemDocument)


#Step 4 : Build a term-document matrix
#Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. The function TermDocumentMatrix() from text mining package can be used as follow :

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#head(d, 10)

#Error Handling
#Every time you are creating plots you might get this error - "Error in plot.new() : figure margins too large". To avoid such errors you can first check par("mar") output. You should be getting:
#To change that write:

par(mar=c(1,1,1,1))
#This should rectify the error. Or else you can change the values accordingly.

#Step 5 : Generate the Word cloud
#The importance of words can be illustrated as a word cloud as follow :

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))





#########################################################
# Document Creation
#########################################################

library(grid)
library(extrafont)
library(useful)
#font_import()
loadfonts()
#fonts()

#Colors
background <- "#2d3142"
white <- "#ffffff"

dark1 <- "#7084a5"
med1 <- "#A1CEE0"
dark2 <- "#ef8354"
med2 <- "#C52D41"
highlight <- "#bfc0c0"
accent <- dark2
main <-dark1
#print(p1, vp = vp3)
#print(p1, vp = vp4)


png("~/data/images/daily-trends.png", width = 4, height = 3, units = "in", res = 500)
# Plot the word cloud (will force a new page)
par(bg = background, fig=c(0.1,0.9,0.1,0.9), mar=c(0,0,0,0))
wordcloud(words = d$word, freq = d$freq, scale = c(2,0.25), min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=c(accent, main, highlight))
vp1 <- viewport(x = 0, y = 0.85, w = 1.0, h = 0.15, just = c("left", "bottom"), name = "vp1")
pushViewport(vp1)
grid.rect(gp = gpar(fill = background, col = background))
grid.text("#DailyTwitterTrend", vjust = 0, y = unit(0.1, "npc"), gp = gpar(fontfamily = "Impact", col = main, cex = 0.8))
upViewport()
vp2 <- viewport(x = 0.77, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp2")
pushViewport(vp2)
grid.rect(gp = gpar(fill = dark2, col = dark2))
grid.text("Top 5 Trends", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = "Impact", col = med1, cex = 0.7))
grid.text(paste(
  topTags[1],
  topTags[2],
  topTags[3],
  topTags[4],
  topTags[5], sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.5, "npc"), gp = gpar( col = main, cex = 0.5))
upViewport()
vp3 <- viewport(x = 0.23, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp3")
grid.text("Sentiment Analysis", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = "Impact", col = main, cex = 0.7))
pushViewport(vp3)

dev.off()



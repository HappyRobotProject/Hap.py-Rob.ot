#
#
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
    panel.grid.minor.y = element_blank(),
  )
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

#Read in todays Trends from database
db <- dbConnect(SQLite(), dbname=db_name)
table_exits <- dbExistsTable(conn = db,name = "trends")
dbDisconnect(db)
if(!table_exits){
  stop()
}
db <- dbConnect(SQLite(), dbname=db_name)
todaysTrends <- dbReadTable(conn = db, name = "trends")
dbDisconnect(db)

#########################################################
# Get the Query for the Most Popular trend
#########################################################
queryCount <- as.data.frame(table(unlist(todaysTrends$query)))
queryCount <- arrange(queryCount,desc(Freq))
#Get the most popular trend
query <- as.character(queryCount$Var1[1])
queryTerm <- query

filtered_trends <- dplyr::filter(todaysTrends, query == queryTerm)
trendName <- filtered_trends$name[1]

write.csv(data.frame(trend=trendName),"todays-trend.csv")

db <- dbConnect(SQLite(), dbname=db_name)
dbWriteTable(conn = db,"todays_trends", data.frame(trend = trendName, date = as.character(Sys.Date())), append = TRUE)
dbDisconnect(db)


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

tweetList <- twitteR::searchTwitter(query, n=5000)

tweets <- twitteR::twListToDF(tweetList)
authors <- tweets$screenName
mentions <- as.data.frame(table(unlist(twExtractUserNames(tweets$text))))
mentions <- arrange(mentions, desc(Freq))[1:10,]
authors <- as.data.frame(table(unlist(tweets$screenName)))
authors <- arrange(authors, desc(Freq))[1:10,]

#Check to see if we follow these people all ready
dbConnections <- dbConnect(SQLite(), dbname=db_connections)

findScreenName <- function(screen_name){
  results <- dbGetQuery(conn = dbConnections,"Select * from twitter_connections where screenname = ?", str_replace_all(stri_trans_tolower(screen_name),"@",""))
  return(nrow(results)>0)
}

follow <- mentions %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sn_exists = findScreenName(Var1)) %>%
  dplyr::filter(sn_exists == FALSE) %>%
  top_n(1,Freq) %>%
  select(screen_name = Var1)
follow$screen_name <- as.character(follow$screen_name)
follow <- follow[1,]
follow$screen_name <- str_replace_all(stri_trans_tolower(follow$screen_name),"@","")

dbWriteTable(conn = dbConnections,"twitter_to_follow", follow, append = TRUE)

follow <- authors %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sn_exists = findScreenName(Var1)) %>%
  dplyr::filter(sn_exists == FALSE) %>%
  top_n(1,Freq) %>%
  select(screen_name = Var1)
follow$screen_name <- as.character(follow$screen_name)
follow <- follow[1,]
follow$screen_name <- str_replace_all(stri_trans_tolower(follow$screen_name),"@","")

dbWriteTable(conn = dbConnections,"twitter_to_follow", follow, append = TRUE)
dbDisconnect(dbConnections)


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
# Split out the subsets
emotionTotals <- sentimentTotals[1:8,]
emotionMax <- max(emotionTotals$count)
positiveNegative <- sentimentTotals[9:10,]

#########################################################
# Sentiment Plots for the infographic
#########################################################


emotionTotals <- dplyr::bind_cols(emotionTotals,data.frame("posneg"=c(-1,1,-1,-1,1,-1,1,1)))
emotionTotals$value <- (emotionTotals$count * emotionTotals$posneg / emotionMax * 100)
#emotionTotals$percent1 <- emotionTotals$percent * emotionTotals$posneg
emotionTotals$sentiment <- stri_trans_totitle(emotionTotals$sentiment)
emotionTotals$pltcolour <- ifelse(emotionTotals$value < 0, info.accent, info.main)
emotionTotals$hjust <- ifelse(emotionTotals$value > 0, 1.1, -0.1)
emotionTotals <- arrange(emotionTotals, value)
emotionTotals <- transform(emotionTotals, sentiment = reorder(sentiment, value))
plotcolors=c(info.accent,info.accent,info.accent,info.accent,info.main,info.main,info.main,info.main)

emotionGraph <-ggplot(emotionTotals, aes(sentiment, value, label = sentiment, hjust = hjust)) + 
  #geom_bar(stat = "identity", aes(fill = colour))
  geom_text(aes(y = 0, colour = sentiment, family=info.font),size=1.75, color=plotcolors) + 
  geom_bar(stat = "identity", aes(fill = sentiment), width = 0.5, color=plotcolors, fill=plotcolors) +
  coord_flip() + labs(x = "", y = "") +
  scale_x_discrete(breaks = NULL) + 
  theme(legend.position = "none") + scale_y_continuous(limits=c(-110, 110)) + #scale_x_continuous(expand = c(0,0)) + 
  bar_theme()
emotionGraph


#########################################################
# Positive / Negative Donut Chart
#########################################################

# Create test data.
donutData <- positiveNegative # = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))

# Add addition columns, needed for drawing with geom_rect.
donutData$fraction = donutData$count / sum(donutData$count)
#donutData = donutData[order(donutData$sentiment), ]
donutData <- dplyr::arrange(donutData,desc(sentiment))
donutData <- transform(donutData, sentiment = reorder(sentiment, count))
donutData$ymax = cumsum(donutData$fraction) +0.0
donutData$ymin = c(0, head(donutData$ymax, n=-1)) +0.0
donutData$ymid = ((donutData$ymax - donutData$ymin)/2) + donutData$ymin



# Make the plot
donutPlot = ggplot(donutData, aes(fill=sentiment, ymax=ymax, ymin=ymin, xmax=4, xmin=2.5)) +
#donutPlot <- ggplot(donutData, aes(fill=sentiment, ymax=ymax, ymin=ymin)) + #, xmax=4, xmin=2.5)) +
  #scale_x_continuous(limits = c(0, 4), expand = c(0,0)) +  #scale_y_continuous(limits = c(ymin, ymax), expand = c(0,0)) +
  geom_rect(fill=c(info.main,info.accent), xmax=4, xmin=2.5) +
  #scale_fill_manual(name="Overall Sentiment", values = c(accent, main), label=c("Negative","Positive")) +  
  geom_text( aes(label = paste(round(fraction * 100, digits = 0),"%",sep=""), y=donutData$ymid, x = 3.25), size=1.75, fontface="bold", color=info.background)+
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(#legend.text=element_text(size=3, family="Arial", color = highlight),
        #legend.position = c(0.5,0.5),
        #legend.box.margin = c(0,0,0,0),
        #legend.key.size = unit(c(0.05,0.05),"npc"),
        #legend.background = element_rect(fill = background, color = background),
        #legend.key = element_rect(color=background, fill = background),
        #legend.title = element_text(size=4, family="Arial", color = highlight),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())+
  #ggplot2::annotate("text", x = 0, y = 0, label = "plus minus !", color=highlight) +
  labs(title="")
#par(mar=c(0,0,0,0))
donutPlot
donutPlot <- donutPlot + basic_theme()


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

#fonts()


#print(p1, vp = vp3)
#print(p1, vp = vp4)


png("/Users/Tim/data/images/daily-trends.png", width = 4, height = 3, units = "in", res = 500)
# Plot the word cloud (will force a new page)
par(bg = info.background, fig=c(0.1,0.9,0.1,0.9), mar=c(0,0,0,0))
wordcloud(words = d$word, freq = d$freq, scale = c(2,0.25), min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, family = info.font, colors=c(info.main, info.main.light, info.accent))
vp1 <- viewport(x = 0, y = 0.85, w = 1.0, h = 0.15, just = c("left", "bottom"), name = "vp1")
pushViewport(vp1)
grid.rect(gp = gpar(fill = info.background, col = info.background))
grid.text("#DailyTwitterTrend", vjust = 0, y = unit(0.5, "npc"), gp = gpar(fontfamily = info.font, col = info.highlight, cex = 1.6))
grid.text(paste("The top trend for", format(Sys.Date(), format="%B %d, %Y"), "is", trendName,sep = " "), vjust = 0, y = unit(0.1, "npc"), gp = gpar(fontfamily = info.font, col = info.highlight, cex = 0.8))
upViewport()

#Right Pane
vp2 <- viewport(x = 0.77, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp2")
vpEmotion <- viewport(x = 0.0, y = 0.85, w = 1.0, h = 0.5, just = c("left", "top"))
pushViewport(vp2)
print(emotionGraph, vp=vpEmotion)
grid.text("Tweet Emotions", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = info.font, col = info.highlight, cex = 0.65))
grid.text(paste(
  "Infographic",
  "created by",
  "@Hap_py_Rob_ot",
  "https://git.io/vQWQn",
  sep = "\n"), vjust = 0, hjust = 0, x = unit(0.02, "npc"), y = unit(0.05, "npc"), gp = gpar(fontfamily = info.font, col = info.main, cex = 0.5))

upViewport()

#Left Pane
vp3 <- viewport(x = 0.0, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp3")
vpDonut <- viewport(x = 0.5, y = 0.75, w = 1.0, h = 1.0)
pushViewport(vp3)
#grid.rect(gp = gpar(fill = info.background, col = info.accent.light))
print(donutPlot, vp=vpDonut)
grid.text("Sentiment Analysis", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = info.font, col = info.highlight, cex = 0.65))
grid.text("Positive Sentiment", vjust = 0, y = unit(0.53, "npc"), gp = gpar(fontfamily = info.font, col = info.main, cex = 0.4))
grid.text("Negative Sentiment", vjust = 0, y = unit(0.5, "npc"), gp = gpar(fontfamily = info.font, col = info.accent, cex = 0.4))
#grid.rect(gp = gpar(fill = info.background, col = info.background))
grid.text("Top 5 Trends", vjust = 0, y = unit(0.35, "npc"), gp = gpar(fontfamily = info.font, col = info.highlight, cex = 0.65))
grid.text(paste(
  topTags[1],
  topTags[2],
  topTags[3],
  topTags[4],
  topTags[5], sep = "\n"), vjust = 0, hjust = 0, x = unit(0.02, "npc"), y = unit(0.1, "npc"), gp = gpar(fontfamily = info.font, col = info.highlight, cex = 0.5))
dev.off()

#
#Delete the trends
db <- dbConnect(SQLite(), dbname=db_name)
dbRemoveTable(conn = db, name =  "trends")
dbDisconnect(db)


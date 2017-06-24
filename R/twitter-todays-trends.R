#
#
#
#########################################################
# Script Setup
#########################################################
setwd("~/data")

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
dark1 <- "#1A0641"
med1 <- "#A1CEE0"
dark2 <- "#FF0712"
med2 <- "#C52D41"
highlight <- "#D2EDF1"

pdf("~/Documents/R/DailyTrump.pdf", width = 10, height = 20)
#Printout the Wordcloud
par(bg = highlight, fig=c(0.25,0.75,0.6,0.9))
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "RdBu"))

#grid.newpage() 
#Create viewport for the title
vp1 <- viewport(x = 0, y = 0.9, w = 1.0, h = 0.1, just = c("left", "bottom"), name = "vp1")
vp2 <- viewport(x = 0.0, y = 0.8, w = 0.25, h = 0.1, just = c("left", "bottom"))
vp3 <- viewport(x = 0.0, y = 0.0, w = 1.0, h = 0.3, just = c("left", "bottom"))
vp4 <- viewport(x = 0.0, y = 0.0, w = 1.0, h = 1.0, just = c("left", "bottom"))

#pushViewport(viewport(layout = grid.layout(4, 3)))
pushViewport(vp1)
grid.rect(gp = gpar(fill = dark1, col = dark1))
#grid.rect(gp = gpar(fill = dark1, col = dark1), x = unit(0.5, "npc"), y = unit(0.95, "npc"), width = unit(1, "npc"), height = unit(0.10, "npc"))
grid.text("#TWITTER", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = highlight, cex = 12, alpha = 0.2))
grid.text("Daily Trump", y = unit(0.4, "npc"), gp = gpar(fontfamily = "Impact", col = med2, cex = 6.4))

grid.text("@Paying_Atention", vjust = 0, y = unit(0.1, "npc"), gp = gpar(fontfamily = "Impact", col = med1, cex = 0.8))
grid.text("Twitter Sentiment Analysis", vjust = 0, y = unit(0.05, "npc"), gp = gpar(fontfamily = "Impact", col = med1, cex = 0.8))
grid.text("#trump @realDonaldTrump", vjust = 0, y = unit(0.0, "npc"), gp = gpar(fontfamily = "Impact", col = med1, cex = 0.8))
upViewport()
pushViewport(vp2)
grid.rect(gp = gpar(fill = dark2, col = dark2))
grid.text("Tweets", vjust = 0, y = unit(0.8, "npc"), gp = gpar(fontfamily = "Impact", col = highlight, cex = 3))
grid.text(nr, vjust = 0, y = unit(0.1, "npc"), gp = gpar(fontfamily = "Impact", col = highlight, cex = 8))
upViewport()
#pushViewport(vp3)
#Sentiment Chart
#grid.rect(gp = gpar(fill = dark1, col = dark1))
#print(p1)
print(p1, vp = vp3)
#print(p1, vp = vp4)


#print(p1, vp = vplayout(3, 1:3))
#print(p2, vp = vplayout(2, 1:3))
#grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
#grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar( col = "#CA8B01", cex = 13, alpha = 0.3))
#grid.text("A VERY VERY VERY VERY LONG TITLE", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar( col = "#552683", cex = 1.2))
#grid.text("DATA INFO", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(col = "white", cex = 1.2))
#grid.text(paste(
#  "Syndicated to",
#  "Source",
#  "Author",
#  "Maintainer",
#  "Frequency of Update",
#  "Granularity",
#  "Temporal Date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar( col = "#552683", cex = 0.8))
#grid.text(paste(
#  "http://alstatr.blogspot.com",
#  "http://alstatr.blogspot.com",
#  "Analysis with Programming",
#  "Al-Ahmadgaid B. Asaad",
#  "Annually",
#  "National",
#  "2011-2013", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(col = "#552683", cex = 0.8))
dev.off()


png("~/data/images/daily-trends.png", width = 4, height = 3, units = "in", res = 500)
# Plot the word cloud (will force a new page)
par(bg = highlight, fig=c(0.1,0.9,0.1,0.9), mar=c(0,0,0,0))
wordcloud(words = d$word, freq = d$freq, scale = c(2,0.25), min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "RdBu"))
vp1 <- viewport(x = 0, y = 0.85, w = 1.0, h = 0.15, just = c("left", "bottom"), name = "vp1")
pushViewport(vp1)
grid.rect(gp = gpar(fill = dark1, col = dark1))
grid.text("Daily Twitter Trends", vjust = 0, y = unit(0.1, "npc"), gp = gpar(fontfamily = "Impact", col = med1, cex = 0.8))
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
  topTags[5], sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.5, "npc"), gp = gpar( col = highlight, cex = 0.5))
upViewport()
vp3 <- viewport(x = 0.23, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp3")
grid.text("Sentiment Analysis", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = "Impact", col = med1, cex = 0.7))
pushViewport(vp3)

dev.off()



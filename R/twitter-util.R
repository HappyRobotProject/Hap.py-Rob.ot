#
# Twitter Helper Functions
#
setwd("/Users/tim/data")
# Load libraries
library(stringr)


#Simple function to allow another script to check if this is loaded
twitterutilversion <- function(){
  return("0.0.1")
}

twDBug <- TRUE

twPath <- function(){
  return("/Users/tim/data")
}


# Extract authentication keys from a csv file
twkeys <- function(){
  fileName <- paste(twPath(),"/auth/twitter-HRP.csv", sep = "")
  auth <-read.csv(file = fileName, header = TRUE, stringsAsFactors = FALSE)
  ck <- auth$consumer.key
  cs <- auth$consumer.secret
  at <- auth$access.token
  as <- auth$access.secret
  an <- auth$appname
  return (list(consumer_key=ck,consumer_secret=cs,access_token=at,access_secret=as,app_name=an))
}


twSetRtweetToken <- function(token, file_name = "/auth/rtweet-token-HRP.rds"){
  fileName <- paste(twPath(),file_name, sep = "")
  ## save token
  saveRDS(token, file = fileName)
}
twGetRtweetToken <- function(file_name = "/auth/rtweet-token-HRP.rds"){
  fileName <- paste(twPath(),file_name, sep = "")
  ## read token
  token <- readRDS(file = fileName)
}

twRemoveUserNames <- function(tweets){
  #Check for char array?
  results <- str_replace_all(tweets,"@\\w+", "")
  return(results)
}

twExtractUserNames <- function(tweets){
  #Check for char array?
  results <- str_extract_all(tweets,"@\\w+")
  return(results)
}

twRemoveHashtags <- function(tweets){
  #Check for char array?
  results <- str_replace_all(tweets,"#\\w+", "")
  return(results)
}

twExtractHashtags <- function(tweets){
  #Check for char array?
  results <- str_extract_all(tweets,"#\\w+")
  return(results)
}

twRemoveLinks <- function(tweets){
  #Check for char array?
  #results <- str_replace_all(tweets,"http://\\w+", "")
  #results <- str_replace_all(results,"https://\\w+", "")
  results <- str_replace_all(tweets," ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "")
  #results <- str_replace_all(tweets," ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")
  return(results)
}

twPrepTweetsForCloud <- function(tweets){
  results <- twRemoveHashtags(tweets)
  results <- twRemoveUserNames(results)
  results <- twRemoveLinks(results)
  #remove remaining non-printing characters
  results <- str_replace_all(results, "[^[:alnum:]///' ]", "")
  
  return(results)
}

twPrepTweetsForSentiment <- function(tweets){
  results <- twRemoveHashtags(tweets)
  results <- twRemoveUserNames(results)
  results <- twRemoveLinks(results)
  #remove remaining non-printing characters
  results <- str_replace_all(results, "[^[:alnum:]///' ]", "")
  return(results)
}



twTweetToDocMatrix <- function(tweetText){
  library("tm")
  library("SnowballC")
  #load the text
  #The text is loaded using Corpus() function from text mining ™ package. Corpus is a list of a document (in our case, we only have one document).
  #To import the file saved locally in your computer, type the following R code. You will be asked to choose the text file interactively.
  #text <- readLines(file.choose())
  # Load the data as a corpus
  docCorpus <- Corpus(VectorSource(tweetText))
  #VectorSource() function creates a corpus of character vectors
  
  #Transformation is performed using tm_map() function to replace, for example, special characters from the text.
  #Replacing “/”, “@” and “|” with space:
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docCorpus <- tm_map(docCorpus, toSpace, "/")
  docCorpus <- tm_map(docCorpus, toSpace, "@")
  docCorpus <- tm_map(docCorpus, toSpace, "\\|")
  
  
  #the tm_map() function is used to remove unnecessary white space, to convert the text to lower case, to remove common stopwords like ‘the’, “we”.
  #The information value of ‘stopwords’ is near zero due to the fact that they are so common in a language. Removing this kind of words is useful before further analyses. For ‘stopwords’, supported languages are danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish and swedish. Language names are case sensitive.
  #I’ll also show you how to make your own list of stopwords to remove from the text.
  #You could also remove numbers and punctuation with removeNumbers and removePunctuation arguments.
  #Another important preprocessing step is to make a text stemming which reduces words to their root form. In other words, this process removes suffixes from words to make it simple and to get the common origin. For example, a stemming process reduces the words “moving”, “moved” and “movement” to the root word, “move”.
  #Note that, text stemming require the package ‘SnowballC’.
  
  #The R code below can be used to clean your text :
  
  # Convert the text to lower case
  docCorpus <- tm_map(docCorpus, content_transformer(tolower))
  # Remove numbers
  docCorpus <- tm_map(docCorpus, removeNumbers)
  # Remove english common stopwords
  docCorpus <- tm_map(docCorpus, removeWords, stopwords("english"))
  docCorpus <- tm_map(docCorpus, removeWords, stopwords("french"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docCorpus <- tm_map(docCorpus, removeWords, c("#Biogen", "RT", "https","inc","amp","tco","biogen","biib","ibb", "trump", "potus")) 
  # Remove punctuations
  docCorpus <- tm_map(docCorpus, removePunctuation)
  # Eliminate extra white spaces
  docCorpus <- tm_map(docCorpus, stripWhitespace)
  
  # Text stemming
  #--> docs <- tm_map(docs, stemDocument)
  
  
  #Step 4 : Build a term-document matrix
  #Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. The function TermDocumentMatrix() from text mining package can be used as follow :
  
  dtm <- TermDocumentMatrix(docCorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  docDf <- data.frame(word = names(v),freq=v)
  return(docDf)
}


twTweetsToSentiment <- function(tweetDataFrame){
  library(syuzhet)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(dplyr )
  
  #testing
  if(twDBug) print("In twTweetsToSentiment")
  tweetDataFrame$text <- twPrepTweetsForSentiment(tweetDataFrame$text)
  if(twDBug) print("In twTweetsToSentiment 1")
  sentiment <- get_nrc_sentiment(tweetDataFrame$text)  
  tweetDataFrame <- cbind(tweetDataFrame, sentiment)
  sentimentTotals <- data.frame(colSums(tweetDataFrame[,c(17:26)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  # Split out the subsets
  emotionTotals <- sentimentTotals[1:8,]
  positiveNegative <- sentimentTotals[9:10,]
  #Biogen
  emotionMax <- max(emotionTotals$count)
  emotionTotals <- dplyr::bind_cols(emotionTotals,data.frame("posneg"=c(-1,1,-1,-1,1,-1,1,1)))
  emotionTotals$value <- (emotionTotals$count * emotionTotals$posneg / emotionMax * 100)
  #emotionTotals$percent1 <- emotionTotals$percent * emotionTotals$posneg
  emotionTotals$sentiment <- stri_trans_totitle(emotionTotals$sentiment)
  emotionTotals$hjust <- ifelse(emotionTotals$value > 0, 1.1, -0.1)
  emotionTotals <- arrange(emotionTotals, value)
  emotionTotals <- transform(emotionTotals, sentiment = reorder(sentiment, value))
  resp <- list(sentimentTweets = tweetDataFrame, emotionTotals = emotionTotals, positivityTotals = positiveNegative)
  return(resp)
}

rtTweetsToSentiment <- function(tweetDataFrame){
  library(syuzhet)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(dplyr )
  
  #testing
  #tweetDataFrame <- tweets
  if(twDBug) print("In twTweetsToSentiment")
  tweetDataFrame$text <- twPrepTweetsForSentiment(tweetDataFrame$text)
  
  sentiment <- get_nrc_sentiment(tweetDataFrame$text)  
  tweetDataFrame <- cbind(tweetDataFrame, sentiment)
  sentimentTotals <- data.frame(colSums(tweetDataFrame[,c(36:45)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  # Split out the subsets
  emotionTotals <- sentimentTotals[1:8,]
  positiveNegative <- sentimentTotals[9:10,]
  #Biogen
  emotionMax <- max(emotionTotals$count)
  emotionTotals <- dplyr::bind_cols(emotionTotals,data.frame("posneg"=c(-1,1,-1,-1,1,-1,1,1)))
  emotionTotals$value <- (emotionTotals$count * emotionTotals$posneg / emotionMax * 100)
  #emotionTotals$percent1 <- emotionTotals$percent * emotionTotals$posneg
  emotionTotals$sentiment <- stri_trans_totitle(emotionTotals$sentiment)
  emotionTotals$hjust <- ifelse(emotionTotals$value > 0, 1.1, -0.1)
  emotionTotals <- arrange(emotionTotals, value)
  emotionTotals <- transform(emotionTotals, sentiment = reorder(sentiment, value))
  resp <- list(sentimentTweets = tweetDataFrame, emotionTotals = emotionTotals, positivityTotals = positiveNegative)
  return(resp)
}


rtExtractTweets <- function(q = "", 
                            n = 3200,
                            re_tweets = FALSE,
                            sinceID = 0,
                            dbTable = "",
                            daysToKeep = 30,
                            token = NULL
){
  #Extract tweets
  #q <- "#Trump" 
  #n <- 1000
  #sinceID <- 0
  #re_tweets <- FALSE
  #dbTable <- "trump"
  #tok1 <- twGetRtweetToken(file_name = "PA_Analysis_Tok.rds")
  
  tweets <- rtweet::search_tweets(q = q, n = n, include_rts = re_tweets, since_id = sinceID, lang = "en")
  #tweetList <- searchTwitter("Fampyra", n = 3200, lang = 'en', sinceID = 0)
  #tweetList <- searchTwitter(q, n = n, lang = 'en', sinceID = sinceID)
  #if (length(tweetList) > 0){
  #  tweets <- twListToDF(tweetList)
  #  tweets$latitude <- as.numeric(tweets$latitude)
  #  tweets$longitude <- as.numeric(tweets$longitude)
  #}
  
  #Pull DB tweets
  #dbTable = "tecfideraTweets"
  if (sinceID > 0) {
    dbtweets <- dbReadTable(db, name = dbTable) 
    dbtweets$created_at <- ymd_hms(dbtweets$created_at)
    dbtweets$is_quote_status <- as.logical(dbtweets$is_quote_status)
    dbtweets$is_retweet <- as.logical(dbtweets$is_retweet)
    #dbtweets$isRetweet <- as.logical(dbtweets$)
    #dbtweets$retweeted <- as.logical(dbtweets$retweeted)
    #dbtweets$latitude <- as.numeric(dbtweets$latitude)
    #dbtweets$longitude <- as.numeric(dbtweets$longitude)
    #Merge tweets
    if(length(tweets) > 0) {
      dbtweets <- bind_rows(dbtweets, tweets)
      dbtweets <- dplyr::filter(dbtweets, created_at > (Sys.Date()-days(daysToKeep)))
    }
  } else {
    dbtweets <- tweets
  }
  #Store tweets
  dbtweets$created_at = as.character(dbtweets$created_at)
  dbWriteTable(conn = db, name = dbTable, value = dbtweets, row.names = FALSE, overwrite = TRUE)
  return(dbtweets)
}

dmPrepMessageForQuery <- function(message = NULL){
  q <- twRemoveLinks(message)
  return(as.character(q))
}

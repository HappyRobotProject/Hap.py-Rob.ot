#Development Tools
install.packages("devtools")
install.packages("useful")
#Data Prep
install.packages("plyr")
install.packages("readr")
#Graphics
install.packages("ggplot2")
install.packages("ggradar")
devtools::install_github("ricardo-bion/ggradar") # dev version
#Database Packages
install.packages("DBI")
install.packages("RSQLite")
install.packages("sqldf")
#Web and Web services
install.packages("curl")
install.packages("openssl")
install.packages("httr")
install.packages("ROAuth")
install.packages("httpuv")
#Twitter Packages
install.packages("rtweet")
install.packages("twitteR")
install.packages("graphTweets") # CRAN release
#devtools::install_github("JohnCoene/graphTweets") # dev version
#Text Mining
install.packages("stringr")
install.packages("tm")
#Sentiment Analysis
install.packages("rJava")
install.packages("syuzhet")
install.packages("lubridate")
#
install.packages("tidyverse")
install.packages("text2vec")
install.packages("caret")
install.packages("glmnet")
install.packages("ggrepel")
install.packages("xml2")
install.packages("purrrlyr")
#Word Clouds
install.packages("wordcloud")
install.packages("SnowballC")
#colors
install.packages("RColorBrewer")
#fonts/Users/tim/code/Hap.py-Rob.ot/R/RPackageInstall.R
install.packages("extrafont")
library(extrafont)
font_import() # Import all fonts
fonts() # Print list of all fonts
install.packages("jsonlite")




#Development Tools
library("devtools")
library("useful")
#Data Prep
library("plyr")
#Database Packages
library("DBI")
library("RSQLite")
library("sqldf")
#Web and Web services
library("curl")
library("openssl")
library("httr")
library("ROAuth")
library("httpuv")
#Twitter Packages
library("rtweet")
library("twitteR")
library("graphTweets") # CRAN release
#devtools::install_github("JohnCoene/graphTweets") # dev version
#Text Mining
library("stringr")
library("tm")
#Sentiment Analysis
library("rJava")
library("syuzhet")
library("lubridate")
#
library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)
#Word Clouds
library("wordcloud")
library("SnowballC")
#colors
library("RColorBrewer")
#fonts
library(extrafont)
font_import() # Import all fonts
fonts() # Print list of all fonts
# configured to work on a Mac, change directory to Unix or Windows
download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular Air-Light 3.46.45 PM.ttf", "/Library/Fonts/Circular Air-Light 3.46.45 PM.ttf", method="curl")

extrafont::font_import(pattern = 'Circular', prompt=FALSE)


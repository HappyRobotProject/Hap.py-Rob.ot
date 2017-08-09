#################################################################
# Playground
#################################################################


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

#Get a token
tokn <- twGetRtweetToken()

result <- to_follow %>%
  dplyr::rowwise() %>%
  dplyr::mutate(followed = followScreenName(screen_name)) 

#drop the table
dbRemoveTable(conn = dbConnections, name =  "twitter_to_follow")
dbDisconnect(dbConnections)

#Get user followers
followers <- get_followers("Hap_py_Rob_ot",token=tokn)
friends <- get_friends("Hap_py_Rob_ot",token=tokn)








# REST 
#################################################################
library(jsonlite)
qod <- fromJSON("http://quotes.rest/qod.json")
quotes <- qod$contents$quotes



#################################################################
nogrid_theme <- function(){
  theme(
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_blank()
  )
}

kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    #panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

kobe_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Impact", colour = "#552683", size = 10),
    legend.background = element_rect(fill = "#E2E2E3"),
    legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    legend.text = element_text(family = "Impact", colour = "#E7A922", size = 10),
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    #panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.y = element_line(colour = "#E7A922"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}



# Sentiment Plot

p1 <- ggplot(data = emotionTotals, aes(x = sentiment, y = value)) +
  geom_bar(aes(fill = sentiment), stat = "identity", color=c(accent,accent,accent,accent,main,main,main,main), fill=c(accent,accent,accent,accent,main,main,main,main)) +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets") + coord_flip()

p1


# Radar Plot
max <- max(emotionTotals$count)
emotionTotals$percent = (emotionTotals$count / max )* 4 + 3
rad <- select(emotionTotals,sentiment, percent)
tran <- spread(rad,key=sentiment, value=percent)
tran <- cbind(group = "Sentiment", tran)
p2 <- radarPlot(tran, grid.max = 7, 
                grid.min = 0, centre.y = 0, plot.legend = FALSE, 
                font.radar = "Arial", axis.label.size = 1.5, 
                group.line.width = 0.25, group.point.size = 0.33,grid.line.width=0.25,
                background.circle.colour = background,
                axis.line.colour = main, 
                gridline.max.colour = main, gridline.min.colour = main, gridline.mid.colour = main,
                group.colour = c(accent),values.radar = c(""), axis.label.colour = accent,label.gridline.min = FALSE
)
p2
radarPlot <- p2 + basic_theme()
radarPlot


# TRIPLE WIDE Charts
library(ggplot2)
y1 <- round(rnorm(n = 36, mean = 7, sd = 2)) # Simulate data from normal distribution
y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
x <- rep(LETTERS[1:12], 3)
grp <- rep(c("Grp 1", "Grp 2", "Grp 3"), each = 12)
dat <- data.frame(grp, x, y1, y2, y3)

p3 <- ggplot(data = dat, aes(x = reorder(x, rep(1:12, 3)), y = y3, group = factor(grp))) +
  geom_bar(stat = "identity", fill = "#552683") + coord_polar() + facet_grid(. ~ grp) +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
p3
p3 + kobe_theme2()

#
# Twitter Helper Functions
#

# Load libraries
library(ggplot2)
library("tm")
library("SnowballC")
library("wordcloud")


#Simple function to allow another script to check if this is loaded
plotutilversion <- function(){
  return("0.0.1")
}

#########################################################
# Themes
#########################################################
pltBasicTheme <- function(){
  theme(
    plot.background = element_rect(fill = "transparent", colour = "transparent"),
    plot.margin = unit(c(0,0,0,0),"npc"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    panel.spacing = unit(c(0,0,0,0), "npc"),
    legend.position = "none"
  )
}
pltBarTheme <- function(){
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

pltColorTheme <- function(){
  #Fonts
  response <- list(font = "Impact",
                   background = "#2d3142",
                   white = "#ffffff",
                   main = "#7084a5",
                   light = "#A1CEE0",
                   accent = "#ef8354",
                   accent.light = "#C52D41",
                   highlight = "#bfc0c0",
                   negative = "#ef8354",
                   positive = "#7084a5" )
  return(response)
}



pltRadar <- function(plot.data,
                    font.radar="Arial",
                    values.radar = c("0%", "50%", "100%"),                       
                    axis.labels=colnames(plot.data)[-1],                             
                    grid.min=0,  #10,
                    grid.mid=0.5,  #50,
                    grid.max=1,  #100,
                    centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                    plot.extent.x.sf=1,
                    plot.extent.y.sf=1.2,
                    x.centre.range=0.02*(grid.max-centre.y),
                    label.centre.y=FALSE,
                    grid.line.width=0.5,
                    gridline.min.linetype="longdash",
                    gridline.mid.linetype="longdash",
                    gridline.max.linetype="longdash",
                    gridline.min.colour="grey",
                    gridline.mid.colour="#007A87",
                    gridline.max.colour="grey",
                    grid.label.size=7,
                    gridline.label.offset=-0.1*(grid.max-centre.y),
                    label.gridline.min=TRUE,
                    axis.label.offset=1.15,
                    axis.label.size=8,
                    axis.label.colour="grey",
                    axis.line.colour="grey",
                    group.line.width=1.5,
                    group.point.size=6,
                    background.circle.colour="#D7D6D1",
                    background.circle.transparency=0.2,
                    plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                    legend.title="",
                    plot.title="",
                    legend.text.size=grid.label.size,
                    group.colour = c("#0000FF","#00FFFF")) {
  
  library(ggplot2)
  
  plot.data <- as.data.frame(plot.data)
  
  plot.data[,1] <- as.factor(as.character(plot.data[,1]))
  names(plot.data)[1] <- "group"
  
  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  #Declare required internal functions
  
  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    
    path <- df[,1]
    
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        #pathData[,j]= pathData[,j]
        
        
        graphData=rbind(graphData, data.frame(group=i, 
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    graphData #data frame returned by function
  }
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  
  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=20) + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))
  
  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, family=font.radar, color=axis.label.colour) +
    scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar,color=axis.label.colour)
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, family=font.radar,color=axis.label.colour)
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)
  
  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
  
  
  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    
    base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1, family=font.radar) }
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5, family=font.radar) }
  
  base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20,
                                                                                    family = font.radar)) +
    theme(legend.text = element_text(size = legend.text.size), legend.position="left") +
    theme(legend.key.height=unit(2,"line")) +
    #scale_colour_manual(values=rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)) +
    scale_colour_manual(values=group.colour) +
    theme(text=element_text(family=font.radar)) + 
    theme(legend.title=element_blank())
  
  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }
  
  return(base)
  
}





pltEmotion <- function(emotionData){
  colors <- pltColorTheme()
  plotcolors=c(colors$negative,colors$negative,colors$negative,colors$negative,colors$positive,colors$positive,colors$positive,colors$positive)
  emotionPlot <-ggplot(emotionData, aes(sentiment, value, label = sentiment, hjust = hjust)) + 
    #geom_bar(stat = "identity", aes(fill = colour))
    geom_text(aes(y = 0, colour = sentiment, family=colors$font),size=1.75, color=plotcolors) + 
    geom_bar(stat = "identity", aes(fill = sentiment), width = 0.6, color=plotcolors, fill=plotcolors) +
    coord_flip() + labs(x = "", y = "") +
    scale_x_discrete(breaks = NULL) + 
    theme(legend.position = "none") + scale_y_continuous(limits=c(-110, 110)) + #scale_x_continuous(expand = c(0,0)) + 
    pltBarTheme()
  return(emotionPlot)
}



pltDonut <- function(donutData){
  #########################################################
  # Positive / Negative Donut Chart
  #########################################################
  theme <- pltColorTheme()
  
  # Add addition columns, needed for drawing with geom_rect.
  donutData$fraction = donutData$count / sum(donutData$count)
  #donutData = donutData[order(donutData$sentiment), ]
  donutData <- dplyr::arrange(donutData,desc(sentiment))
  donutData <- transform(donutData, sentiment = reorder(sentiment, count))
  donutData$ymax = cumsum(donutData$fraction) +0.0
  donutData$ymin = c(0, head(donutData$ymax, n=-1)) +0.0
  donutData$ymid = ((donutData$ymax - donutData$ymin)/2) + donutData$ymin
  
  # Make the plot
  donutPlot = ggplot(donutData, aes(fill=sentiment, ymax=ymax, ymin=ymin, xmax=4, xmin=2.0)) +
    #donutPlot <- ggplot(donutData, aes(fill=sentiment, ymax=ymax, ymin=ymin)) + #, xmax=4, xmin=2.5)) +
    #scale_x_continuous(limits = c(0, 4), expand = c(0,0)) +  #scale_y_continuous(limits = c(ymin, ymax), expand = c(0,0)) +
    geom_rect(fill=c(theme$positive,theme$negative), xmax=4, xmin=2.0) +
    #scale_fill_manual(name="Overall Sentiment", values = c(accent, main), label=c("Negative","Positive")) +  
    geom_text( aes(label = paste(round(fraction * 100, digits = 0),"%",sep=""), y=donutData$ymid, x = 3), size=2.25, fontface="bold", color=theme$background)+
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
  donutPlot <- donutPlot + pltBasicTheme()
  
  return(donutPlot)
}



pltCreateImage <- function(listData, filename =  "/Users/Tim/data/images/userInfographic.png"){
  
  library(grid)
  result <- 0
  theme <- pltColorTheme()
  print(filename)
  
  
  png(filename, width = 4, height = 3, units = "in", res = 500)
  # Plot the word cloud (will force a new page)
  par(bg = theme$background, fig=c(0.1,0.9,0.1,0.9), mar=c(0,0,0,0))
  wordcloud(words = listData$tweetWords$word, freq = listData$tweetWords$freq, scale = c(2,0.25), min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, family = theme$font, colors=c(theme$main, theme$highlight, theme$accent))
  vp1 <- viewport(x = 0, y = 0.85, w = 1.0, h = 0.15, just = c("left", "bottom"), name = "vp1")
  pushViewport(vp1)
  grid.rect(gp = gpar(fill = theme$background, col = theme$background))
  grid.text(listData$query, vjust = 0, y = unit(0.5, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 1.6))
  grid.text(paste("Infographic created for",listData$sender,"on", format(Sys.Date(), format="%B %d, %Y"), sep = " "), vjust = 0, y = unit(0.1, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 0.8))
  upViewport()
  
  #Right Pane
  vp2 <- viewport(x = 0.77, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp2")
  vpEmotion <- viewport(x = 0.0, y = 0.85, w = 1.0, h = 0.5, just = c("left", "top"))
  pushViewport(vp2)
  print(listData$emotionPlot, vp=vpEmotion)
  grid.text("Tweet Emotions", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 0.65))
  grid.text(paste(
    "Infographic",
    "created by",
    "@Hap_py_Rob_ot",
    "https://git.io/vQWQn",
    sep = "\n"), vjust = 0, hjust = 0, x = unit(0.05, "npc"), y = unit(0.05, "npc"), gp = gpar(fontfamily = theme$font, col = theme$accent, cex = 0.5))
  
  upViewport()
  
  #Left Pane
  vp3 <- viewport(x = 0.0, y = 0.0, w = 0.23, h = 0.85, just = c("left", "bottom"), name = "vp3")
  vpDonut <- viewport(x = 0.5, y = 0.75, w = 1.0, h = 1.0)
  pushViewport(vp3)
  #grid.rect(gp = gpar(fill = theme$background, col = theme$accent.light))
  print(listData$donutPlot, vp=vpDonut)
  grid.text("Sentiment Analysis", vjust = 0, y = unit(0.9, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 0.65))
  grid.text("Positive Sentiment", vjust = 0, y = unit(0.53, "npc"), gp = gpar(fontfamily = theme$font, col = theme$main, cex = 0.4))
  grid.text("Negative Sentiment", vjust = 0, y = unit(0.5, "npc"), gp = gpar(fontfamily = theme$font, col = theme$accent, cex = 0.4))
  #Tweet Count
  grid.text(listData$tweetCount, vjust = 0, y = unit(0.35, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 1.75))
  grid.text("tweets", vjust = 0, y = unit(0.31, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 0.7))
  
  #grid.rect(gp = gpar(fill = theme$background, col = theme$background))
  upViewport()
  vp4 <- viewport(x = 0.50, y = 0.10, w = 0.7, h = 0.10, just = c("center", "top"), name = "vp4")
  pushViewport(vp4)
  #grid.rect(gp = gpar(fill = theme$accent, col = theme$accent))
  grid.text("Get your own infographic", y = unit(0.88, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 0.4))
  grid.text(paste(
    "Follow @Hap_py_Rob_ot",
    "Send a Message with the hashtag or search term for the infographic",
    "Be a bit patient ;).  Average processing is 15 - 30 minutes", sep = "\n"), x = unit(0.5, "npc"), y = unit(0.4, "npc"), gp = gpar(fontfamily = theme$font, col = theme$highlight, cex = 0.3))
  dev.off()
  
  result <- 1
  return(result)
}



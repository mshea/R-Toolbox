# Function to convert long table to wide table with keys and values of 1 per day.
sortTagsIntoTable <- function(df, colnum) {
  ddf <- data.frame(df[1],df[2])
  ddf <- ddf[!duplicated(ddf),]
  tagtable <- table(ddf[2])
  tagframe <- as.data.frame(tagtable)
  sortedTags <- tagframe[with (tagframe, order(-Freq)),]
  topTags <- as.vector(sortedTags[[1]])
  dateTags <- data.frame(ddf[1], ddf[2])
  d <- as.data.frame.matrix(table(dateTags))[topTags][1:colnum]
}

# Function to turn values into keys with values of "1"
turnValuesIntoKeys <- function(df) { # Dump keys, turn values into keys, add new value to 1  
  newf <- data.frame(df$datetime, df$value) # Dump the key column
  colnames(newf) <- c("datetime", "key") # rename value into key
  newf$value <- rep(1,nrow(newf)) # give the new data a value column with 1
  return(newf)
}

# Function to display a number of stacked binary sparklines
binarySparkline <- function(d, tagsize, dates, titletext) {
  v <- 0
  par(mfrow=c(ncol(d),1), mar=c(.1,tagsize,.1,.1), lwd=1, oma=c(1.5,.1,.1,.1))
  for (y in d) {
    v <- v + 1
    par(las=1)
    colName <- paste(colnames(d)[v], " ", sum(y))
    y <- c(y,0)
    x <- c(1:length(y))    
    plot(x, y, space=NULL, type="n", pch=124, frame=F, xaxt='n',  yaxt="n", ann=FALSE, cex=1, col='#aaaaaa')
    # Build polygon construct
    y2 <- rep(y, each=2)
    y2 <- y2[-length(y2)]
    x2 <- rep(x, each=2)[-1]
    x3 <- c(min(x2), x2, max(x2))
    y3 <- c(0, y2, 0)
    polygon(x3, y3, border=NA, col="grey")
    axis(side=2, at=.5, pos=1, labels=colName, lwd=F)
  }
  firstdate = format(dates[1], format="%b")
  seconddate = format(dates[max(x)*.25], format="%b")
  thirddate = format(dates[max(x)*.5], format="%b")
  fourthdate = format(dates[max(x)*.75], format="%b")
  fifthdate = format(dates[max(x)-1], format="%b")
  axis(side=1, pos=c(.75), at=c(1,max(x)*.25,max(x)*.5,max(x)*.75,max(x)-1), lwd="0", labels=c(firstdate, seconddate, thirddate, fourthdate, fifthdate), cex.axis=.8)
}

# Function to filter data. taglist is the list of tags to include. 
# stopwords is the list of tags to omit
filterData <- function(d, taglist, stopwords, valuesaskeys) {
  if (length(taglist) >= 1) {
    taglist <- c("Create", taglist)
    d <- subset(d, (key %in% taglist))
  }
  for (stopword in stopwords) {
    d <- d[d[,2] != stopword,]
    d$value[d$key == "Create"] <- 1 # turn create values into 1.
  }
  if (valuesaskeys > 0) {d <- turnValuesIntoKeys(d)}
  return(d)
}

# Params
newd <- read.csv("./Desktop/lifedata.csv")
outputdir <- "~/Desktop/"
displaynames <- c()
stopwords <- c("Sleep", "Relax", "Love", "Thinking About", "Befriend", "Health","Happiness", "Latitude", "Longitude", "Event")
newd <- newd[order(as.Date(newd$datetime, format="%m/%d/%Y %I:%M:%S %p")),]
dates <- unique(c(as.Date(newd$datetime, format="%m/%d/%Y %I:%M:%S %p")))

view_values_as_keys <- 0
tagnames <- c("Listened to Friend","Emailed Friend","Ran RPG","Game With Friends")
tagsize <- 10 # How much space do you need on the left side of the graph?
filtereddata <- filterData(newd, tagnames, stopwords, view_values_as_keys)
numrows <- length(unique(filtereddata$key))
newtable <- sortTagsIntoTable(filtereddata, numrows)
newtable <- newtable[rowSums(newtable) > 0, , drop=FALSE]
if (length(displaynames) > 1) {colnames(newtable) <- displaynames}
newtable[1] <- NULL # this dumps the Create
newtable["Talked to Friend"] <- rowSums(newtable[c(1,2,3,4)])
newtable["Talked to Friend"][newtable["Talked to Friend"] > 1] <- 1
svgheight <- (ncol(newtable)*.6)+1.2 # make the size of the svg based on number of columns of data
svg(filename=paste(outputdir, "befriend.svg", sep=""), height=svgheight, width=20, pointsize=40)
binarySparkline(newtable, tagsize, dates, "Befriend")
dev.off()

goaldata <- newtable["Talked to Friend"]

view_values_as_keys <- 0
tagnames <- c("Listened to Michelle","Called Mom","TV With Michelle","Game With Michelle","Movie With Michelle")
tagsize <- 10 # How much space do you need on the left side of the graph?
filtereddata <- filterData(newd, tagnames, stopwords, view_values_as_keys)
numrows <- length(unique(filtereddata$key))
newtable <- sortTagsIntoTable(filtereddata, numrows)
newtable <- newtable[rowSums(newtable) > 0, , drop=FALSE]
if (length(displaynames) > 1) {colnames(newtable) <- displaynames}
newtable[1] <- NULL # this dumps the Create
newtable["Loved"] <- rowSums(newtable[c(1,2,3,4,5)])
newtable["Loved"][newtable["Loved"] > 1] <- 1
svgheight <- (ncol(newtable)*.6)+1.2 # make the size of the svg based on number of columns of data
svg(filename=paste(outputdir, "love.svg", sep=""), height=svgheight, width=20, pointsize=40)
binarySparkline(newtable, tagsize, dates, "Love")
dev.off()

goaldata["Loved"] <- newtable["Loved"]

view_values_as_keys <- 0
tagnames <- c("Read","Videogame","Audiobook","TV","Movie")
tagsize <- 10 # How much space do you need on the left side of the graph?
filtereddata <- filterData(newd, tagnames, stopwords, view_values_as_keys)
numrows <- length(unique(filtereddata$key))
newtable <- sortTagsIntoTable(filtereddata, numrows)
newtable <- newtable[rowSums(newtable) > 0, , drop=FALSE]
if (length(displaynames) > 1) {colnames(newtable) <- displaynames}
newtable[1] <- NULL # this dumps the Create
newtable["Relaxed"] <- rowSums(newtable[c(1,2,3,4,5)])
newtable["Relaxed"][newtable["Relaxed"] > 1] <- 1
svgheight <- (ncol(newtable)*.6)+1.2 # make the size of the svg based on number of columns of data
svg(filename=paste(outputdir, "relax.svg", sep=""), height=svgheight, width=20, pointsize=40)
binarySparkline(newtable, tagsize, dates, "Relax")
dev.off()

goaldata["Relaxed"] <- newtable["Relaxed"]

view_values_as_keys <- 0
tagnames <- c("Write","Edit","Code","Ran RPG","Freelance","Podcast")
tagsize <- 10 # How much space do you need on the left side of the graph?
filtereddata <- filterData(newd, tagnames, stopwords, view_values_as_keys)
numrows <- length(unique(filtereddata$key))
newtable <- sortTagsIntoTable(filtereddata, numrows)
newtable <- newtable[rowSums(newtable) > 0, , drop=FALSE]
if (length(displaynames) > 1) {colnames(newtable) <- displaynames}
newtable[1] <- NULL # this dumps the Create
newtable["Created Something"] <- rowSums(newtable[c(1,2,3,4,5,6)])
newtable["Created Something"][newtable["Created Something"] > 1] <- 1
svgheight <- (ncol(newtable)*.6)+1.2 # make the size of the svg based on number of columns of data
svg(filename=paste(outputdir, "create.svg", sep=""), height=svgheight, width=20, pointsize=40)
binarySparkline(newtable, tagsize, dates, "Create")
dev.off()


goaldata["Created"] <- newtable["Created Something"]

view_values_as_keys <- 0
tagnames <- c("Create","Ate Well","10k Steps","Stairs","Hike")
tagsize <- 10 # How much space do you need on the left side of the graph?
filtereddata <- filterData(newd, tagnames, stopwords, view_values_as_keys)
numrows <- length(unique(filtereddata$key))
newtable <- sortTagsIntoTable(filtereddata, numrows)
newtable <- newtable[rowSums(newtable) > 0, , drop=FALSE]
if (length(displaynames) > 1) {colnames(newtable) <- displaynames}
newtable[1] <- NULL # this dumps the Create
newtable["Healthy"] <- rowSums(newtable[c(1,2,3,4)])
newtable["Healthy"][newtable["Healthy"] > 1] <- 1
svgheight <- (ncol(newtable)*.6)+1.2 # make the size of the svg based on number of columns of data
svg(filename=paste(outputdir, "health.svg", sep=""), height=svgheight, width=20, pointsize=40)
binarySparkline(newtable, tagsize, dates, "Health")
dev.off()

goaldata["Healthy"] <- newtable["Healthy"]

goaldata <- data.frame(goaldata["Created"],goaldata["Relaxed"],goaldata["Loved"],goaldata["Talked to Friend"],goaldata["Healthy"])
colnames(goaldata)[4] <- "Befriended"

tagsize <- 10 # How much space do you need on the left side of the graph?
newtable <- goaldata
newtable <- newtable[rowSums(newtable) > 0, , drop=FALSE]
if (length(displaynames) > 1) {colnames(newtable) <- displaynames}
svgheight <- (ncol(newtable)*.6)+1.2 # make the size of the svg based on number of columns of data
svg(filename=paste(outputdir, "goal_activity.svg", sep=""), height=svgheight, width=20, pointsize=40)
binarySparkline(newtable, tagsize, dates, "Goal Activity")
dev.off()
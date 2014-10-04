outputdir <- "~/Desktop/"

# Change the path below to the URL where you host your lifetracker app
d <- read.csv("~/Desktop/lifedata.csv")
d <- d[order(as.Date(d$datetime, format="%m/%d/%Y %I:%M:%S %p")),]
#d <- d[d$key =="atewell" | d$key == "atepoorly",]
dt <- reshape(d, idvar="datetime", timevar="key", direction="wide")
location <- data.frame(dt['value.latitude'], dt['value.longitude'])
dt <- data.frame(dt['datetime'],dt['value.create'], dt['value.relax'], dt['value.love'], dt['value.befriend'], dt['value.health'], dt['value.happiness'])
names(dt) <- c("datetime","Create","Relax","Love","Befriend","Health","Happiness")
dt <- dt[complete.cases(dt),]
dates <- c(as.Date(dt$datetime, format="%m/%d/%Y %I:%M:%S %p"))
dt$datetime <- NULL


binarySparkline <- function(d) {
  v <- 0
  par(mfrow=c(ncol(d),1), mar=c(.1,6,.1,0), lwd=1)
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
    polygon(x3, y3, border=NA, col="#666666")
    axis(side=2, at=.5, pos=1, labels=colName, lwd=F)
  }
}

# Function for converting a set of date, key, values
# into a table of 0s and 1s sorted most to least. Used for binary Sparklines.
sortTagsIntoTable <- function(df) {
  ddf <- data.frame(df[1],df[2])
  ddf <- ddf[!duplicated(ddf),]
  tagtable <- table(ddf[2])
  tagframe <- as.data.frame(tagtable)
  sortedTags <- tagframe[with (tagframe, order(-Freq)),]
  topTags <- as.vector(sortedTags[[1]])
  dateTags <- data.frame(ddf[1], ddf[2])
  d <- as.data.frame.matrix(table(dateTags))[topTags][1:50]
}

# Output for binarySparklines with csv input
stopwords <- c("relax", "love", "thinkingabout", "befriend", "health","happiness", "latitude", "longitude", "sp500", "weather_desc","temp_f")

dtags <- d
for (stopword in stopwords) {
  dtags <- dtags[dtags[,2] != stopword,]
}
dtags <- sortTagsIntoTable(dtags)
dtags <- dtags[rowSums(dtags) > 1, , drop=FALSE] 
dtags <- dtags[c("atewell", "atepoorly")]

svg(filename=paste(outputdir, "tags.svg", sep=""), height=1.5, width=20, pointsize=35)
binarySparkline(dtags)
dev.off()
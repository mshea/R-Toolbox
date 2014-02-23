# This R script contains a series of functions for displaying information
# in a data-rich format based on the ideas of Edward Tufte's 
# "The Visual Display of Quantitative Information".
#
# Each visualization is intended to maximize the data to ink ratio
# by showing mean values and quantiles instead of default scales.
# Each visualization is also tuned to display a full year's worth of data. 
#
# Each function takes in a dataframe in a particular format and outputs
# a data-rich graphic. You may output this to the default Quartz view or to
# an image using the command:
#
# png(filename="filename.png", height=1600, width=1200, pointsize=24)
# To reset to Quartz after outputting an image, type:
# dev.off()
#
# The following functions are included:
#
# - binarySparkline: for displaying a number of stacked sparklines representing
# 1s and 0s.
# - smallMultiples: for displaying a series of line plots comparing similar data
# - multipleBoxplot: for displaying a number of boxplots with measured total
# quantiles

# binarySparkline
# This function takes in a dataframe containing rows of categories or tags
# each with an equal number of rows containing either 1s or 0s.
# For example:
# Read, Watched TV, Played Games, Walked
# 0,1,0,1
# 1,1,0,1
# 1,1,1,0
#
# Use the sortTagsIntoTable function to convert a set of dates, tags, and values
# into a table of 0s and 1s by tag and date for use in the binarySparkline
# function

binarySparkline <- function(d) {
	v <- 0
	par(mfrow=c(ncol(d),1), mar=c(.1,10,.1,.1), lwd=1)
	for (y in d) {
		v <- v + 1
		par(las=1)
		colName <- paste(colnames(d)[v], " ", sum(y))
		plot(y, space=NULL, ylim=c(1, 1), type="p", pch=124, frame=F, xaxt='n',  yaxt="n", ann=FALSE, cex=1, col='#aaaaaa')
		axis(side=2, at=1, pos=1, labels=colName, lwd=F)
	}
}

# Function for converting a set of date, key, values
# into a table of 0s and 1s sorted most to least. Used for binary Sparklines.
sortTagsIntoTable <- function(df) {
	tags <- as.data.frame(table(df[2]))
	attach(tags)
	sortedTags <- tags[order(-Freq),]
	topTags <- as.vector(sortedTags[[1]])
	dateTags <- data.frame(df[1], df[2])
	d <- as.data.frame.matrix(table(dateTags))[topTags]
}

# Sample data for sparkline output
d <- data.frame(replicate(50,sample(0:1,365,rep=TRUE)))
png(filename="~/Desktop/tags.png", height=3000, width=1600, pointsize=40)
binarySparkline(d)
dev.off()

# Output for binarySparklines with csv input
df <- read.csv("~/Desktop/tags.csv")
d <- sortTagsIntoTable(df)
png(filename="~/Desktop/tags.png", height=3000, width=800, pointsize=40)
binarySparkline(d)
dev.off()


# This function takes in a dataframe of roughly six columns
# and displays them on a series of stacked line charts.
# The mean is displayed along with a jittered rug instead of the typical
# default Y scale.

smallMultiples <- function(df, dates){
	par(mfrow=c(ncol(df),1), mar=c(1,5,1,.1), lwd=1, oma=c(3,.2,.2,.2))
	x <- 1
	dateTicks <- seq(from = dates[1], to = tail(dates, n=1), length.out=5)
	for (i in df) {
		plot(dates, i, frame=F, font.main = 1, main="", type="n", ylim=c(1,10), yaxt='n', xaxt="n", xlab="" , ylab=colnames(df)[x], col='#333333')
		quans <- quantile(i, names=FALSE)
		axis(side=2, at=quans[3], lwd="0", lwd.ticks="0", las=1, labels=quans[3])
		rug(jitter(i, amount=.5), side=2, ticksize = -.1, col='#aaaaaa')
		rect(dates[1], quans[2], dates[-1], quans[4], border="#eeeeee", col="#eeeeee")
		lines(dates, rep(quans[3], times=length(dates)), col="#aaaaaa")
		lines(dates, i, col="#333333")
		x <- x + 1
	}
	axis.Date(side=1, pos=c(-1), at=dateTicks, lwd="0", lwd.ticks="1", format="%d %b", col="#cccccc")
}

# Replicate dates for small multiples, should be the same number of rows
md <- data.frame(replicate(6,sample(1:10,365,rep=TRUE)))
dates <- seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days")
smallMultiples(md, dates)

# Output for smallMultiples using csv data.
d <- read.csv("~/Desktop/lifedata.csv")
dates <- c(as.Date(d$Date, format="%m/%d/%Y"))
df <- data.frame(d$Create, d$Relax, d$Love, d$Befriend, d$Health, d$Happiness)
colnames(df) <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
smallMultiples(df, dates)


# This function takes in a dataframe containing a number of columns with 
# an equal number of numeric rows of data between 1 and 10 and generates 
# a boxplot to compare them together.

multipleBoxplot <- function(d) {
	par(mfrow=c(1,1), lwd=1)
	boxplot(d,frame=F,yaxt='n', bty='n', xlab="", ylab="",xaxt='n')
	axis(side=1, c(1:6), lwd="0", lwd.ticks="0", las=1, labels=colnames(d))
	total <- c()
	for (i in d) {
		total <- c(total, i)
	}
	axis(side=2, c(quantile(total, names=FALSE, type=1)), lwd="0", lwd.ticks="1", las=1)
}

# Sample boxplot output
md <- data.frame(replicate(6,sample(1:10,365,rep=TRUE)))
multipleBoxplot(md)

# This function takes in a dataframe containing dates and times. Dates should be
# in YYYY-MM-DD format and times should be in HH:MM:SS format. Each column
# should be labeled with "Dates" and "Times".

generateDateTimeScatterplot <- function(df) {
	#png(filename="~/Desktop/scatterplot.png", height=600, width=1200, pointsize=9)
	par(mar=c(3,4,1,1), cex=2)
	Dates <- as.POSIXlt(df$Dates)
	xTicks <-quantile(Dates, names=FALSE)
	Times <- as.POSIXct(df$Times, format="%H:%M:%S")
	yTicks <- quantile(Times, names=FALSE)
	xLabels <- format(as.Date(xTicks), "%d %b %Y")
	yLabels <- format(as.POSIXct(yTicks, origin="1960-01-01 00:00:00"), "%H:%M")
	plot(Dates,Times,pch='.',frame=F, xaxt='n', yaxt='n', bty='n', xlab="", ylab="")
	axis(side=1, xTicks, lwd="0", lwd.ticks="1", labels=xLabels)
	axis(side=2, yTicks, lwd="0", lwd.ticks="1", labels=yLabels, las=1)
}

# Output for Scatterplot
generateDateTimeScatterplot(read.csv("~/Desktop/sample_date_time_data.csv"))


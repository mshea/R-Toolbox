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

binarySparkline <- function(d) {
	par(mfrow=c(ncol(d),1), mar=c(.1,10,.1,.1), lwd=1)
	v <- 0
	for (y in d) {
		v <- v + 1
		x <- seq_along(y)
		y2 <- rep(y, each=2)
		y2 <- y2[-length(y2)]
		x2 <- rep(x, each=2)[-1]
		x3 <- c(min(x2), x2, max(x2))
		y3 <- c(0, y2, 0)
		colName <- paste(colnames(d)[v], " ", sum(y))
		par(las=1)
		plot(x, y, space=NULL, ylim=c(0, max(y)), type="n", pch=20, frame=F, xaxt='n',  yaxt="n", ann=FALSE)
		axis(side=2, at=.5, pos=1, labels=colName, lwd=F)
		polygon(x3, y3, border="#999999", col="#999999")
	}
}

# Replicate the binarySparkline data frame and run the function
binaryData <- data.frame(replicate(25,sample(0:1,365,rep=TRUE)))
binarySparkline(binaryData)

# This function takes in a dataframe of roughly six columns
# and displays them on a series of stacked line charts.
# The mean is displayed along with a jittered rug instead of the typical
# default Y scale.

smallMultiples <- function(df, dates){
	par(mfrow=c(ncol(df),1), mar=c(3,5,2,1), lwd=2)
	x <- 1
	for (i in df) {
		plot(dates, i, frame=F, font.main = 1, main=colnames(df)[x], type="l", ylim=c(1,10), yaxt='n',xlab="" , ylab="")
		yAxisTicks <- c(1,round(mean(i), digits=2),10)
		axis(side=2, yAxisTicks, lwd="0", lwd.ticks="0", las=1, labels=yAxisTicks)
		rug(jitter(i), side=2, ticksize = -.07)
		x <- x + 1
	}
}

# Replicate dates for small multiples, should be the same number of rows
md <- data.frame(replicate(6,sample(1:10,365,rep=TRUE)))
dates <- seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days")
smallMultiples(md, dates)


# This function takes in a dataframe containing a number of columns with 
# an equal number of numeric rows of data between 1 and 10 and generates 
# a boxplot to compare them together.

multipleBoxplot <- function(d) {
	par(mfrow=c(1,1), lwd=3)
	boxplot(d,frame=F,yaxt='n', bty='n', xlab="", ylab="",xaxt='n')
	axis(side=1, c(1:6), lwd="0", lwd.ticks="0", las=1, labels=colnames(d))
	total <- c()
	for (i in d) {
		total <- c(total, i)
	}
	axis(side=2, c(quantile(total, names=FALSE, type=1)), lwd="0", lwd.ticks="1", las=1)
}

# Replicate the Boxplot Data
md <- data.frame(replicate(6,sample(1:10,365,rep=TRUE)))
multipleBoxplot(md)


# Output for smallMultiples & multipleBoxplot:
d <- read.csv("~/Desktop/lifedata.csv")
dates <- c(as.Date(d$Date, format="%m/%d/%Y"))
colnames(dates) <- "Date"
df <- data.frame(d$Create, d$Relax, d$Love, d$Befriend, d$Health, d$Happiness)
colnames(df) <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
smallMultiples(df, dates)

multipleBoxplot(df)

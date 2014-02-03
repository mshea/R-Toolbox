# Build scatterplot for emails received

# Expects a CSV of dates in YYYY-MM-DD and times in HH:MM:SS 
# with Date and Time headers
d = read.csv("~/Desktop/received.csv")

Date = d$Date
Time = d$Time

# Get the mean, min, max, upper quartile, lower quartiles
Date <- as.Date(Date)
Time <- as.POSIXct(strptime(Time, "%H:%M:%S"))

# Get the summaries of date and time to indentify the min, mean, max, 1st quartile, and 2nd quartile.

summary(Date)
summary(Time)

xTicks <- c("2004-06-08","2008-06-02","2010-03-18","2012-08-25","2014-01-25")
xTicks <-as.Date(xTicks)

yTicks <- c("00:00:00","10:08:01","13:46:38","17:57:28","23:59:55")
yTicks <- as.POSIXct(strptime(yTicks, "%H:%M:%S"))

xLabels <- c("8 Jun 2004", "2 Jun 2008", "18 Mar 2010", "25 Aug 2012", "25 Jan 2014")
yLabels <- c("00:00", "10:08", "13:46", "17:57", "23:59")

plot(Date,Time,pch=".",frame=F, xaxt='n', yaxt='n', bty='n', xlab="", ylab="")
axis(side=1, xTicks, lwd="0", lwd.ticks="1", labels=xLabels)
axis(side=2, yTicks, lwd="0", lwd.ticks="1", labels=yLabels, las=1)

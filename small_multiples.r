# This function expects a data frame with a number of vectors. The first
# of these vectors should be called "Date" and have a date in mm/dd/yyyy format.

smallMultiples <- function(df){
	Dates <- as.Date(df$Date, format="%m/%d/%Y")
	df$Date <- NULL
	displayRows <- ncol(df) / 2 
	# Show extra columns if odd number.
	if ((ncol(df) %% 2) != 0){ displayRows <- displayRows + 1 }
	par(mfrow=c(displayRows,2), mar=c(5,5,2,1))
	x <- 1
	for (i in df) {
		plot(Dates, i, frame=F, font.main = 1, main=colnames(df)[x], type="l", ylim=c(1,10), yaxt='n',xlab="" , ylab="")
		yAxisTicks <- c(1,round(mean(i), digits=2),10)
		axis(side=2, yAxisTicks, lwd="0", lwd.ticks="0", las=1, labels=yAxisTicks)
		rug(jitter(i), side=2, ticksize = -.07)
		x <- x + 1
	}
}

d <- read.csv("~/Desktop/lifedata.csv")
df <- data.frame(d$Date, d$Create, d$Relax, d$Love, d$Befriend, d$Health, d$Happiness)

colnames(df) <- c("Date", "Create", "Relax", "Love", "Befriend", "Health", "Happiness")
smallMultiples(df)
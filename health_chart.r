d <- read.csv("~/Desktop/lifedata.csv")
d <- d[order(as.Date(d$datetime, format="%m/%d/%Y %I:%M:%S %p")),]
d <- reshape(d, idvar="datetime", timevar="key", direction="wide")
d <- data.frame(d['datetime'],d['value.health'])
names(d) <- c("datetime","Health")
d$Date <- c(as.Date(d$datetime, format="%m/%d/%Y %I:%M:%S %p"))
d$datetime <- NULL
d <- d[complete.cases(d),]
d$Health <- as.numeric(levels(d$Health))[d$Health]
dateTicks <- seq(from = d$Date[1], to = tail(d$Date, n=1), length.out=5)
svg(filename="~/Desktop/health.svg", height=4, width=20, pointsize=35)
par(mar=c(3, 5, 1, 0))
plot(d$Date, d$Health, frame=F, font.main = 1, main="", type="n", yaxt='n', xaxt="n", xlab="", ylab="Health", col='#333333')
quans <- quantile(d$Health, names=FALSE)
imean <- round(mean(d$Health), digits=2)
axis(side=2, at=imean, lwd="0", lwd.ticks="0", las=1, labels=imean, hadj=.3)
#rug(jitter(d$Health, amount=.2), side=2, ticksize = -.15, col='#aaaaaa', line=-1)
rect(d$Date[1], quans[2], d$Date[-1], quans[4], border="#eeeeee", col="#eeeeee")
lines(d$Date, rep(imean, times=length(d$Date)), col="#aaaaaa")
lines(d$Date, rep(1, times=length(d$Date)), col="#aaaaaa")
lines(d$Date, rep(10, times=length(d$Date)), col="#aaaaaa")
lines(d$Date, d$Health, col="#666666", lwd=4)
axis.Date(side=1, at=dateTicks, lwd="0", lwd.ticks="1", format="%d %b", col="#cccccc")
dev.off()

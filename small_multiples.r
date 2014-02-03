plot(Date, Happiness, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", ylab="")
axis(side=1, xTicks, lwd="0", lwd.ticks="1", labels=xLabels)
axis(side=2, c(1:10), lwd="0", lwd.ticks="1", labels=yLabels, las=1)

axis(side=2, c(1:10), lwd="0", lwd.ticks="1", las=1)


axis(side=4, c(1,7.375,10), lwd="0", lwd.ticks="1", las=1, labels=c("1", "7.375", "10"))

axis(side=1, Dates, lwd="0", lwd.ticks="1", las=1, labels=Dates)

# Code to generate a 2 row by 3 column small-multiple of the six life goals.

d = read.csv("~/Desktop/lifedata.csv")

df = c(d$Date, d$Create, d$Relax, d$Love, d$

Date <- as.Date(d$Date)
Create <- d$Create
Relax <- d$Relax
Love <- d$Love
Befriend <= d$Befriend
Health <- d$Health
Happiness <- d$Happiness

png(height=1000, width=1500, pointsize=32, filename="~/Desktop/lifegoal_small_multiples.png")

par(mfrow=c(2,3))

dateTicks <- seq(as.Date("01/01/2014", format = "%d/%m/%Y"),
             by = "15 days", length = 3)
plot(Date, Create, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", font.main = 1, main="Create", ylab="")
axis(side=2, c(1,3,7.312,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","3","7.312","10"))
axis.Date(side=1, at=dateTicks, Date, lwd="0", lwd.ticks="1")
yAxisTicks <- jitter(Create)
axis(side=2, Create, lwd="0", lwd.ticks="1", las=1, labels=F)

plot(Date, Relax, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", font.main = 1, main="Relax", ylab="")
axis(side=2, c(1,2,7.031,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","2","7.031","10"))
axis.Date(side=1, at=dateTicks, Date, lwd="0", lwd.ticks="1")
yAxisTicks <- jitter(Relax)
axis(side=2, yAxisTicks, lwd="0", lwd.ticks="1", las=1, labels=F)

plot(Date, Love, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", font.main = 1, main="Love", ylab="")
axis(side=2, c(1,4,8.344,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","4","8.344","10"))
axis.Date(side=1, at=dateTicks, Date, lwd="0", lwd.ticks="1")
yAxisTicks <- jitter(Love)
axis(side=2, yAxisTicks, lwd="0", lwd.ticks="1", las=1, labels=F)

plot(Date, Befriend, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", font.main = 1, main="Befriend", ylab="")
axis(side=2, c(1,6.812,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","6.812","10"))
axis.Date(side=1, at=dateTicks, Date, lwd="0", lwd.ticks="1")
yAxisTicks <- jitter(Befriend)
axis(side=2, yAxisTicks, lwd="0", lwd.ticks="1", las=1, labels=F)

plot(Date, Health, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", font.main = 1, main="Health", ylab="")
axis(side=2, c(1,2,6.938,9,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","2","6.938","9","10"))
axis.Date(side=1, at=dateTicks, Date, lwd="0", lwd.ticks="1")
yAxisTicks <- jitter(Health)
axis(side=2, yAxisTicks, lwd="0", lwd.ticks="1", las=1, labels=F)

plot(Date, Happiness, frame=F, pch=20, type="l", ylim=c(1,10), xaxt='n', yaxt='n', bty='n', xlab="", font.main = 1, main="Happiness", ylab="")
axis(side=2, c(1,4,7.812,9,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","4","7.812","9","10"))
axis.Date(side=1, at=dateTicks, Date, lwd="0", lwd.ticks="1")
yAxisTicks <- jitter(Happiness)
axis(side=2, yAxisTicks, lwd="0", lwd.ticks="1", las=1, labels=F)
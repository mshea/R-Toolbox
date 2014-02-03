## The R code to generate a nice Tufte-esque boxplot of my lifedata.

d <- read.csv("~/Desktop/lifedata.csv")
Create <- d$Create
Relax <- d$Relax
Love <- d$Love
Befriend <- d$Befriend
Health <- d$Health
Happiness <- d$Happiness

# png(height=900, width=1200, pointsize=24, filename="~/Desktop/lifegoal_boxplot.png")

df2 <- data.frame(Create, Relax, Love, Befriend, Health, Happiness)
boxplot(df2,frame=F,yaxt='n', bty='n', xlab="", ylab="")

total <- c(Create, Relax, Love, Befriend, Health, Happiness)
summary(total) # Change second axis to meet low, 1st Q, mean, 2Q, and high

boxplot(df2,frame=F,yaxt='n', bty='n', xlab="", ylab="",xaxt='n')
axis(side=1, c(1:6), lwd="0", lwd.ticks="0", las=1, labels=c("Create","Relax","Love","Befriend","Health","Happiness"))
axis(side=2, c(1,7,7.394,9,10), lwd="0", lwd.ticks="1", las=1, labels=c("1","7","7.394","9","10"))
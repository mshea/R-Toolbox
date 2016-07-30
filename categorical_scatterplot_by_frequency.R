# This plot generates a long categorical scatterplot sometimes referred to
# as a binary sparkline. It shows events across time for a large number of 
# categories.
#
# In this case, it is flipping values for keys so it can display the values
# for particular keys (like when did I read which books).

require(ggplot2)
require(plyr)
d <- read.csv("~/Documents/github/R Toolbox/lifedata.csv",
              stringsAsFactors = FALSE)
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
# d <- subset(d, datetime > as.Date("2015-07-16"))

values <- c("Read", "Audiobook")
d <- subset(d, key %in% values)
d["key"] <- d["value"]
d["value"] <- 1
d<-d[!(d$key==1),]
counts <- as.data.frame(table(d$key))
colnames(counts)[1] <- "key"
d = join(d,counts,by='key')

d$key <- paste(d$key, " - ", d$Freq, " (", 
  round(d$Freq / nrow(table(d$datetime)) * 100), 
  "%)", sep = "")
d <- transform(d, key = reorder(key, Freq))
svg("~/Desktop/categorical_scatterplot_by_frequency.svg", height=12, width=8)
ggplot(d, aes(x = datetime, y=key)) + 
  #geom_point(color="#555555") + # circles
  geom_point(shape=108, color="#555555", size=4) + # lines
  #geom_point(shape=15, color="#555555", alpha = .5) + # Squares
  theme(
        #axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        #panel.background=element_blank(),
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  ggtitle("Books Read since 1 January 2014")
dev.off()
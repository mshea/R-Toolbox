require(ggplot2)
d <- read.csv("./lifedatatotal.csv")
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
# d <- subset(d, datetime > as.Date("2015-07-16"))
values <- c("Read", "Audiobook")
d <- subset(d, key %in% values)
d["key"] <- d["value"]
d["value"] <- 1
d<-d[!(d$key==1),]
d <- transform(d, key = reorder(key, rev(datetime)))
svg("books.svg", height=6, width=8)
ggplot(d, aes(x = datetime, y=key)) + 
  #geom_point(color="#555555") + # circles
  #geom_point(shape=108, size=4, color="#555555") + # lines
  geom_point(shape=15, color="#555555") + # Squares
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
        #panel.grid.minor=element_blank(),
        plot.background=element_blank())
dev.off()
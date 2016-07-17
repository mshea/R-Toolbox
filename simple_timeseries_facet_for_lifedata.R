require(ggplot2)
d <- read.csv("./lifedatatotal.csv")
values <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
d <- subset(d, key %in% values)
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
d$value <- as.character(d$value)
d$value <- as.numeric(d$value)
d <- ddply(d, "key", transform, mean  = round(mean(value), 2))
d$newtitle <- paste(d$key, d$mean, sep = " ")
d$key <- factor(d$newtitle, levels = values)
svg("~/Desktop/lifeplot.svg", height=5.7, width=6)
ggplot(d, aes(x = datetime, y=value)) + geom_line(color="#555555", size=.2) + 
  facet_grid(newtitle ~ .) +
  scale_y_continuous(breaks=c(10,7,3)) + 
  theme(axis.line=element_blank(),
            #axis.text.x=element_blank(),
            #axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            #panel.border=element_blank(),
            panel.grid.major=element_line(colour = "#eeeeee"),
            #panel.grid.minor=element_blank(),
            plot.background=element_blank(), 
            strip.background =element_blank() )
dev.off()
# This script generates a categorical time series with six categories
# across a number of years.

library(ggplot2)
d <- read.csv("~/Documents/github/R Toolbox/lifedata.csv")
values <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
d <- subset(d, key %in% values)
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
d$value <- as.character(d$value)
d$value <- as.numeric(d$value)
d$key <- factor(d$key, levels = values)

svg("~/Desktop/lifeplot.svg", height=6, width=6)
ggplot(d, aes(x = datetime, y=value)) + 
  geom_line(color="#000000", size=.1) + 
  facet_grid(key ~ .) +
  scale_y_continuous(breaks=c(3,8)) + 
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
# This script generates a boxplot with an overlayed scatterplot
# to show statistical breakpoints along with actual overlayed data.

require(ggplot2)
d <- read.csv("~/Documents/github/R Toolbox/lifedatatotal.csv")
values <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
d <- subset(d, key %in% values)
#d$key <-  levels(droplevels(d$key))
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
d$value <- as.character(d$value)
d$value <- as.numeric(d$value)
d$key <- factor(d$key, levels = values)

svg("~/Desktop/life_boxscatterplot.svg", height=4, width=4)
ggplot(data = d, aes(d$key, d$value)) + 
  #  geom_violin() + 
  geom_boxplot(outlier.size=0, outlier.colour = "#ffffff") + 
  geom_jitter(size=.2, alpha=.2, width=.6, color = "#0000aa") +
  scale_y_continuous(breaks=c(1:10)) +
  labs(x = "Daily Life Goal", y = "Scores since 1 Jan 2014") + 
  theme(
    #axis.line=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_line(colour = "#cccccc"),
    #panel.grid.minor=element_blank(),
    plot.background=element_blank())
dev.off()
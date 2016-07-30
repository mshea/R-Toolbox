# This script generates a huge scatterplot based on the dates and times of events.
# The test case is tweets plotted by date and time. It needs no other data other than
# date and times in a long list, like emails or log entries or whatever.

library(ggplot2)
library(scales)
d <- read.csv("~/Documents/github/R Toolbox/tweet_dates.csv")
#d <- d[1:5000,]
d$date <- as.Date(d$Parsed.Date, "%Y-%m-%d")
d$timestring <- strftime(as.POSIXct(d$Parsed.Date, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S")
d$time <- as.POSIXct(d$timestring, format="%H:%M:%S")
myplot <- ggplot(d, aes(x = date, y=time)) + 
  geom_point(color="#000000", alpha=.05, size=.5, shape=16)  + 
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_line(colour = "#eeeeee"),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(), 
        strip.background =element_blank()) + 
        scale_y_datetime(labels=date_format("%I:%M %p EST"))  + 
  ggtitle("570,773 #dnd Tagged Tweets by Date and Time")
ggsave("~/Desktop/date_time_scatterplot.jpg", 
       plot = myplot, 
       width = 12, height = 6, 
       dpi = 300)
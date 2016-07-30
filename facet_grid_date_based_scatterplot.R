# This plot generates a categorical scatterplot but instead of using 
# jitter to plot the dots, it puts a dot at the right x and y for the date
# beginning at the upper left and going to the lower right.

require(ggplot2)
d <- read.csv("~/Documents/github/R Toolbox/lifedata.csv")
values <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
d <- subset(d, key %in% values)
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
#d <- subset(d, datetime > as.Date("2015-07-26"))
d$value <- as.character(d$value)
d$value <- as.numeric(d$value)

# Convert dates to x y coordinates for each mini-calendar
days_between <- as.numeric(max(d$datetime) - min(d$datetime))
grid_size <- ceiling(sqrt(days_between))
d$date_index = as.numeric(d$datetime - min(d$datetime))
d$x <- d$date_index %% grid_size + 1
d$y <- grid_size - (d$date_index %/% grid_size) 

# Order data for the factord itself
d$key <- factor(d$key, levels = values)
d$value <- factor(d$value, levels = c(10:1))

# Render the plot
svg("~/Desktop/facet_grid_date_based_scatterplot.svg", height=7.5, width=5)
ggplot(data = d, aes(x = x, y = y)) + 
  facet_grid(value ~ key) +
  geom_point(alpha = 1, size = .2, color = "#000000", fill="#000000", shape=15) + 
  labs(x = "Daily Life Score", y = "Life Score from 1 to 10") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.y= element_text(angle = 90),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        legend.position="none",
        #panel.background=element_blank(),
        #panel.border=element_blank(),
        panel.grid.major=element_line(colour = "#f5f5f5"),
        panel.grid.minor=element_line(colour = "#f5f5f5"),
        plot.background=element_blank(),
        strip.background = element_blank() )
dev.off()
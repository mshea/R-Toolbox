# This script takes in a set of long data with dates, a small number of keys,
# and a set of numeric values. It outputs a plot where each pixel represents
# a day in a calendar year colored based on the value of the key.

d <- read.csv("~/Documents/github/R Toolbox/lifedata.csv")
values <- c("Create", "Relax", "Love", "Befriend", "Health", "Happiness")
d <- subset(d, key %in% values)
d$datetime <- as.Date(d$datetime, "%m/%d/%Y")
d$year <- format(d$datetime,'%Y')
#d <- subset(d, datetime > as.Date("2015-07-26"))
d$value <- as.character(d$value)
d$value <- as.numeric(d$value)

# Convert dates to x y coordinates for each mini-calendar
grid_size <- 20
d$date_index = as.numeric(format(d$datetime, "%j")) - 1

d$x <- d$date_index %% grid_size
d$y <- grid_size - (d$date_index %/% grid_size) 

# Order data for the factord itself
d$key <- factor(d$key, levels = values)
#d$year <- factor(d$year, levels = c(10:1))

good_days <- subset(d, value >= 8)
bad_days <- subset(d, value <= 3)
neutral_days <- subset(d, value <= 7 & value >= 4)

svg_pixel_size <- .7

# Render the plot
svg("~/Desktop/categorical_calendar_plot.svg", height=3, width=5)
ggplot(data = good_days) + 
  facet_grid(year ~ key) +
  geom_point(aes(x = x, y = y, color = value), size = svg_pixel_size, color = "#333333", fill="#333333", shape=15) +  
  geom_point(data = bad_days, aes(x = x, y = y), size = svg_pixel_size, color = "#aa0000", fill="#aa0000", shape=15) +  
  geom_point(data = neutral_days, aes(x = x, y = y), size = svg_pixel_size, color = "#aaaaaa", fill="#aaaaaa", shape=15) +  
  labs(x = "red = scored 1-3; gray = scored 4-7; black = scored 8-10", y = "Days scored 8 or better") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
#        axis.title.y= element_text(angle = 90),
#        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
#        panel.background=element_blank(),
        #panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
#        panel.grid.major=element_line(colour = "#f5f5f5"),
#        panel.grid.minor=element_line(colour = "#f5f5f5"),
#        plot.background=element_blank(),
        strip.background = element_blank() )
dev.off()
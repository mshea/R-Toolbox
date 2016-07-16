# Change the path below to the URL where you host your lifetracker app
d <- read.csv("http://mikeshea.net/lifetracker/lifedata.csv")
d <- subset(d, key %in% c("Latitude", "Longitude"))
dt <- reshape(d, idvar="datetime", timevar="key", direction="wide")
location <- data.frame(dt['value.Latitude'], dt['value.Longitude'])
library(maps)
library(mapdata)
svg(filename=paste("~/Desktop/map.svg", sep=""), height=12, width=20, pointsize=20)
map("state")
latitude <- as.numeric(levels(location$value.Latitude))[location$value.Latitude]
longitude <- as.numeric(levels(location$value.Longitude))[location$value.Longitude]
points(longitude, latitude, pch=19, col="blue")
dev.off()

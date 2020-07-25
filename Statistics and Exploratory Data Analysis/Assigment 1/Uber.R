# Loading dataset 
uberData <- read.csv("Uber Request Data.csv", stringsAsFactors=TRUE)

# Structure of dataset
str(uberData)

install.packages("lubridate")

library(lubridate)

############################################################
# Convert all date time to same format.
uberData$Request.timestamp <- parse_date_time(uberData$Request.timestamp, "dmY HMS", truncated = 3)
uberData$Drop.timestamp <- parse_date_time(uberData$Drop.timestamp, "dmY HMS", truncated = 3)
uberData$weekday <- wday(uberData$Request.timestamp)
uberData$weekday <- factor(uberData$weekday)
uberData$Request.hour <- format(uberData$Request.timestamp, "%H")
uberData$Request.hour <- as.numeric(uberData$Request.hour) 

############################################################
library(ggplot2)
ggplot(uberData, aes(x=Request.hour, col=Status)) + geom_histogram(bins=24)

ggplot(uberData, aes(x=Request.hour, col=Pickup.point)) + geom_histogram(bins=24)

ggplot(uberData, aes(x=weekday, col=Status)) + geom_bar()

ggplot(uberData, aes(x=Status, col=Pickup.point)) + geom_bar()

uberData <- uberData[with(uberData, order(Driver.id, Request.timestamp)), ]

library(dplyr)
install.packages("data.table")

library(data.table)
#setDT(uberdata)[, changeFromPrevious := Request.timestamp - shift(Drop.timestamp, fill = Drop.timestamp[1]), by = Driver.id]
setDT(uberData)[, changeFromPrevious := as.numeric(difftime(Request.timestamp,shift(Drop.timestamp, fill = Drop.timestamp[1]), units="mins")), by = Driver.id]
#Ignore negative waiting time. Also ignore more than 6 hours, because driver could have gone
# to home and come back.
uberData$changeFromPrevious[uberData$changeFromPrevious < 0] <- NA
uberData$changeFromPrevious[uberData$changeFromPrevious > 360] <- NA

# We can see wait time in airport is more than wait time in city.
ggplot(data = uberData, aes( y = changeFromPrevious, x = Pickup.point)) + geom_boxplot()

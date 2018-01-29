
library(ggplot2)
library(dplyr)

data <- read.csv("public_reports_reports_10-26-2017.csv", sep=",", header=TRUE)

start.stop <- data[14:15]

### Plot Start Dates

#convert to character
start.stop$Date.of.Signup <- as.character(start.stop$Date.of.Signup)

#convert to date format
Starts <- as.Date(start.stop$Date.of.Signup, format="%m-%d-%Y" )

# create a data.fame
Starts <- as.data.frame(Starts)

# create days of week
Starts$Day <- weekdays(Starts$Starts)

# get days of week
dwka <- Starts$Day

# make days of week numeric
dwka <- as.numeric(format(Starts$Starts, "%w"))

# create histogram
hist(dwka, breaks=-.5+0:7, labels=unique(Starts$Day[order(dwka)]), main="Starts by Day of Week")


#### Plot Stop Dates

#convert to character
start.stop$Date.of.Signup <- as.character(start.stop$Date.of.Signup)

#convert to character
start.stop$Stop.request.date <- as.character(start.stop$Stop.request.date)

#convert to date format
Stops <- as.Date(start.stop$Stop.request.date, format="%m-%d-%Y" )

# create a data.fame
Stops <- as.data.frame(Stops)

# create days of week
Stops$Day <- weekdays(Stops$Stops)

# get days of week
dwka <- Stops$Day

# make days of week numeric
dwka <- as.numeric(format(Stops$Stops, "%w"))

# create histogram
hist(dwka, breaks=-.5+0:7, labels=unique(Stops$Day[order(dwka)]), main="Stops by Day of Week")




############### With Keywords
cols <- c(4, 7, 14:15)
data2 <- data[,cols]
data2$Date.ofSignup <- as.character(data2$Date.of.Signup)

# https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
# look this up 

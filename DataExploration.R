setwd("/Users/vivianpeng/git/VAX")
CFRwc <- read.delim("WhoopingCough.csv", sep= ",", header = TRUE, stringsAsFactors = FALSE)
CFRCDC<- read.delim("CFR_CDC_WC_Cases.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
require(maps)
require(maptools)
require(ggplot2)
require(plyr)
#################### PLOT DATA ON MAP ########################

# find max and min longitude and latitude
long <- c(min(CFRwc$Long),max(CFRwc$Long) )
lat <- c(min(CFRwc$Lat),max(CFRwc$Lat) )

all_states <- map_data("state")
MapCases <- ggplot(data=CFRwc, aes(Long,Lat))
MapCases + geom_polygon(data=all_states, aes(x=long, y=lat, group=group, color=State), colour="darkgray", fill = "ivory") +
  geom_point(aes(size=Cases)) + xlim(long) + ylim(lat)

#################### SCATTER PLOT DATA ########################
# log scale
ggplot(CFRCDC, aes(x=Year, y=log(Cases))) + geom_point(aes(colour=Source)) + ggtitle("Pertusis Cases CDC vs. CFR")

# non-log scale
ggplot(CFRCDC, aes(x=Year, y=Cases)) + geom_point(aes(colour=Source)) + ggtitle("Pertusis Cases CDC vs. CFR")



#################### PLOT DATA BY IMPACT SCALE ########################
ggplot(CFRwc, aes(x = Year, y = Cases, color = ImpactScale)) + geom_point() + ggtitle("CFR Pertussis Cases")



#################### Aggregate data by state ########################
aggregate(Cases ~ State + Year, CFRwc, sum)

# check for recurring outbreaks in the same state

ggplot(CFRwc, aes(x = State)) + geom_histogram()

# you can see from the histogram that only 6 states have had 4 or more outbreaks over the 5 year span
California <- CFRwc[grep("California", CFRwc$State),]
Minnesota <- CFRwc[grep("Minnesota", CFRwc$State),]
Montana <- CFRwc[grep("Montana", CFRwc$State),]
NorthDakota <- CFRwc[grep("North Dakota", CFRwc$State),]
Washington <- CFRwc[grep("Washington", CFRwc$State),]
Wisconsin <- CFRwc[grep("Wisconsin", CFRwc$State),]


###################### Look at vaccine exemptions by state ###############
religious <- read.delim("ReligiousExempt.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
philosophical <- read.delim("PhilosophicalExempt.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

philosophical$State <- as.factor(philosophical$State)
philosophical$Year <- as.factor(philosophical$Year)
religious$State <- as.factor(religious$State)
religious$Year <- as.factor(religious$Year)

ggplot(religious, aes(x=Year, y=PubPercent)) +geom_line(aes(color=State, group = State)) + ggtitle("NonExempt Cases for Religious Purposes")
ggplot(philosophical, aes(x=Year, y=PubPercent)) +geom_line(aes(color=State, group = State)) + ggtitle("NonExempt Cases for Philosophical Purposes")


class(religious$PubPercent)


names(CFRwc)

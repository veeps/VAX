setwd("/Users/vivianpeng/git/VAX")
CFRwc <- read.delim("WhoopingCough.csv", sep= ",", header = TRUE, stringsAsFactors = FALSE)
CFRCDC<- read.delim("CFR_CDC_WC_Cases.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
require(maps)
require(maptools)
require(ggplot2)

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


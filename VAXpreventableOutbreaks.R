setwd("/Users/vivianpeng/git/VAX")
outbreaks <- read.csv("VAXpreventableOubtreaks.csv", header = TRUE, stringsAsFactors = FALSE, sep =",")
names(outbreaks)
# look for US cases
USoutbreaks <- outbreaks[grep("U.S.", outbreaks$Location),]

# select for Whooping Cough cases
WhoopingCough <- USoutbreaks[grep("Whooping Cough", USoutbreaks$Outbreak), ]
head(WhoopingCough)

# separate States from location column
require(plyr)
require(stringr)
location <- str_split_fixed(WhoopingCough$Location, " ",2)
colnames(location) <-c("Country", "State")
location<- as.data.frame(location)


# remove parenthesis from State column
a <- gsub("\\(", "", location$State)
State <- gsub("\\)", "", a)

# split city into its own column
City <- str_split_fixed(State, ", ",2)
colnames(City) <- c("State", "City")
Cities <- as.data.frame(City)

# add city and state columns back into Whooping Cough table
WhoopingCough$State <- Cities$State
WhoopingCough$City <- Cities$City
WhoopingCough <- WhoopingCough[,-3]

# Change numeric variables to numerics instead of characters
WhoopingCough$Long <- as.numeric(WhoopingCough$Long)
WhoopingCough$Lat <- as.numeric(WhoopingCough$Lat)
WhoopingCough$Cases <- as.numeric(WhoopingCough$Cases)
WhoopingCough$Fatalities <- as.numeric(WhoopingCough$Fatalities)
colnames(WhoopingCough) <- c("Category", "Outbreak", "Lat", "Long", "Date", "Cases", "Fatalities", "ImpactScale", "SourceCitation", "Source", "State", "City")
names(WhoopingCough)

# write results to file
write.table(WhoopingCough, "WhoopingCough.csv", sep=",", col.names=TRUE, row.names = FALSE)
write.table(uspop.c2, "uspop.csv", sep=",", col.names= TRUE, row.names = FALSE)



##########################################################
require(maps)
require(maptools)
require(ggplot2)
# find max and min longitude and latitude
long <- c(min(WhoopingCough$Long),max(WhoopingCough$Long) )
lat <- c(min(WhoopingCough$Lat),max(WhoopingCough$Lat) )

all_states <- map_data("state")
MapCases <- ggplot(data=WhoopingCough, aes(Long,Lat))
MapCases + geom_polygon(data=all_states, aes(x=long, y=lat, group=group, color=State), colour="darkgray", fill = "ivory") +
  geom_point(aes(size=Cases)) + xlim(long) + ylim(lat)



#Exploring CFR Whooping Cough data

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

WC <-WhoopingCough

# select for start and end dates for Date column
Dates<- str_split(WC$Date, "-", 2)

# since not all dates have a range, for the ones that do have a range, 
# I am going to replace the range with the start date value 
StartDate <- sapply(Dates, FUN=function(x) x[1])

# split Date to month and Year
MonthYear <- str_split(StartDate, "/", 2)
WhoopingCough$Month <- sapply(MonthYear, FUN= function(x) x[1])
WhoopingCough$Year <- sapply(MonthYear, FUN=function(x) x[2])
WhoopingCough$Year <- as.numeric(WhoopingCough$Year)

names(WhoopingCough)

# split data according to impact scale
epidemic <- WhoopingCough[grep("Epd", WhoopingCough$ImpactScale), ]
cluster <- WhoopingCough[grep("Clst", WhoopingCough$ImpactScale), ]
min(cluster$Cases) 

# write results to file
write.table(WhoopingCough, "WhoopingCough.csv", sep=",", col.names=TRUE, row.names = FALSE)


unique(WhoopingCough$State)
unique(WhoopingCough$Year)

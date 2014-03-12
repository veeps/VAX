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

# remove commas from columns
WhoopingCough$SourceCitation <- gsub(",", "", WhoopingCough$SourceCitation)

# change month into numerics
WhoopingCough$Month <- as.numeric(WhoopingCough$Month)

# clean up row 5
WhoopingCough$SourceCitation[5] <- "Associated Press. Children's Health - Another infant dies of whooping cough in California July 30 2010"
WhoopingCough$ImpactScale[5] <- "Isl"

# write results to file
write.table(WhoopingCough, "WhoopingCough.csv", sep=",", col.names=TRUE, row.names = FALSE)



##########################################################


# total cases per year
cases_2008 <- sum(WhoopingCough[grep("2008", WhoopingCough$Year),6])
cases_2009 <- sum(WhoopingCough[grep("2009", WhoopingCough$Year), 6])
cases_2010 <- sum(WhoopingCough[grep("2010", WhoopingCough$Year), 6])
cases_2011 <- sum(WhoopingCough[grep("2011", WhoopingCough$Year), 6])
cases_2012 <- sum(WhoopingCough[grep("2012", WhoopingCough$Year), 6])
cases_2013 <- sum(WhoopingCough[grep("2013", WhoopingCough$Year), 6])

CFRCases <- c(cases_2008, cases_2009, cases_2010, cases_2011, cases_2012, cases_2013)
Years <- c(2008, 2009, 2010, 2011,2012,2013)
TotalCasesByYear <- data.frame(Years, CFRCases)
colnames(TotalCasesByYear) <- c("Year", "CFRCases")

ggplot(TotalCasesByYear, aes(x=Years, y=log(CFRCases))) +geom_point(color = "indianred1") + ggtitle("Pertussis Cases in the US")

# read in CDC data
CDCcases <- read.table("CDC_PertussisCases.csv", header = TRUE, stringsAsFactors= FALSE,sep="," )
colnames(CDCcases) <- c("Year", "CDCCases")


# filter for cases from 2008 to 2013
CDCcases$Year
CDC2008_2013 <- CDCcases [87:92, ]

# merge CFR data with CDC data
mergedCases <- join(x=TotalCasesByYear, y= CDC2008_2013, by = "Year", match ="first")

# melt data
require(reshape)
meltMerged <- melt(mergedCases, "Year", c("CFRCases", "CDCCases"))
colnames(meltMerged) <- c("Year", "Source", "Cases")

# write results to file
write.table(meltMerged, "CFR_CDC_WC_Cases.csv", sep = ",", col.names=TRUE, row.names = FALSE)



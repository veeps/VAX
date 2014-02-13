setwd("/Users/vivianpeng/git/VAX")

require(plyr)
cases <- read.table("CDC_PertussisCases.csv", header = TRUE, stringsAsFactors= FALSE,sep="," )
colnames(cases) <- c("Year", "Cases")
as.numeric(cases$Cases)
# exploratory graphs

require(ggplot2)
ggplot(cases, aes(x=as.factor(Year), y=log(Cases))) +geom_point(color = "indianred1") + ggtitle("Pertussis Cases in the US")

cases$Cases
head(cases)
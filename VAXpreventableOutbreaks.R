setwd("/Users/vivianpeng/git/VAX")
outbreaks <- read.csv("VAXpreventableOubtreaks.csv", header = TRUE, stringsAsFactors = FALSE, sep =",")
names(outbreaks)
# look for US cases
USoutbreaks <- outbreaks[grep("U.S.", outbreaks$Location),]

head(USoutbreaks)

# select for Whooping Cough cases
WhoopingCough <- USoutbreaks[grep("Whooping Cough", USoutbreaks$Outbreak), ]
head(WhoopingCough)

path1 <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/'
Stocks <- read.csv(file = paste0(path1,"FTSE_Stocks.csv"))
path2 <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/'
Sectors <- read.csv(file = paste0(path2,"FTSE_Stocks.csv"))


Sectors$x <- as.character(Sectors$x)
Stocks$x <- as.character(Stocks$x)

## Copy sectors from known stocks
for (i in 1:nrow(Stocks)) {
  Stocks$x[i] <- unlist(strsplit(Stocks$x[i], split='.', fixed=TRUE))[1]
  if (sum(Stocks$x[i]==Sectors$x)==1) {
  Stocks[i,2] <- as.character(Sectors[Sectors$x==Stocks$x[i], 3])
  Stocks[i,3] <- Sectors[Sectors$x==Stocks$x[i], 4]
  } 
}
write.csv(Stocks, file = paste0(path1,"FTSE_Stocks.csv"),row.names=FALSE)





#####################################################################################
#####################################################################################

####### After finding the sectors (manually:::Bloomberg) assign sector type alphabetically
####### as ggplot does when coloring (consistency)
rm(list=ls())
path1 <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/'
Stocks <- read.csv(file = paste0(path1,"FTSE_Stocks_2.csv"))

for (i in 1:nrow(Stocks)) {
  if (Stocks[i,2]=='Communications') {
      Stocks[i,3] <- 1
  } else if (Stocks[i,2]=='Consumer Staples') {
      Stocks[i,3] <- 2
  } else if (Stocks[i,2]=='Consumers Discretionary') {
      Stocks[i,3] <- 3
  } else if (Stocks[i,2]=='Energy') {
    Stocks[i,3] <- 4
  } else if (Stocks[i,2]=='Financials') {
    Stocks[i,3] <- 5
  } else if (Stocks[i,2]=='Health Care') {
    Stocks[i,3] <- 6
  } else if (Stocks[i,2]=='Industrials') {
    Stocks[i,3] <- 7
  } else if (Stocks[i,2]=='Materials') {
    Stocks[i,3] <- 8
  } else if (Stocks[i,2]=='Technology') {
    Stocks[i,3] <- 9
  } else if (Stocks[i,2]=='Utilities') {
    Stocks[i,3] <- 10
  }
}
write.csv(Stocks, file = paste0(path1,"FTSE_Stocks.csv"),row.names=FALSE)



#####################################################################################
#####################################################################################

### Merge with market capitalisation

## Proper format for the names in MC
rm(list=ls())
path1 <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/'
Stocks <- read.csv(file = paste0(path1,"FTSE_Stocks.csv"))
Capit <- read.csv(file = paste0(path1,"MarketCapitalisations.csv"))
Capit$V1 <- as.character(Capit$V1)
for (i in 1:nrow(Capit)) {
  Capit$V1[i] <- unlist(strsplit(as.character(Capit$V1[i]), split='.', fixed=TRUE))[1]
}
write.csv(Capit, file = paste0(path1,"MarketCapitalisation.csv"),row.names=FALSE)
Capit2 <- Capit[, 2:3]
names(Capit2) <- c('x', 'MC')


## Merge
fullInfo <- merge(Stocks, Capit2, by = 'x')

write.csv(fullInfo, file = paste0(path1,"FTSE_Stocks.csv"),row.names=FALSE)

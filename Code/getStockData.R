

setwd('C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE')
startDate <- "2015-01-01"
# endDate <-


options(java.parameters = "-Xmx1024m")
library(XLConnect)
## Get tickers and process dataframe
allTickers <- readWorksheet(loadWorkbook('C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/YahooTickerSymbols-Jan2016.xlsx'),sheet=1)
names(allTickers) <- allTickers[3,]
allTickers <- allTickers[4:nrow(allTickers),]
## Subset UK tickers
UKtickers <- subset(allTickers, Country=='UK')

library(quantmod)  # also loads xts and TTR
library(plyr)
      # Fetch all Symbols & store only the tickers to retrieve the data
      # symbols <- stockSymbols()
      # symbols <- symbols[,1]
symbols <- UKtickers$Ticker
dataset<- xts() # Only run once


n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)
# Actual loop: 
for(i in 1:length(symbols)) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,from=startDate, src='yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from=startDate, src='yahoo')
    dataset <- merge(dataset, Cl(get(symbols[i])))
    rm(symbol)
  }
   setTxtProgressBar(pb, i)
}
df <- data.frame(dataset)
labels <- vector(mode="character", length=0)
for (i in 1:length(names(df))) {
   labels[i] <- unlist(strsplit(names(df)[i], split='.C', fixed=TRUE))[1]
}
names(df) <- labels
#    row.names(df) <- as.POSIXct(row.names(df), format= '%Y-%m-%d %H:%M:%S', tz="GMT")
df1 <- data.frame(t(df))
df2 <- data.frame(cbind(names(df), df1))
names(df2)[1] <- 'tickers'


### Get Capitalisations and convert to numeric (removing M and B factors)
what_metrics <- yahooQF("Market Capitalization")
metrics <- getQuote(paste(symbols, sep="", collapse=";"), what=what_metrics)
metrics <- data.frame(Symbol=symbols, metrics[,2:length(metrics)])
names(metrics) <- c('tickers', 'MarketCapitalisation')
metrics$MarketCapitalisation <- as.character(metrics$MarketCapitalisation)
MC <- data.frame(matrix(ncol=2, nrow=nrow(metrics)))
MC[,1] <- metrics$tickers
for (i in 1: nrow(metrics)) {
    if (grepl('M', metrics$MarketCapitalisation[i])) {
      MC[i,2] <- (as.numeric(unlist(strsplit(metrics$MarketCapitalisation[i], split='M', fixed=TRUE))[1]))*1e6
    } else if (grepl('B', metrics$MarketCapitalisation[i])) {
      MC[i,2] <- (as.numeric(unlist(strsplit(metrics$MarketCapitalisation[i], split='B', fixed=TRUE))[1]))*1e9
    } else {
      MC[i,2] <- as.numeric(metrics$MarketCapitalisation[i])
    }
}
names(MC) <- c('tickers', 'MarketCap')
### We not consider stocks without Market Capitalisation (not relevant)
MC1 <- MC[is.na(MC$MarketCap)==FALSE,]
# MC2 <- data.frame(t(MC1$MarketCap))
# names(MC2) <- as.character(MC1$tickers)



#### Merge datasets. This way we get rid of the stocks that doesnt have Market Cap.
dfStocks <-  Reduce(function(x,y) merge(x,y, by = 'Date'), list_dfs)
stockData <-  merge(MC1, df2, by='tickers')
stockData2 <- as.data.frame(t(stockData[, 2:ncol(stockData)]))

names(stockData2) <- stockData[,1]


### Reject stocks with too many N/A values
stockData3 <- stockData2[, colSums(is.na(stockData2)) < 5]

### Inner join of the observations (dates), reject rows with N/A values
stockData4 <- stockData3[rowSums(is.na(stockData3)) == 0, ]

#### filter by capitalisation > 1e9
stockData5 <- stockData4[, as.numeric(stockData4[1,])>1e9]


path <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/'
write.csv(stockData5[2:nrow(stockData5),], file= paste0(path, 'raw/stockData.csv'))
write.csv(cbind(names(stockData5),as.numeric(stockData5[1,])), file= paste0(path, '/MarketCapitalisations.csv'))


 # write.csv(cbind(1:length(names(stockData5)), names(stockData5)), file= 'FTSE_indexes.csv')

rm(list=ls())

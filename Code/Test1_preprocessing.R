# Cleaning and preprocessing Test1 data

path <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/Test1/'
subpath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/'

myList = list.files(path, pattern="*.csv")
for (i in 1:length(myList)) {
  assign(myList[i], read.csv(paste0(path,myList[i])))
}

df <-  Reduce(function(x,y) merge(x,y, by = 'Date'), 
               list(subset(AAPL.csv, select = c(Date, Close)),
                    subset(ADBE.csv, select = c(Date, Close)),
                    subset(AMZN.csv, select = c(Date, Close)),
                    subset(BRK.B.csv, select = c(Date, Close)),
                    subset(BSX.csv, select = c(Date, Close)),
                    subset(CSCO.csv, select = c(Date, Close)),
                    subset(GE.csv, select = c(Date, Close)),
                    subset(JNJ.csv, select = c(Date, Close)),
                    subset(JPM.csv, select = c(Date, Close)),
                    subset(MMM.csv, select = c(Date, Close)),
                    subset(MSFT.csv, select = c(Date, Close)),
                    subset(MTB.csv, select = c(Date, Close)),
                    subset(NKE.csv, select = c(Date, Close)),
                    subset(T.csv, select = c(Date, Close)),
                    subset(UNP.csv, select = c(Date, Close)),
                    subset(XOM.csv, select = c(Date, Close))))



stockNames <- gsub(".csv", "", myList)    #Deleting extension from names
names(df) <- c('Date', stockNames)     #Naming variables in df
Stocks <- df[2:ncol(df)]
Dates <- df[1]

StocksNormalized <- data.frame(scale(Stocks))    #Normalization of stocks


### Save dataframes
write.csv(StocksNormalized, file = paste0(subpath,"StocksNormalized.csv"),row.names=FALSE)
write.csv(Dates, file = paste0(subpath,"Dates.csv"),row.names=FALSE)
write.csv(names(Stocks), file = paste0(subpath,"Stocks.csv"),row.names=FALSE)
write.csv(Stocks, file = paste0(subpath,"StocksNotNorm.csv"),row.names=FALSE)

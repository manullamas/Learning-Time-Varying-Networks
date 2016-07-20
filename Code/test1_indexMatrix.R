
subpath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/'
Stocks <- read.csv(paste0(subpath,'Stocks.csv'))

Stocks <- cbind(c(1:16), Stocks)

write.csv(Stocks, file = paste0(path,"Stocks.csv"),row.names=FALSE)

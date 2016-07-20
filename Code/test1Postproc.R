# Stocks Postprocessing

subpath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/'
Stcks <- read.csv(paste0(subpath, 'network_1.csv'))

# Remove first column: index
Stcks <- Stcks[,-1]

Stcks <- cbind(stockNames, Stcks)
nm <- c('names', stockNames)
Prueba <- rbind(nm, Stcks)



write.csv(Prueba, file = paste0(subpath,"Network_prueba.csv"),row.names=FALSE)
1

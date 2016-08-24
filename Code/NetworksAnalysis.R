            
rm(list=ls())
op <- par()



  
                #######################################
                #######   EXPLORATORY ANALYSIS  #######
                #######################################


#        LOAD EXPLORATORY VARIABLES

  setwd('C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/networks')
  folderPath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/'

  ret <- read.csv(paste0(folderPath,'/processed/FTSE_returns.csv'))
     obserPerEpoch=20
                    #   Sectors <- read.csv(paste0(folderPath,'FTSE_Stocks.csv'))
                    #   Sectors <- Sectors[2:nrow(Sectors),]  # remove AAL, not in sample
      Sectors <- read.csv(paste0(folderPath,'FTSE_Stocks.csv'))
    
  stockNames <- read.csv(paste0(folderPath,'FTSE_indexes.csv'))
  avgDegree <- read.csv('avgDegree.csv', header = F)
  degreeMatrix <- read.csv( 'degreeMatrix.csv', header = F)
  GainLossMatrix <- read.csv('GainLoss.csv', header = F)
  
  #         LOAD ALL EPOCH'S MATRICES

  # Create lists of weighted, signed and distance matrices
  listWeights = list.files(paste0(folderPath,'networks/'), pattern='*weights.csv')
  weightedMatrices <- list()
  listSigned = list.files(paste0(folderPath,'networks/'), pattern='*signs.csv')
  signedMatrices <- list()
  listDistances = list.files(paste0(folderPath,'networks/'), pattern='dist*')
  distMatrices <- list()
  listCorrelation = list.files(paste0(folderPath,'networks/'), pattern='corr*')
  corrMatrices <- list()
  sparseMatrices <- list()
#   listCovariance = list.files(paste0(folderPath,'networks/'), pattern='covar*')
#   covMatrices <- list()
    returns <-list()
  for (t in seq_along(listWeights)) {
        returns[[t]] <- ret[((t-1)*obserPerEpoch+1):(t*obserPerEpoch),] 
        weightedMatrices[[t]] <- read.csv(file = listWeights[t], header = F)
        signedMatrices[[t]] <- read.csv(file = listSigned[t], header = F)
        # distMatrices[[t]] <- read.csv(file = listDistances[t], header = F)
        corrMatrices[[t]] <- read.csv(file = listCorrelation[t], header = F)
        # covMatrices[[t]] <- read.csv(file = listCovariance[t], header = F)
        sparseMatrices[[t]] <- replace(weightedMatrices[[t]], which(abs(weightedMatrices[[t]])<0.005, arr.ind=TRUE), 0)
  }
  
  # Define useful variables
  nStocks <- length(stockNames[,2])
  nSectors <- length(unique(Sectors$Sector.type))
  T <- length(listSigned)
  labels <- as.character(stockNames[,2])
  
  ## change the input of stockNames(use sectors dataframe so simpler)
  # stockNames <- cbind(1:length(stockNames), stockNames)
  
  
  
  
  
  
  
####################################################################################################################
####################################################################################################################
####################################################################################################################
  
                      #################################
                      ######   NETWORK DRAWING   ######
                      #################################
  
  library(igraph)
  library(network)
  library(sna)
  library(ndtv)
  library(visNetwork)
  library(ggplot2)
  source("C:/Users/Manuel/Desktop/Southampton/MasterThesis/Code/multiplot.R")
  
  
  #        CREATE igraph OBJECTS: signed, weighted and sparse nets
  
    adjacency_list_weights <- list()
    adjacency_list_signs <- list()
    adjacency_list_corr <- list()
   for (t in 1:length(weightedMatrices)) {
        adjacency_list_weights[[t]] <- graph_from_adjacency_matrix(as.matrix(weightedMatrices[[t]]), 
                                   mode = "directed", weighted = TRUE, diag = TRUE, 
                                   add.colnames = NULL, add.rownames = NA)
        adjacency_list_signs[[t]] <- graph_from_adjacency_matrix(as.matrix(signedMatrices[[t]]), 
                                 mode = "directed", weighted = NULL, diag = TRUE, 
                                 add.colnames = NULL, add.rownames = NA)
        adjacency_list_corr[[t]] <- graph_from_adjacency_matrix(as.matrix(corrMatrices[[t]]),
                                 mode = "directed", weighted = TRUE, diag = TRUE, 
                                 add.colnames = NULL, add.rownames = NA)
    #netSigns[[t]] <- simplify(adjacency_list_signs[[t]], remove.multiple = F, remove.loops = T)
  }
  netSigns <- lapply(adjacency_list_signs, function(x) simplify(x, remove.multiple = F, remove.loops = T))
  netWeights <- lapply(adjacency_list_weights, function(x) simplify(x, remove.multiple = F, remove.loops = T))
  netCorr <- lapply(adjacency_list_corr, function(x) simplify(x, remove.multiple = F, remove.loops = T))
   bound <- 0.01
   
   # To compute a suitable reject boundary for the correlation networks in order to have sparse/significant networks I first compute the 
   # quantile 95 (sparsity 95%) for each epoch and then compute the mean of all of them. This gives a uniform boundary for all networks
   # having now same scale in the weights, also (and to avoid just picking 5% highest weights, in case there is some epoch's network
   # extremely strong obscuring the rest) we compute the mean of these epoch's quantiles
   quant95 <- matrix(0, T, 1)
   for (t in 1:T) {
        quant95[t] <- quantile(E(netCorr[[t]])$weight, 0.95)   # mean(E(netCorr[[t]])$weight)
   }
   boundCorr <- mean(quant95)
  netSparse <- lapply(netWeights, function(x) delete_edges(x, E(x)[abs(weight) < bound]))
  netCorrSparse <- lapply(netCorr, function(x) delete_edges(x, E(x)[abs(weight) < boundCorr]))
  # igraph format -> http://igraph.org/r/doc/graph_from_adjacency_matrix.html
  

####################################################################################################################
  
  
  ## NET PARAMETER SETTING (NETWORKS)
  
  deg.in <- lapply(netSparse, function(x) igraph::degree(x, mode = 'in'))
  deg.out <- lapply(netSparse, function(x) igraph::degree(x, mode = 'out'))
  deg.total <- lapply(netSparse, function(x) igraph::degree(x, mode = 'total'))
  deg.corr <- lapply(netCorrSparse, function(x) igraph::degree(x, mode = 'out'))
  edge.start <- lapply(netSparse, function(x) ends(x, es=E(x), names=F)[,1])
  edge.end <- lapply(netSparse, function(x) ends(x, es=E(x), names=F)[,2])
  weights <- lapply(netSparse, function(x) (E(x)$weight))
  absWeights <- lapply(netSparse, function(x) abs(E(x)$weight))
  weights.C <- lapply(netCorrSparse, function(x) (E(x)$weight))
  #   absWeights.C <- lapply(netCorrSparse, function(x) abs(E(x)$weight))
  
  
  # Average/sd In Weight per Stock
  avgWeight.in.Stocks <- list()
  sdWeight.in.Stocks <- list()
  for (t in 1:T) {
      avgWeight.in.Stocks[[t]] <- matrix(nrow=nStocks,ncol=1)
      sdWeight.in.Stocks[[t]] <- matrix(nrow=nStocks,ncol=1)
      for (i in 1:nStocks) {
          avgWeight.in.Stocks[[t]][i] <- mean((E(netSparse[[t]])$weight)[edge.end[[t]]==i])
          sdWeight.in.Stocks[[t]][i] <- sd(((E(netSparse[[t]])$weight)[edge.end[[t]]==i]))
      }
      avgWeight.in.Stocks[[t]][is.na(avgWeight.in.Stocks[[t]])] <- 0
      sdWeight.in.Stocks[[t]][is.na(sdWeight.in.Stocks[[t]])] <- 0
  }
    

    info.stocks <- list()
    sortedDeg.in <- list()
    sortedDeg.out <- list()
    sortedDeg.total <- list()
    hubs.in <- list()
  for (t in seq_along(deg.out)) {
      #deg[[t]] <- cbind(Sectors$x, deg.total[[t]], deg.in[[t]], deg.out[[t]])
      info.stocks[[t]] <- (cbind(stockNames$FTSE100_list, Sectors$Sector.type, Sectors$Sector, data.frame(deg.total[[t]]),
                data.frame(deg.out[[t]]), data.frame(deg.in[[t]]), data.frame(deg.corr[[t]]), avgWeight.in.Stocks[[t]], sdWeight.in.Stocks[[t]]))
      names(info.stocks[[t]]) <- c('stocks', 'sector.type', 'sectors', 'deg.total', 'deg.out', 'deg.in', 'deg.corr', 'avg.weight.in', 'sd.weight.in')
      sortedDeg.in[[t]] <- info.stocks[[t]][order(deg.in[[t]], decreasing=T),]
      sortedDeg.out[[t]] <- info.stocks[[t]][order(deg.out[[t]], decreasing=T),]
      sortedDeg.total[[t]] <- info.stocks[[t]][order(deg.total[[t]], decreasing=T),]
      hubs.in[[t]] <- sortedDeg.in[[t]][sortedDeg.in[[t]]$deg.in > nrow(info.stocks[[t]])*0.5,]
  }
     ### calculate sum of weights also here!
    
    
    
    
    
  ##       Set Vertex/Edges attributes & Layouts
  library(RColorBrewer)
  col = list(color = brewer.pal(11, 'Paired'))
  col.GL <- colorRampPalette(c("dark red", '#91cf60'))
  pal.GLsimple <- c('#fc8d59', 'white', '#91cf60')
#   pal.GL <- col.GL(10)

    edge.col.out <- list()
    edge.col.in <- list()
    edge.sector.out <- list()
    edge.sector.in <- list()
#     edge.col.out.C <- list()
#     edge.col.in.C <- list()
    TESLA.fr <- list()
    TESLA.fr.grid <- list()
    TESLA.kk.abs <- list()
    TESLA.kk3 <- list()
    TESLA.MDS <- list()
#     TESLA.MDSdist <- list()
    TESLA.MDScorr <- list()
    Reingold <- list()
    TESLA.lgl <- list()
    TESLA.lgl2 <- list()
    TESLA.drl <- list()
    ###################################drl <- list()
#     CORR.fr <- list()
#     CORR.fr.abs <- list()
#     CORR.kk.abs <- list()
#     CORR.MDS <- list()
#     CORR.MDSdist <- list()
      kmeans.init <-list()
      library(cluster) 

  for (t in seq_along(netSparse)) {
    
      print(paste0('t = ', t))
        
    
    ### Vertices
        V(netSparse[[t]])$sectors <- Sectors$Sector
        V(netSparse[[t]])$sector.type <- Sectors$Sector.type
        V(netSparse[[t]])$color <- col$color[Sectors$Sector.type]
        GainLoss.order <- findInterval(GainLossMatrix[,t], sort(GainLossMatrix[,t]))
        V(netSparse[[t]])$col.contagion <- pal.GLsimple[sign(GainLossMatrix[,t])+2]
        V(netSparse[[t]])$col.contagion2 <- col.GL(12)[as.numeric(cut(GainLossMatrix[,t], breaks=seq(from=-max(abs(GainLossMatrix[,t])), to=max(abs(GainLossMatrix[,t])), length.out = 13)), include.lowest=TRUE)]
        V(netSparse[[t]])$col.contagion.homog <- col.GL(99)[as.numeric(cut(GainLossMatrix[,t], breaks=seq(from=-max(abs(GainLossMatrix)), to=max(abs(GainLossMatrix)), length.out = 99)), include.lowest=TRUE)]
        #         V(netSparse)[[t]]$col.contagion <- pal.GL(nrow(GainLossMatrix))[GainLoss.order]
        #V(netSparse[[t]])$size <- 3*log(deg.out[[t]])
        #V(netSparse[[t]])$hubs <-
        
#         V(netCorrSparse[[t]])$sectors <- Sectors$Sector
#         V(netCorrSparse[[t]])$sector.type <- Sectors$Sector.type
#         V(netCorrSparse[[t]])$color <- col$color[Sectors$Sector.type]
#   
        
    ### Edges
        E(netSparse[[t]])$weights.abs <- abs(E(netSparse[[t]])$weight) ### absWeights
         edge.col.out[[t]] <- V(netSparse[[t]])$color[edge.start[[t]]]
         edge.col.in[[t]] <- V(netSparse[[t]])$color[edge.end[[t]]]
         edge.sector.out[[t]] <- Sectors$Sector[edge.start[[t]]]
         edge.sector.in [[t]] <- Sectors$Sector[edge.end[[t]]]
        E(netSparse[[t]])$col.out <- edge.col.out[[t]]
        E(netSparse[[t]])$col.in <- edge.col.in[[t]]
        edge.col.contagion <- V(netSparse[[t]])$col.contagion[edge.start[[t]]]
        E(netSparse[[t]])$col.contagion <- edge.col.contagion
        edge.col.contagion2 <- V(netSparse[[t]])$col.contagion2[edge.start[[t]]]
        E(netSparse[[t]])$col.contagion2 <- edge.col.contagion2
        edge.col.contagion.homog <- V(netSparse[[t]])$col.contagion.homog[edge.start[[t]]]
        E(netSparse[[t]])$col.contagion.homog <- edge.col.contagion.homog
        
        
#         E(netCorrSparse[[t]])$weights.abs <- abs(E(netCorrSparse[[t]])$weight) ### absWeights
#           edge.col.out.C[[t]] <- V(netCorrSparse[[t]])$color[edge.start.C[[t]]]
#           edge.col.in.C[[t]] <- V(netCorrSparse[[t]])$color[edge.end.C[[t]]]
#         E(netCorrSparse[[t]])$col.out <- edge.col.out.C[[t]]
#         E(netCorrSparse[[t]])$col.in <- edge.col.in.C[[t]]

        weights.out <- data.frame(matrix(ncol=1, nrow=nStocks))
        weights.in <- data.frame(matrix(ncol=1, nrow=nStocks))
        weights.corr <- data.frame(matrix(ncol=1, nrow=nStocks))
#         weights.out.C <- data.frame(matrix(ncol=1, nrow=nStocks))
#         weights.in.C <- data.frame(matrix(ncol=1, nrow=nStocks))
        
        for (n in 1:nStocks){
            weights.out[n,1] <- c(info.stocks$stocks[n], sum(weights[[t]][edge.start[[t]] == n]))
            weights.in[n,1] <- c(info.stocks$stocks[n], sum(weights[[t]][edge.end[[t]] == n]))
            weights.corr[n,1] <- c(info.stocks$stocks[n], sum(weights.C[[t]][edge.start[[t]] == n]))

            }
        info.stocks[[t]] <- cbind(info.stocks[[t]], weights.out, weights.in, weights.corr)
        names(info.stocks[[t]]) <- c('stocks', 'sector.type', 'sectors', 'deg.total', 'deg.out', 'deg.in', 'deg.corr', 'weights.out', 'weights.in', 'weights.corr')
  
    
    ### Layouts
        TESLA.fr[[t]] <- layout_with_fr(netSparse[[t]])#, weights = Weights[[t]])
        Reingold[[t]] <- layout.reingold.tilford(netSparse[[t]], circular=T)
        TESLA.fr.grid[[t]] <- layout_with_fr(netSparse[[t]], grid='grid')
        TESLA.kk.abs[[t]] <- layout_with_kk(netSparse[[t]], weights = absWeights[[t]])
        #kk3[[t]] <- layout_with_kk(netSparse[[t]], weights = absWeights[[t]], dim = 3)
        TESLA.MDS[[t]] <- layout_with_mds(netSparse[[t]])
        # TESLA.MDSdist[[t]] <- layout_with_mds(netSparse[[t]], dist = as.matrix(distMatrices[[t]]))
        TESLA.MDScorr[[t]] <- layout_with_mds(netSparse[[t]], dist = as.matrix(corrMatrices[[t]]))
#         TESLA.lgl[[t]] <- layout_with_lgl(netSparse[[t]])
#         TESLA.lgl2[[t]] <- layout_with_lgl(netSparse[[t]], maxiter=150, maxdelta=nStocks, area=nStocks^2, repulserad=nStocks^4)
#         TESLA.drl[[t]] <- layout_with_drl(netSparse[[t]])
#         fr_grid[[t]] <- 
#         CORR.fr[[t]] <- layout_with_fr(netCorrSparse[[t]])#, weights = Weights[[t]])
#         CORR.fr.abs[[t]] <- layout_with_fr(netCorrSparse[[t]], weights = absWeights.C[[t]])
#         CORR.kk.abs[[t]] <- layout_with_kk(netCorrSparse[[t]], weights = absWeights.C[[t]])
#         CORR.MDS[[t]] <- layout_with_mds(netCorrSparse[[t]])
#         CORR.MDSdist[[t]] <- layout_with_mds(netCorrSparse[[t]], dist = as.matrix(distMatrices[[t]]))

  }
    


  



  
  ####################################################################################################################
  ####################################################################################################################
  ####################################################################################################################
  
  
                     ############################
                     ######    ANALYSIS    ######
                     ############################
  
  
  # Create analysis variables
  
  # info.stocks <- cbind(stockNames$FTSE100_list, Sectors[,3], data.frame(deg.total), data.frame(deg.out), data.frame(deg.in))  # avgDegree???
  # names(info.stocks) <- c('stocks', 'sectors', 'deg.total', 'deg.out', 'deg.in')
  maxValues <- lapply(weightedMatrices, function(x) max(x))
  minValues <- lapply(weightedMatrices, function(x) min(x))
  means <- lapply(weightedMatrices, function(x) mean(as.matrix(x)))
  sds <- lapply(weightedMatrices, function(x) sd(as.matrix(x)))
  info.epochs <- cbind(1:T, maxValues, minValues, means, sds)
  
  # matplot(info.stocks, type = c("b"), pch=1, col = 1:3, main = 'Network degrees')
  # text(1:nStocks, info.stocks$deg.total, labels = info.stocks$stocks, col = col$color[Sectors$Sector.type])
  # legend(x = 60, y = 95, legend = c('deg.out','deg.in','deg.total'), col=1:3, pch=1, pt.cex=2, cex=.8, ncol=1, y.intersp = 0.2)

  ###  cent <- lapply(netSparse, function(x) centralization.degree(x)$centralization)
  # plot(1:length(maxValues), unlist(maxValues), xlab='epoch', ylab='weight', main='Max Weights')
  # plot(1:length(minValues), unlist(minValues), xlab='epoch', ylab='weight', main='Min Weights')
  # plot(1:length(means), unlist(means), xlab='epoch', ylab='weight', main='mean weights')

  
  
  
  #---------------------#
  # ANALYSIS BY SECTORS #
  #---------------------#
  
  sectors.degOut <- lapply(info.stocks, function(x) aggregate(x$deg.out, by=list(Category=x$sectors), FUN=sum))
  sectors.degIn <- lapply(info.stocks, function(x) aggregate(x$deg.in, by=list(Category=x$sectors), FUN=sum))
  sectors.weightOut <- lapply(info.stocks, function(x) aggregate(x$weights.out, by=list(Category=x$sectors), FUN=sum))
  sectors.weightIn <- lapply(info.stocks, function(x) aggregate(x$weights.in, by=list(Category=x$sectors), FUN=sum))
  info.sectors <- list()
    for (t in 1:T) {
       info.sectors[[t]] <- cbind(sectors.degOut[[t]], sectors.degIn[[t]]$x, sectors.weightOut[[t]]$x, sectors.weightIn[[t]]$x)
       names(info.sectors[[t]]) <- c('sector', 'deg.out', 'deg.in', 'weight.out', 'weight.in')
     }
  ids <- info.stocks[[1]]$sectors
  countSect <- data.frame(table(ids))
  
  sectors <- list()
    for (n in seq_along(sectors.degOut[[t]]$Category)) {
      sectors[[n]] <- data.frame(matrix(ncol=11, nrow=T))
      for (t in 1:T) {
        sectors[[n]][t,] <- c(t, sectors.degOut[[t]]$x[n], sectors.degIn[[t]]$x[n], sectors.weightOut[[t]]$x[n], sectors.weightOut[[t]]$x[n]/countSect[n,2], sectors.weightOut[[t]]$x[n]/sectors.degOut[[t]]$x[n], sectors.weightIn[[t]]$x[n], sectors.weightIn[[t]]$x[n]/countSect[n,2], sectors.weightIn[[t]]$x[n]/sectors.degOut[[t]]$x[n],sectors.degOut[[t]]$x[n]/countSect$Freq[n], sectors.degIn[[t]]$x[n]/countSect$Freq[n])
        names(sectors[[n]]) <- c('epoch', 'deg.out', 'deg.in', 'weight.out', 'avg.weight.out.links', 'avg.weight.out.edges','weight.in', 'avg.weight.in.links', 'avg.weight.in.edges', 'meanDeg.out', 'meanDeg.in')
      }
    }
  names(sectors) <- info.sectors[[1]]$sector

#     ggplot(info.sectors[[t]], aes(x = sector, y = deg.in, fill = sector)) + geom_bar(stat = 'identity') +
#       theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
#       theme_bw() + labs(x = '', y = 'Degree In') + ggtitle('Degree by Sectors')
#       + theme(legend.position="none", axis.text.y = element_text(face=NULL, size=6, angle=0))
#     ggplot(info.sectors[[t]], aes(x = sector, y = deg.out, fill = sector)) + geom_bar(stat = 'identity') +
#       theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
#       theme_bw() + labs(x = '', y = 'Degree Out') + ggtitle('Degree by Sectors')
#       + theme(legend.position="none", axis.text.y = element_text(face=NULL, size=6, angle=0))
#     ggplot(info.sectors[[t]], aes(x = sector, y = weight.in, fill = sector)) + geom_bar(stat = 'identity') +
#       theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
#       theme_bw() + labs(x = '', y = 'Weights In') + ggtitle('Weights by Sectors')
#       + theme(legend.position="none", axis.text.y = element_text(face=NULL, size=6, angle=0))
#     ggplot(info.sectors[[t]], aes(x = sector, y = weight.out, fill = sector)) + geom_bar(stat = 'identity') +
#       theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
#       theme_bw() + labs(x = '', y = 'Weights Out') + ggtitle('Weights by Sectors')
#       + theme(legend.position="none", axis.text.y = element_text(face=NULL, size=6, angle=0))
#     
  

  
#------------------------------------------------------------------------------------------    
  
  
  #-------------------------------#
  # ANALYSIS OF INDIVIDUAL STOCKS #
  #-------------------------------#
  op <- par()
  # on.exit(par(op))
  # distribution of plots in a grid
  # par(mfrow=c(2,2), mar=c(0,0,0,0))
      library(ggplot2)
      library(grid)
      source("C:/Users/Manuel/Desktop/Southampton/MasterThesis/Code/multiplot.R")
      source("C:/Users/Manuel/Desktop/Southampton/MasterThesis/Code/gg_color_hue.R")
      pdf("Analysis_plots.pdf")
  
  for (t in seq_along(netSparse)) {
  
    h1 <- qplot(deg.in[[t]],
          geom="histogram",
          binwidth = 1,  
          main = paste0('t = ',t), 
          xlab = "Deg.in")
    h2 <- qplot(weights[[t]],
                geom="histogram",
                binwidth = 0.2,
                xlab = "Weights") +
          geom_vline(data=hubs.in[[t]], aes(xintercept=avg.weight.in), color='red', show.legend=TRUE) +
          geom_text(aes(hubs.in[[t]]$avg.weight.in, 200 ,label = hubs.in[[t]]$stocks, vjust=1, hjust=0))
          #main = paste0('t = ',t), 
          #fill=I("blue"), 
          #col=I("red"), 
          #alpha=I(.2)
    #text(1:nStocks, deg.in[[t]]+2, labels = Sectors$x, col = 'black')
    dIn <-   ggplot(info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
                         y = deg.in, fill = sectors)) + geom_bar(stat = 'identity') +
                         theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                         theme_bw() + labs(x = '', y = 'Degree In') + theme(legend.position="none",
                         axis.text.y =element_blank()) +  ###element_text(face=NULL, size=6, angle=0)) +
                         geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3)
    dOut <-   ggplot(info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
                         y = deg.out, fill = sectors)) + geom_bar(stat = 'identity') +
                         theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                         theme_bw() + labs(x = '', y = 'Degree Out') + theme(legend.position="none",
                         axis.text.y =element_blank()) +
                         geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3) 
    wIn <-   ggplot(info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
                         y = weights.in, fill = sectors)) + geom_bar(stat = 'identity') +
                         theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                         theme_bw() + labs(x = '', y = 'Weights In') + theme(legend.position="none",
                         axis.text.y =element_blank()) +
                         geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3)
    wOut <-   ggplot(data= info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
                        y = weights.out, fill = sectors)) + geom_bar(stat = 'identity') +
                        theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                        theme_bw() + labs(x = '', y = 'Weights Out') + theme(legend.position="none",
                        axis.text.y =element_blank()) +
                        geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3)
    dSin <-   ggplot(info.sectors[[t]], aes(x = sector, y = deg.in, fill = sector)) + geom_bar(stat = 'identity') +
                        theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                        theme_bw() + labs(x = '', y = 'Degree In') + ggtitle('Degree by Sectors') +
                        theme(legend.position="none")
    dSout <-  ggplot(info.sectors[[t]], aes(x = sector, y = deg.out, fill = sector)) + geom_bar(stat = 'identity') +
                        theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                        theme_bw() + labs(x = '', y = 'Degree Out') + ggtitle('Degree by Sectors') +
                        theme(legend.position="none")
    wSin <-   ggplot(info.sectors[[t]], aes(x = sector, y = weight.in, fill = sector)) + geom_bar(stat = 'identity') +
                        theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                        theme_bw() + labs(x = '', y = 'Weights In') + ggtitle('') +
                        theme(legend.position="none")
    wSout <-  ggplot(info.sectors[[t]], aes(x = sector, y = weight.out, fill = sector)) + geom_bar(stat = 'identity') +
                        theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
                        theme_bw() + labs(x = '', y = 'Weights Out') + ggtitle('') +
                        theme(legend.position="none")
                        
    multiplot(h1, h2) # , layout = as.matrix(c(1,2,3,3), nrow=2, byrow=TRUE))
    multiplot(dIn, dOut, dSin, dSout, cols = 2)
    multiplot(wIn, wOut, wSin, wSout, cols = 2)
    
  }
  dev.off()
  
  

#------------------------------------------------------------------------------------------    
  
  
  ### create   info.epochs <- cbind(1:T, maxValues, minValues, means, sds) dataframe (as info.sto)

  
  #--------------------------------#
  # ANALYSIS OF THE GLOBAL NETWORK #
  #--------------------------------#
  
  
    pdf('AnalysisOverTime.pdf')
### Plot: avg Degree Out of the sectors and network over time
    sumDeg.out <- sapply(info.stocks, function(x) sum(x$deg.out))
    meanDeg.out <- sapply(info.stocks, function(x) mean(x$deg.out))
    sdsDeg.out <- sapply(info.stocks, function(x) sd(x$deg.out))
    meanWeight.out <- sapply(info.stocks, function(x) mean(x$weights.out))
    sdsWeight.out <- sapply(info.stocks, function(x) sd(x$weights.out))
    
    dfDeg <- data.frame(cbind(1:T, sumDeg.out, meanDeg.out, sdsDeg.out, meanWeight.out, sdsWeight.out))
    names(dfDeg) <- c('epoch', 'sumDeg.out', 'meanDeg.out', 'sdsDeg.out', 'meanWeight.out', 'sdsWeight.out')
        pl1 <- ggplot(data=dfDeg, aes(x=epoch, y=meanDeg.out), colour='black') + geom_errorbar(aes(ymin=meanDeg.out-sdsDeg.out, ymax=meanDeg.out+sdsDeg.out), width=0.25) +
               geom_line() + geom_point() + ggtitle('Average Out Degree') + ylab('Avg. Degree')
        df <- cbind(sector=rep(names(sectors),sapply(sectors,nrow)),do.call(rbind,sectors))
         ggplot() + geom_line(data=df, aes(x=epoch,y=meanDeg.out, color=sector)) + scale_colour_hue(c=45, l=80) + ggtitle('Mean Degree Out per Stock') + ylab('Degree Out')
         ggplot() + geom_line(data=df, aes(x=epoch,y=meanDeg.in, color=sector)) + scale_colour_hue(c=45, l=80) + ggtitle('Mean Degree In per Stock') + ylab('DegreeIn')
         
### Plot: variation of Weights Out of the sectors and network over time
#         pl2 <- ggplot(data=dfDeg, aes(x=epoch, y=meanWeight.out), colour='black') + geom_errorbar(aes(ymin=meanWeight.out-sdsWeight.out, ymax=meanWeight.out+sdsWeight.out), width=0.25) +
#             geom_line() + geom_point() + ggtitle('Average Out Weight') + ylab('Avg. Weight')
        ggplot() +geom_line(data=df, aes(x=epoch,y=avg.weight.out.links, color=sector)) + #scale_colour_hue(c=45, l=80) +
               ggtitle('Avg. Weight Out per stock')
        ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.out.edges, color=sector)) + #scale_colour_hue(c=45, l=80) +
               ggtitle('Avg. weight per edge')
         ggplot() + geom_line(data=df, aes(x=epoch,y=weight.out, color=sector)) + #scale_colour_hue(c=45, l=80) + 
               ggtitle('Total Weight Out')
         ggplot() + geom_line(data=df, aes(x=epoch,y=weight.in, color=sector)) + #scale_colour_hue(c=45, l=80) 
               ggtitle('Total Weight In')
         ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.in.links, color=sector)) + #scale_colour_hue(c=45, l=80) + 
               ggtitle('Avg. Weight In per Stock')
         ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.in.edges, color=sector)) + #scale_colour_hue(c=45, l=80) + 
               ggtitle('Avg. Weight In per edge')
         
         
      dev.off()
      
      
      
      
### Heatmap: Sectors relationships
        library(plyr)
        edges.info <- list()
        sectConnectivity <- list()
        for (t in 1:T) {
          edges.info[[t]] <- cbind(data.frame(edge.start[[t]], as.character(info.stocks[[t]]$stocks[edge.start[[t]]]), info.stocks[[t]]$sectors[edge.start[[t]]], info.stocks[[t]]$sector.type[edge.start[[t]]], edge.end[[t]], info.stocks[[t]]$stocks[edge.end[[t]]], info.stocks[[t]]$sectors[edge.end[[t]]], info.stocks[[t]]$sector.type[edge.end[[t]]]))
          names(edges.info[[t]]) <- c('edge.start', 'stock.start', 'sector.start', 'sectorType.start', 'edge.end', 'stock.end', 'sector.end', 'sectorType.end')  
          links <- count(edges.info[[t]],vars = c("sectorType.start","sectorType.end"))
          totalEdges <- nrow(edges.info[[1]])
          sectConnectivity[[t]] <- data.frame(matrix(ncol=nSectors,nrow=nSectors))
            for (n in 1:nSectors) {
                for (m in 1:nSectors) {
                  freq <- subset(links, sectorType.start==n & sectorType.end==m)$freq
                  if (length(freq)==0) {
                    sectConnectivity[[t]][n,m] <- 0
                  } else if (length(freq)==1) {
                      sectConnectivity[[t]][n,m] <- subset(links, sectorType.start==n & sectorType.end==m)$freq
                  }
                }
            }
          names(sectConnectivity[[t]]) <- c('Communications', 'Consumer Staples', 'Consumers Discretionary', 'Energy', 'Financials', 'Health Care', 'Industrials', 'Materials', 'Technology', 'Utilities')
          row.names(sectConnectivity[[t]]) <- c('Communications', 'Consumer Staples', 'Consumers Discretionary', 'Energy', 'Financials', 'Health Care', 'Industrials', 'Materials', 'Technology', 'Utilities')
          sectConnectivity[[t]] <- as.matrix(sectConnectivity[[t]])
          }

        pdf('sectorConnectivity.pdf')
        library(reshape2)
#         palf <- colorRampPalette(c('white', 'dark blue'))
        for (t in 1:T) {
           heatmap(sectConnectivity[[t]], Rowv = NA, Colv = NA, col = palf(10), scale="none", margins=c(10,10), ylab='Edge Start', xlab='Edge End', main= paste0('t = ', t))
          melted_sectCon <- melt(sectConnectivity[[t]])
          ggplot(melted_sectCon, aes(x=Var1, y=Var2)) + geom_tile(aes(fill = value), colour = "white") + 
                scale_fill_gradient(low = "white", high = "steelblue") +
                ggtitle('Links between Sectors') + ylab('Edge end') + xlab('Edge start')
          
          ####### same plot with weights??
          }
        dev.off()
        
        
        
                #       sectors2 <- list()
#       sectors2 <- sectors
#       sectors2[[12]] <- dfDeg
#       names(sectors2)<- c(names(sectors), 'network') 
#       pDegEpochNorm <- ggplot() + geom_line(data=df, aes(x=epoch,y=deg.out.norm, color=sector))
#       
# put all in the same dataframe to group by id of list!

      

# ### Degree evolution of all stocks
#   library("reshape2")
#   library('gridExtra')
#   library('grid')
#   pdf('degreeStocks.pdf')
#   pltList.deg <- list()
#       for (n in 1:nStocks) {
#         # df <- data.frame(cbind(1:T, info.stocks[[t]]$deg.out, info.stocks[[t]]$deg.in, info.stocks[[t]]$weights.out, info.stocks[[t]]$weights.in))
#          degO <- unlist(lapply(info.stocks,"[",n,5,drop=FALSE))
#          degI <- unlist(lapply(info.stocks,"[",n,6,drop=FALSE))
#          weO <- unlist(lapply(info.stocks,"[",n,7,drop=FALSE))
#          weI <- unlist(lapply(info.stocks,"[",n,8,drop=FALSE))
#          dfD <- data.frame(cbind(1:T, degO, degI, weO, weI))
#          names(dfD) <- c('epoch', 'degO', 'degI', 'weightsO', 'weightsI')
#          
#          pltList.deg[[n]] <- ggplot() + 
#            geom_line(data=dfD, aes(x = epoch, y = degI, color = "red")) +
#            geom_line(data=dfD, aes(x = epoch, y = degO, color = "blue"))  +
#            xlab('epoch') + ylab('Degree') + ggtitle(info.stocks[[t]]$stocks[n]) +
#            theme(legend.position="none")
#            # scale_colour_discrete(breaks=c("blue", "red"),labels=c("deg.in", "deg.out"))
#       }
#      do.call(grid.arrange,pltList.deg[1:20])
#      do.call(grid.arrange,pltList.deg[21:40])
#      do.call(grid.arrange,pltList.deg[41:60])
#      do.call(grid.arrange,pltList.deg[61:85])
#      dev.off()
# #     dfDlong <- melt(dfD, id="epoch")  # convert to long format
# #     ggplot(dfDlong, aes(x=epoch, y=value, colour=variable)) +
# #       geom_line()
#------------------------------------------------------------------------------------------    

  
  
 ### PCA analysis
     ir.pca <- prcomp(ret, center = TRUE,  scale. = TRUE)
     print(ir.pca)
     plot(ir.pca, type = "l")
     summary(ir.pca)
          # use it to reduce dimensionality??
     
### k-means analysis
     
     pdf('kmeans.pdf')
        wss <- (nrow(ret)-1)*sum(apply(ret,2,var))
        for (i in 2:15) wss[i] <- sum(kmeans(ret, centers=i)$withinss)
          plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
          kmeans.init[[t]] <- kmeans(ret, centers=3)
          clusplot(ret, kmeans.init[[t]]$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main='kmeans (all returns)')#, xlim=c(-15,12), ylim=c(-5, 5))
     dev.off()
  
     
     
### Hierarchichal clustering
     library(ggdendro)
     library(ape)
     library(dendextend)
     pdf('HierarchicalClustering.pdf')
     for (t in 1:T) {
     d <- dist(t(returns[[t]]), method = "euclidean")
     fit <- hclust(d, method="ward.D")
     dend <- as.dendrogram(fit)
     clustSectors <- info.stocks[[t]]$sector.type
          hc2 <- plot(dend, main=paste0('t = ', t), cex.axis=0.2)#, horiz=TRUE) # display dendogram
          groups <- cutree(fit, k=5)
          # draw dendogram with red borders around the 5 clusters 
          rect.hclust(fit, k=5, border="red")
          hc1 <- plot(as.phylo(fit), type = "fan",tip.color = col$color[clustSectors])

#           dhc <- as.dendrogram(fit)
#           # Rectangular lines
#           ddata <- dendro_data(dhc, type = "rectangle")
#           hc2 <- ggplot(segment(ddata)) + 
#               geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
#               geom_text(segment(ddata), aes(x=x, y=y, label=info.stocks[[1]]$stocks, hjust=0,color=info.stocks[[1]]$sectors, size=3)) +
#               coord_flip() + 
#               scale_y_reverse(expand = c(0.2, 0))
#           
#           hc3 <- ggplot(as.ggdend(dend)) + 
#                 scale_y_reverse(expand = c(0.2, 0)) +
#                 coord_polar(theta="x")
#           
#           
#           hc3 <- plot(as.phylo(fit),tip.color = col$color[clustSectors])
          
#           multiplot(hc1, hc2, cols=2)
     }
     dev.off()
     

  ##todo
  
  #  analysis by sectors (relative interlinks and so)
  #  boxplots of weights (by sectors)
  #  boxplot of weights (one for each t)
  #  boxplot of degree (one for each t)
   # timeline of the degrees and weights of stocks and sectors (so ploted vs t)
     
     
     
     

     
     
     
     #       pal <- scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1)
     #       col2 = pal(11)
     
     
    
     
     
     
     
     
     
     
     
     
     ###### GEPHI test
     library(rgexf)
     for (t in 1:T) {
     nod <- data.frame(cbind(V(netSparse[[t]]), as.character(V(netSparse[[t]]))))
     edg <- t(Vectorize(get.edge, vectorize.args='id')(netSparse[[t]], 1:ecount(netSparse[[t]])))
     write.gexf(nodes = nod, edges = edg, edgesWeight = E(netSparse[[t]])$weight, defaultedgetype = "directed", output = paste0('t',t,'.gexf'))
                #, nodesAtt = nodes_att, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, 
     routput = "lesmis.gexf")
     }
     write.gexf(nod,edg)
     
     
     
     ecount(netSparse[[24]])
     
     
     #######prueba de dynamic networks (animations)
     library(ndtv)
     library(intergraph)
     nett <- lapply(netSparse2, function(x) asNetwork(x, directed=TRUE))
     tnet<-networkDynamic(network.list=nett)
     render.d3movie(tnet, vertex.col='color', edge.col='gray', displaylabels=FALSE)
#      render.animation(tnet)
#      ani.replay()

     
     
     
     
     
     
     
     
     
     
      
     
     ######## Network measures
     
     netprueba2 <- netSparse[[1]]
     E(netprueba2)$weight <- absWeights[[1]]
     incloseness <- igraph::closeness(netprueba2, mode='in')
     clos <- data.frame(cbind(as.character(info.stocks[[1]]$stocks), as.character(info.stocks[[1]]$sectors), incloseness))
     names(clos) <- c('stocks', 'sectors', 'incloseness')
     ggplot(clos, aes(x=stocks, y=incloseness, color=sectors)) + geom_point() + coord_flip()
      
          
     betweenness <- igraph::betweenness(netSparse[[1]])
     betweenness
     plot(1:85, betweenness)
     
     netUndirected <- as.undirected(netSparse[[1]], mode='collapse')
     ev_obj_net <- igraph::evcent(netUndirected)
     eigen_net <- ev_obj_net$vector
     prueba <- data.frame(cbind(as.character(info.stocks[[1]]$stocks), eigen_net))
     names(prueba) <- c('stocks', 'eigencentrality')
     ggplot(prueba, aes(x=stocks, y=eigen_net), color='black') + geom_point() + coord_flip()
     ## tb podemos calcular eigencentrality In and Out, aqui hemos transformado la net en undirected
     plot(netSparse[[1]])
     
     
     
     library('qgraph')
     netprueba <- qgraph(sparseMatrices[[1]])
     cent <- centrality(netprueba)
     
     
     dev.off()
     ?closeness
     
     plot(1:85, eigen_net)
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     on.exit(par(op))
     # distribution of plots in a grid
     par(mfrow=c(2,2), mar=c(0,0,0,0)) # plot four figures - 2 rows, 2 columns
     par(mfrow=c(1,1))
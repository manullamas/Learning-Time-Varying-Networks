            
  rm(list=ls())
  op <- par("mypar"=myvalue)
  on.exit(par(op))
  # distribution of plots in a grid:
  par(mfrow=c(2,2), mar=c(0,0,0,0)) # plot four figures - 2 rows, 2 columns
  par(mfrow=c(1,1))

  
                #######################################
                #######   EXPLORATORY ANALYSIS  #######
                #######################################


#        LOAD EXPLORATORY VARIABLES

setwd('C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/networks')
folderPath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/'

SectorsFull <- read.csv(paste0(folderPath,'FTSE_Stocks.csv'))
Sectors <- SectorsFull[2:nrow(SectorsFull),]  # remove AAL, not in sample
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
for (t in seq_along(listWeights)) {
  weightedMatrices[[t]] <- read.csv(file = listWeights[t], header = F)
  signedMatrices[[t]] <- read.csv(file = listSigned[t], header = F)
  distMatrices[[t]] <- read.csv(file = listDistances[t], header = F)
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


#        CREATE igraph OBJECTS: signed, weighted and sparse nets

  adjacency_list_weights <- list()
  adjacency_list_signs <- list()
for (t in 1:length(weightedMatrices)) {
  adjacency_list_weights[[t]] <- graph_from_adjacency_matrix(as.matrix(weightedMatrices[[t]]), 
                                 mode = "directed", weighted = TRUE, diag = TRUE, 
                                 add.colnames = NULL, add.rownames = NA)
  adjacency_list_signs[[t]] <- graph_from_adjacency_matrix(as.matrix(signedMatrices[[t]]), 
                               mode = "directed", weighted = NULL, diag = TRUE, 
                               add.colnames = NULL, add.rownames = NA)
  #netSigns[[t]] <- simplify(adjacency_list_signs[[t]], remove.multiple = F, remove.loops = T)
  
}
netSigns <- lapply(adjacency_list_signs, function(x) simplify(x, remove.multiple = F, remove.loops = T))
deg.total <- lapply(adjacency_list_weights, function(x) simplify(x, remove.multiple = F, remove.loops = T))
    bound <- 0.01
netSparse <- lapply(deg.total, function(x) delete_edges(x, E(x)[abs(weight) < bound]))
# igraph format -> http://igraph.org/r/doc/graph_from_adjacency_matrix.html



####################################################################################################################


##        PARAMETER SETTING (NETWORKS)

# net <- list(netSparse, netWeights)
# ecount(netSigns)
# ecount(netWeights)
# igraph::list.vertex.attributes(netSparse)
# igraph::list.edge.attributes(netSparse)
# ecount(netWeights)
# ecount(netSparse)
# ecount(netSigns)

deg.out <- lapply(netSparse, function(x) igraph::degree(x, mode = 'out'))
deg.in <- lapply(netSparse, function(x) igraph::degree(x, mode = 'in'))
deg.total <- lapply(netSparse, function(x) igraph::degree(x, mode = 'total'))

  info.stocks <- list()
  sortedDeg.in <- list()
  sortedDeg.out <- list()
  sortedDeg.total <- list()
  hubs.in <- list()
for (t in seq_along(deg.out)) {
  info.stocks[[t]] <- (cbind(stockNames$FTSE100_list, Sectors[,3], data.frame(deg.total[[t]]),
                                data.frame(deg.out[[t]]), data.frame(deg.in[[t]])))
  names(info.stocks[[t]]) <- c('stocks', 'sectors', 'deg.total', 'deg.out', 'deg.in')
  
  sortedDeg.in[[t]] <- info.stocks[[t]][order(deg.in[[t]], decreasing=T),]
  sortedDeg.out[[t]] <- info.stocks[[t]][order(deg.out[[t]], decreasing=T),]
  sortedDeg.total[[t]] <- info.stocks[[t]][order(deg.total[[t]], decreasing=T),]
  hubs.in[[t]] <- sortedDeg.in[[t]][sortedDeg.in[[t]]$deg.in > mean(sortedDeg.in[[t]]$deg.in),]
}
# deg.out1 <- igraph::degree(netSparse[[1]], mode = 'out')
# deg.in <- igraph::degree(netSparse, mode = 'in')
# deg.total <- igraph::degree(netSparse, mode = 'total')
# info.stocks <- cbind(stockNames$FTSE100_list, Sectors[,3], data.frame(deg.total[[1]]), data.frame(deg.out[[1]]), data.frame(deg.in[[1]]))  # avgDegree???
# names(info.stocks) <- c('stocks', 'sectors', 'deg.total', 'deg.out', 'deg.in')
# sortedDeg <- info.stocks[order(deg.in[[1]], decreasing=T),]
# hubs.in <- sortedDeg[sortedDeg$deg.in > mean(sortedDeg$deg.in),]

##       Set Vertex/Edges attributes & Layouts
library(RColorBrewer)
col = list(color = brewer.pal(11, 'Paired'))
absWeights <- lapply(netSparse, function(x) abs(E(x)$weight))
weights <- lapply(netSparse, function(x) (E(x)$weight))
edge.start <- lapply(netSparse, function(x) ends(x, es=E(x), names=F)[,1])
edge.end <- lapply(netSparse, function(x) ends(x, es=E(x), names=F)[,2])
  edge.col.out <- list()
  edge.col.in <- list()
  fr <- list()
  kk <- list()
  kk3 <- list()
  MDS <- list()
  MDSdist <- list()
  lgl <- list()
  #drl <- list()
  
for (t in seq_along(netSparse)) {
 print(paste0('t = ', t))
  
  # Vertices
  V(netSparse[[t]])$sectors <- Sectors$Sector
  V(netSparse[[t]])$sector.type <- Sectors$Sector.type
  V(netSparse[[t]])$color <- col$color[Sectors$Sector.type]
  #V(netSparse[[t]])$size <- 3*log(deg.out[[t]])
  #V(netSparse[[t]])$hubs <- 

  # Edges
  E(netSparse[[t]])$weights.abs <- abs(E(netSparse[[t]])$weight) ### absWeights
    edge.col.out[[t]] <- V(netSparse[[t]])$color[edge.start[[t]]]
    edge.col.in[[t]] <- V(netSparse[[t]])$color[edge.end[[t]]]
  E(netSparse[[t]])$col.out <- edge.col.out[[t]]
  E(netSparse[[t]])$col.in <- edge.col.in[[t]]
  
  # Layouts
  fr[[t]] <- layout_with_fr(netSparse[[t]], weights = absWeights[[t]])
  kk[[t]] <- layout_with_kk(netSparse[[t]], weights = absWeights[[t]])
  #kk3[[t]] <- layout_with_kk(netSparse[[t]], weights = absWeights[[t]], dim = 3)
  MDS[[t]] <- layout_with_mds(netSparse[[t]])
  MDSdist[[t]] <- layout_with_mds(netSparse[[t]], dist = as.matrix(distMatrices[[t]]))
  #lgl[[t]] <- layout_with_lgl(netSparse[[t]])
  #drl[[t]] <- layout_with_drl(netSparse[[t]])
  #l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
}
  
igraph::V(netSparse[[1]])
igraph::list.vertex.attributes(netSparse[[10]])
igraph::list.edge.attributes(netSparse[[10]])
# # Set Vertex attbs.
# V(netSparse)$sectors <- Sectors$Sector
# V(netSparse)$sector.type <- Sectors$Sector.type
# library(RColorBrewer)
# col = list(color = brewer.pal(11, 'Paired'))
# V(netSparse)$color <- col$color[Sectors$Sector.type]
# V(netSparse)$size <- 3*log(deg.out)
# Set Edges attbs.
# absWeights <- abs(E(netSparse)$weight)
# signWeights <- E(netSparse)$weight
# E(netSparse)$weights.abs <- abs(E(netSparse)$weight)
# edge.start <- ends(netSparse, es=E(netSparse), names=F)[,1]
# edge.end <- ends(netSparse, es=E(netSparse), names=F)[,2]
# edge.out.col <- V(netSparse)$color[edge.start]
# edge.in.col <- V(netSparse)$color[edge.end]
# E(netSparse)$col.out <- edge.out.col
# E(netSparse)$col.in <- edge.in.col
# 
# fr <- layout_with_fr(netSparse, weights = absWeights)
# kk <- layout_with_kk(netSparse, weights = absWeights)
# kk3 <- layout_with_kk(netSparse, weights = absWeights, dim = 3)
# MDS <- layout_with_mds(netSparse)
# MDSdist <- layout_with_mds(netSparse, dist = as.matrix(distMatrices[[1]]))
# lgl <- layout_with_lgl(netSparse)
# drl <- layout_with_drl(netSparse)
# l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)



####################################################################################################################


##    PLOT NETWORKS

pdf("Network_kk.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = kk[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'kk layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]+deg.in[[t]]/10)
  legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("Network_fr.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = fr[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'fr layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]+deg.in[[t]]/10)
  legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("Network_MDS.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = MDS[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'MDS layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]+deg.in[[t]]/10)
  legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("Network_MDSdist.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = MDSdist[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'MDS (distance matrix) layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]+deg.in[[t]]/10)
  legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

rnd <- layout_randomly(netSparse[[1]])
pdf("Network_randomFixed.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = rnd, edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'MDS layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]+deg.in[[t]]/10)
  legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()



### Gain loss!

layouts <- c('layout_with_fr',  'layout_with_mds')
l <- list()
for (layout in layouts) {
  print(paste0('l = ', layout))
  l[[which(layouts==layout)]] <- lapply(netSparse, function(x) do.call(layout, list(x)))

}

       layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] # Remove layouts that do not apply to our graph. 
       layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree|star|circle|nicely|gem", layouts)]
#       par(mfrow=c(3,3), mar=c(1,1,1,1))
#       pdf(file = 'pruebaSparse_In_layouts2.pdf', width = 16, height = 15)
#       E(netSparse)$weight <- absWeights
#       for (layout in layouts) { 
#         print(layout)
#         l <- do.call(layout, list(netSparse)) 
#         plot.igraph(netSparse, layout = l, edge.color = edge.in.col, edge.curved=.3, edge.width= (E(netSparse)$weight)/3,
#                     vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black', vertex.size = (deg.out+deg.in/10),
#                     edge.arrow.size = 1, edge.arrow.width = 1,
#                     main = layout, edge.arrow.mode = 0)
#       }
#       E(netSparse)$weight <- signWeights
#       dev.off()
      
      





      
      
      
      
      
      
      
      
      
      
      



##########################
##########################
##########################
##########################


# ITERATIONS THROUGH ALL EPOCHS!: gif? video?

netSigns <- simplify(adjacency_list_signs[[t]], remove.multiple = F, remove.loops = T)
netWeights <- simplify(adjacency_list_weights[[t]], remove.multiple = F, remove.loops = T)



####################################################################################################################
####################################################################################################################
####################################################################################################################


                       ############################
                       ######   ANIMATIONS   ######
                       ############################


library('animation') 
library('igraph')
ani.options(convert='C:/Program Files/ImageMagick-7.0.2-Q16/convert.exe')
ani.options('convert')
rnd <- layout_randomly(netSparse[[1]])

saveGIF( {
  for (t in 1:T) {
    plot.igraph(netSparse[[t]], layout = rnd, edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
                vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
                edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
                vertex.size = deg.out[[t]]+deg.in[[t]]/10, sub = paste0('t = ', t))
    legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
           pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
    }
  },
  interval = 1, movie.name="network_rnd.gif" 
)

detach('package:igraph') 
detach('package:animation')













l <- layout.fruchterman.reingold(net)
saveGIF( { col <- rep("grey40", vcount(netSigns)) 
           plot(netSigns, vertex.color=col, layout=l)
           
           step.1 <- V(net)[media=="Wall Street Journal"] 
           col[step.1] <- "#ff5100" 
           plot(netSigns, vertex.color=col, layout=l)
           
           step.2 <- unlist(neighborhood(netSigns, 1, step.1, mode="out")) 
           col[setdiff(step.2, step.1)] <- "#ff9d00" 
           plot(netSigns, vertex.color=col, layout=l)
           
           step.3 <- unlist(neighborhood(netSigns, 2, step.1, mode="out")) 
           col[setdiff(step.3, step.2)] <- "#FFDD1F" 
           plot(netSigns, vertex.color=col, layout=l) }, 
         
         interval = .8, movie.name="network_animation.gif" )
detach('package:igraph') 
detach('package:animation')






















####################################################################################################################
####################################################################################################################
####################################################################################################################


                   ############################
                   ######    ANALYSIS    ######
                   ############################


# Create analysis variables

info.stocks <- cbind(stockNames$FTSE100_list, Sectors[,3], data.frame(deg.total), data.frame(deg.out), data.frame(deg.in))  # avgDegree???
names(info.stocks) <- c('stocks', 'sectors', 'deg.total', 'deg.out', 'deg.in')
sortedDeg <- info.stocks[order(deg.in, decreasing=T),]
hubs <- sortedDeg[sortedDeg$deg.in > mean(deg.in),]
# hubs
matplot(info.stocks, type = c("b"), pch=1, col = 1:3, main = 'Network degrees')
text(1:nStocks, info.stocks$deg.total, labels = info.stocks$stocks, col = col$color[Sectors$Sector.type])
legend(x = 60, y = 95, legend = c('deg.out','deg.in','deg.total'), col=1:3, pch=1, pt.cex=2, cex=.8, ncol=1, y.intersp = 0.2)
maxValues <- lapply(weightedMatrices, function(x) max(x))
minValues <- lapply(weightedMatrices, function(x) min(x))
means <- lapply(weightedMatrices, function(x) mean(as.matrix(x)))
sds <- lapply(weightedMatrices, function(x) sd(as.matrix(x)))
info.epochs <- cbind(1:T, maxValues, minValues, means, sds)
###  cent <- lapply(netSparse, function(x) centralization.degree(x)$centralization)
# plot(1:length(maxValues), unlist(maxValues), xlab='epoch', ylab='weight', main='Max Weights')
# plot(1:length(minValues), unlist(minValues), xlab='epoch', ylab='weight', main='Min Weights')
# plot(1:length(means), unlist(means), xlab='epoch', ylab='weight', main='mean weights')
hist(E(netWeights)$weight,20)
hist(E(netSparse)$weight,20)

sectors <- unique(Sectors$Sector)
sectors.deg <- list()
for (t in 1:T) {
  sectors.degOut <- list()
  sectors.degIn <- list()
  sectors.degTotal <- list()
  for (n in 1:nSectors) {
    sectors.degOut <- deg.out[[t]][Sectors$Sector==sectors(n)]
    sectors.degIn <- deg.in[[t]][Sectors$Sector==sectors(n)]
    sectors.degTotal <- deg.total[[t]][Sectors$Sector==sectors(n)]
  }
  sectors.deg[[t]] <- cbind(sectors, sectors.degTotal, sectors.degOut, sectors.degIn)
  
}

epoch = 4
sort(weights[[epoch]], decreasing =T)
edge.end[[epoch]]

pdf("Weight_hist.pdf")
for (t in seq_along(netSparse)) {
  hist(weights[[t]], main = paste0('t = ', t), xlab = 'weights')
  hist(deg.in[[t]], main = paste0('t = ', t), xlab = 'deg.in')
  plot(1:nStocks, deg.in[[t]], pch=16, col = col$color[Sectors$Sector.type])
  text(1:nStocks, deg.in[[t]]+2, labels = Sectors$x, col = 'black')
  
#     layout = rnd, edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
#               vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
#               edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#               main = 'MDS layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]+deg.in[[t]]/10)
#   legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
#          pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
  
}
dev.off()

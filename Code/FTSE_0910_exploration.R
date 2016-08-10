            

rm(list= ls())

            #######################################
            #######   EXPLORATORY ANALYSIS  #######
            #######################################


# LOAD EXPLORATORY VARIABLES

folderPath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/'
SectorsFull <- read.csv(paste0(folderPath,'FTSE_Stocks.csv'))
Sectors <- SectorsFull[2:nrow(SectorsFull),]  # remove AAL, not in sample
## change the input of stockNames(use sectors dataframe so simpler)
stockNames <- read.csv(paste0(folderPath,'FTSE_indexes.csv'))
avgDegree <- read.csv(paste0(folderPath, 'networks/avgDegree.csv'), header = F)
degreeMatrix <- read.csv(paste0(folderPath, 'networks/degreeMatrix.csv'), header = F)
GainLossMatrix <- read.csv(paste0(folderPath, 'networks/GainLoss.csv'), header = F)
nStocks <- length(stockNames[,2])
nSectors <- length(unique(Sectors$Sector.type))


# LOAD ALL EPOCH'S MATRICES

filesList = list.files(paste0(folderPath,'networks/'), pattern="*.csv")
# matrix_list <- vector(mode="character", length=0)
# for (i in 1:length(filesList)) {
#   matrix_list[i] = unlist(strsplit(filesList[i], split='.', fixed=TRUE))[1]   #remove .csv
# }

# Open folder containing adjacency matrices
setwd('C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/networks')

# List of weighted matrices
listWeights = list.files(paste0(folderPath,'networks/'), pattern='*weights.csv')
weightedMatrices <- list()
for (i in seq_along(listWeights)) {
  weightedMatrices[[i]] <- read.csv(file = listWeights[i], header = F)
}

# List of signed matrices
listSigned = list.files(paste0(folderPath,'networks/'), pattern='*signs.csv')
signedMatrices <- list()
for (i in seq_along(listSigned)) {
  signedMatrices[[i]] <- read.csv(file = listSigned[i], header = F)
}

#List of distance matrices
listDistances = list.files(paste0(folderPath,'networks/'), pattern='dist*')
distMatrices <- list()
for (i in seq_along(listDistances)) {
  distMatrices[[i]] <- read.csv(file = listDistances[i], header = F)
}



# TODO:
#     - plots (scatter) of avgDegree, degreeMatrix (y) for each of the stocks (names), with different colors for different sectors!!





####################################################################################################

#      PREPROCESSING FOR igraph PACKAGE




# Required libraries to plot networks
library(igraph)
library(network)
library(sna)
library(ndtv)
library(visNetwork)


# GIVE PROPER FORMAT TO THE INPUTS (-> igraph OBJECTS):  http://igraph.org/r/doc/graph_from_adjacency_matrix.html

adjacency_list_weights <- list()
for (i in 1:length(weightedMatrices)) {
  adjacency_list_weights[[i]] <- graph_from_adjacency_matrix(as.matrix(weightedMatrices[[i]]), mode = "directed",
                    weighted = T, diag = TRUE, add.colnames = NULL, add.rownames = NA)
}

adjacency_list_signs <- list()
for (i in 1:length(weightedMatrices)) {
  adjacency_list_signs[[i]] <- graph_from_adjacency_matrix(as.matrix(signedMatrices[[i]]), mode = "directed",
                                                           weighted = NULL, diag = TRUE, add.colnames = NULL, add.rownames = NA)
}

net <- simplify(adjacency_list_signs[[1]], remove.multiple = F, remove.loops = T)
net2 <- simplify(adjacency_list_weights[[1]], remove.multiple = F, remove.loops = T)





#####################################################################################################

                       #################################
                       ######   NETWORK DRAWING   ######
                       #################################


#                              SIGNED MATRICES
labels <- as.character(stockNames[,2])

# trying plots:      -->  [1]  <---


# Set vertex size proportional to degree
#deg <- degree(net)
deg <- degreeMatrix[,1] #-----------------------------------> OUT DEGREE!!
V(net)$size <- 3*log(deg) # play with this parameter
######## we can encode whatever to the size!!

# Set edge width based on weight  (only makes sense with weighted matrices)
E(net)$width <- E(net)$weight/6
E(net)$arrow.size <- .2

# Set color for each sector (encoded by numeric class in 'Sector.type' in the Stocks dataset)
# nSectors <- length(unique(Sectors$Sector.type))
# colrs <- palette(rainbow(nSectors))   #Bug detected: sometimes it doesnt save this specifications at first attempt 
# V(net)$color <- colrs[Sectors$Sector.type]
# plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
# legend(x=1, y=1, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=colrs,
#        pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)




####################################################################################################


#   MAIN NETWORK (sectors, size->degree)



#pdf(file = 'pruebaMAIN.pdf', width = 16, height = 15) # Store picture as .pdf
deg <- degreeMatrix[,1]  # maybe using degree computed from igraph???
V(net)$size <- 1.7*log(deg)
nSectors <- length(unique(Sectors$Sector.type))
library(RColorBrewer)
col = list(color = brewer.pal(11, 'Paired'))
V(net)$color <- col$color[Sectors$Sector.type]
plot(net, layout= ,edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')
legend(x=0.5, y=-0.5, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=3, cex=1.5, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#dev.off()
# REMEMBER: issues with color when trimming the dataset (AAL wasnt in the sample), thats why in the legend Im using
# to identify them StocksFull (if not it gives color given the Sector.type but the 1st tag doesnt correspond to materials)


###################################################################################################################


#    SECTORS NETWORK (directed+colored links, no labels)


# E(net)$width <- E(net)$weight/6 <---- just when using weighted matrix
nSectors <- length(unique(Sectors$Sector.type))
library(RColorBrewer)
col = list(color = brewer.pal(11, 'Paired'))
V(net)$color <- col$color[Sectors$Sector.type]

#pdf(file = 'pruebaSECTORS.pdf', width = 16, height = 15)
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
deg <- degreeMatrix[,1]  # maybe using degree computed from igraph???
V(net)$size <- 1.7*log(deg)
plot(net, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=labels,
     vertex.label.color = 'black')
legend(x=0.5, y=-0.5, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=3, cex=1.5, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#dev.off()

###################################################################################################################


#     Multidimensional Scaling: similarity distances = pathway


# REMEMBER: I can use my own similarity measurement!!!

edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
nSectors <- length(unique(Sectors$Sector.type))
library(RColorBrewer)
col = list(color = brewer.pal(11, 'Paired'))
V(net)$color <- col$color[Sectors$Sector.type]

#pdf(file = 'pruebaMDS.pdf', width = 16, height = 15)
V(net)$size <- 10
plot(netSparse, layout=layout_with_mds, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')
legend(x=0.5, y=-0.5, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=3, cex=1.5, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#dev.off()


# In order to find/group the stocks that behave more similarly we use as distance metric the Euclidean
# distances of the returns -> we need to input it as a distance Matrix (symmetric)
plot(net, layout=layout_with_mds(net,dist=as.matrix(distMatrices[[1]])), edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')


# MDS (not links)
fit <- cmdscale(distMatrices[[1]], eig = F, k = 2)
x <- fit[,1]
y <- fit[,2]
plot(x, y, pch = 19, xlim = range(x), col = 'blue')
text(x, y, pos = 4, labels = stockNames[,2])
plot(x, y, pch = 19, xlim = c(-0.2, 0.2), ylim = c(-0.4, 0.2), col = 'blue')
text(x, y, pos = 4, labels = stockNames[,2])

# stckNms <- filter(stockNames[,2], !grepl("BARC|LLOY|RBS",x))
# trimmedStockNames <- stockNames[!grepl("BARC|LLOY|RBS",stockNames[,2]),2]
# text(x, y, pos = 4, labels = trimmedStockNames)


#####################################################################################################


#   Gain-Loss propagation (colored directed links (and nodes??): green/red)


#pdf(file = 'pruebaGainLoss.pdf', width = 16, height = 15)
nSectors <- length(unique(Sectors$Sector.type))
library(RColorBrewer)
col = list(color = brewer.pal(11, 'Paired'))
V(net)$color <- col$color[Sectors$Sector.type]
G_L <- GainLossMatrix[,1]
V(net)$size <- 1.7*abs(G_L)*100
edge.start <- ends(net, es=E(net), names=F)[,1]
V(net)$colorLinks <- c('green', 'red')[(G_L<0)+1]
edge.col <- V(net)$colorLinks[edge.start]
V(net)$shape <- c('circle', 'square')[(G_L<0)+1]
plot(net, edge.color=edge.col, edge.curved=.1, edge.arrow.size=0.2, edge.arrow.width=0.2, vertex.label='',
     vertex.label.color = 'black')
#legend
#dev.off()




## maybe use vertex.color=Gain/loss (and plot the names of assets)




#library('png')
# triUp <- readPNG("C:/Users/Manuel/Desktop/Southampton/MasterThesis/supportImages/triangleUp.png") 
# triDown <- readPNG("C:/Users/Manuel/Desktop/Southampton/MasterThesis/supportImages/triangleDown.png")
# triUp_hollow <- readPNG("C:/Users/Manuel/Desktop/Southampton/MasterThesis/supportImages/triangleUp_hollow.png") 
# triDown_hollow <- readPNG("C:/Users/Manuel/Desktop/Southampton/MasterThesis/supportImages/triangleDown_hollow.png")
# V(net)$raster <- list(triUp_hollow, triDown_hollow)[(G_L<0)+1]
# 
# pos.edges <- incident(net, V(net)[G_L>0], mode="out")
# neg.edges <- incident(net, V(net)[G_L<0], mode="out")
# ecol <- rep("gray80", ecount(net)) 
# ecol[pos.edges] <- "green"
# ecol[neg.edges] <- "red"
# plot(net, edge.color=ecol)











#####################################################################################################

# Fruchterman-Reinhold layout (force directed)
l <- layout_with_fr(net)
plot(net, layout=l, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')
legend(x=1, y=1, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)

###################################################################################################################

# to rescale the network manually we need to give it the limits we want and multiply layout by the desired coefficient
# REMEMBER to use argument 'rescale = F'
l <- layout_with_fr(net)
l <- norm_coords(l, ymin=-2, ymax=2, xmin=-2, xmax=2)
plot(net, layout=l, rescale = F, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')
legend(x=1.3, y=1, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.4)

###################################################################################################################

# Kamada-Kawai (force directed)
l1 <- layout_with_kk(net)
l1 <- norm_coords(l1, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(net, layout=l1*2, rescale = F, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')

#####################################################################################################












#                            WEIGHTED MATRICES


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#         ENFORCE SPARSITY IN igraph
# 
# hist(links$weight)
# mean(links$weight)
# sd(links$weight)
# cut.off <- mean(links$weight)


ecount(net2)
netSparse <- delete_edges(net2, E(net2)[abs(weight) < 0.01])  # choose a suitable cut off
ecount(netSparse)
plot(net2, )

# https://en.wikipedia.org/wiki/Assortativity  ||  https://en.wikipedia.org/wiki/Structural_cut-off


deg.out <- igraph::degree(netSparse, mode = 'out')
deg.in <- igraph::degree(netSparse, mode = 'in')
deg.total <- igraph::degree(netSparse, mode = 'total')
V(netSparse)$size <- 3*log(deg.out)
nSectors <- length(unique(Sectors$Sector.type))
library(RColorBrewer)
col = list(color = brewer.pal(11, 'Paired'))   #Take 11 colors from sample called 'Paired'
V(netSparse)$color <- col$color[Sectors$Sector.type]
edge.start <- ends(netSparse, es=E(netSparse), names=F)[,1]
edge.end <- ends(netSparse, es=E(netSparse), names=F)[,2]
edge.out.col <- V(netSparse)$color[edge.start]
edge.in.col <- V(netSparse)$color[edge.end]
V(net)$size <- 1.7*log(deg)

#######
absWeights <- abs(E(netSparse)$weight)
signWeights <- E(netSparse)$weight
#######
fr <- layout_with_fr(netSparse, weights = absWeights)
kk <- layout_with_kk(netSparse, weights = absWeights)
kk3 <- layout_with_kk(netSparse, weights = absWeights, dim = 3)
MDS <- layout_with_mds(netSparse)
MDSdist <- layout_with_mds(netSparse, dist = as.matrix(distMatrices[[1]]))
lgl <- layout_with_lgl(netSparse)
drl <- layout_with_drl(netSparse)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)


pdf(file = 'pruebaSparse_In_MDSdist.pdf', width = 16, height = 15)
plot.igraph(netSparse, layout = lgl, edge.color = edge.in.col, edge.curved=.3, edge.width= (E(netSparse)$weight)/3,
     vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black', vertex.size =deg.out,
     edge.arrow.size = 1, edge.arrow.width = 1,
     main = 'MDSdist - In links - flow clockwise ', edge.arrow.mode = 0)
legend(x=0.5, y=-0.8, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
dev.off()


pdf(file = 'pruebaSparse_Out.pdf', width = 16, height = 15)
plot(netSparse, layout = l1, edge.color = edge.out.col, edge.curved=.2, edge.width= (E(netSparse)$weight)/2,
     vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black', vertex.size =deg.out,
     edge.arrow.size = E(netSparse)$weight, edge.arrow.width = E(netSparse)$weight,
     main = 'kk - Out links', edge.arrow.mode = 0)
legend(x=0.5, y=-0.5, unique(SectorsFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=3, cex=1.5, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
dev.off()






pdf(file = 'pruebaSparse_In_layouts.pdf', width = 16, height = 15)
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] # Remove layouts that do not apply to our graph. 
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree|star|circle|nicely|gem", layouts)]
par(mfrow=c(3,3), mar=c(1,1,1,1)) 
for (layout in layouts) { 
  print(layout)
  l <- do.call(layout, list(netSparse)) 
  plot.igraph(netSparse, layout = l, edge.color = edge.in.col, edge.curved=.3, edge.width= (E(netSparse)$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black', vertex.size =deg.out,
              edge.arrow.size = 1, edge.arrow.width = 1,
              main = layout, edge.arrow.mode = 0)
}
dev.off()















info.stocks <- cbind(stockNames$FTSE100_list, Sectors[,3], data.frame(deg.total), data.frame(deg.out), data.frame(deg.in))  # avgDegree???
sortedDeg <- info.stocks[order(deg.in, decreasing=T),]
hubs <- sortedDeg[sortedDeg$deg.in > mean(deg.in),]
hubs
matplot(info.stocks, type = c("b"), pch=1, col = 1:3)
text(1:nStocks, info.stocks$deg.total, labels = info.stocks$`stockNames$FTSE100_list`, col = col$color[Sectors$Sector.type])
legend(x = 60, y = 95, legend = c('deg.out','deg.in','deg.total'), col=1:3, pch=1, pt.cex=2, cex=.8, ncol=1, y.intersp = 0.2)

#








#####################################################################################################

# distribution of plots in a grid:
par(mfrow=c(2,2), mar=c(0,0,0,0)) # plot four figures - 2 rows, 2 columns
par(mfrow=c(1,1))  # bigger!


##### example data of network drawing with R
rm(list=ls())
setwd('C:/Users/Manuel/Desktop/Southampton/MasterThesis/testNetworkR/polnet2016/Data files/')
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)









#####################################################################################################

                    ################################
                    ######      ANALYSIS      ######
                    ################################

# https://en.wikipedia.org/wiki/Assortativity  ||  https://en.wikipedia.org/wiki/Structural_cut-off

# Explore weights
w1 <- as.matrix(weightedMatrices[[1]])
w1 <- as.matrix(weightedMatrices[[6]])
w1 <- as.matrix(weightedMatrices[[12]])

maxValues <- lapply(weightedMatrices, function(x) max(x))
minValues <- lapply(weightedMatrices, function(x) min(x))
means <- lapply(weightedMatrices, function(x) mean(as.matrix(x)))
sds <- lapply(weightedMatrices, function(x) sd(as.matrix(x)))
      ###  cent <- lapply(netSparse, function(x) centralization.degree(x)$centralization)
plot(1:length(maxValues), unlist(maxValues), xlab='epoch', ylab='weight', main='Max Weights')
plot(1:length(minValues), unlist(minValues), xlab='epoch', ylab='weight', main='Min Weights')
library(vioplot)
vioplot(c(as.matrix(weightedMatrices[[1]])))

#analysis.epochs <- cbind(1:12, maxValues, minValues, means, sds, cent?? ...some interesting general measures of networks!)


# centralization in a for loop (or at least I need a list of T 'net' objects!!)





#w1[which(w1<-1000, arr.ind=T)]<- (-1000)
which(w1<(-1e10), arr.ind=T)
hist(w1[w1>(-200)], 100, freq = T)
hist(w1, freq = T, 100)


hist(w1[w1<0],20)
hist(w1[w1>0],20)

plot(1:length(w1)*length(w1), w1)

length(w1[w1<(-1e9)])
which(w1<(-1e10))
# > (w1[w1<(-1e3)])
# [1] -4.0925e+10 -1.0538e+06 -3.1492e+03 -2.9849e+03 -6.5540e+03 -1.3014e+03 -2.1536e+03
# [8] -1.4197e+03 -2.6632e+03 -4.7288e+05 -1.1901e+03
###### buscar a que indices corresponden los mas pequeños (outliers??, porque??)



            
            #######################################
            #######   EXPLORATORY ANALYSIS  #######
            #######################################


# LOAD EXPLORATORY VARIABLES

folderPath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/'
StocksFull <- read.csv(paste0(folderPath,'FTSE_Stocks.csv'))
Stocks <- StocksFull[2:nrow(StocksFull),]  # remove AAL, not in sample
stockNames <- read.csv(paste0(folderPath,'networks/FTSE_indexes.csv'))
avgDegree <- read.csv(paste0(folderPath, 'networks/avgDegree.csv'), header = F)
degreeMatrix <- read.csv(paste0(folderPath, 'networks/degreeMatrix.csv'), header = F)



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





# TODO:
#     - plots (scatter) of avgDegree, degreeMatrix (y) for each of the stocks (names), with different colors for different sectors!!








#####################################################################################################

            #################################
            ######   NETWORK DRAWING   ######
            #################################


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


#####################################################################################
####### trying plots: Signed Matrix        -->  [1]  <---

# plot(net)
# 
# plot(net, edge.arrow.size=.01,vertex.label=as.character(stockNames[,2]))    #arrows' size decreased, added labels
# 
# plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')



# Set vertex size proportional to degree
#deg <- degree(net)
deg <- degreeMatrix[,1] #-----------------------------------> OUT DEGREE!!
V(net)$size <- 3*log(deg) # play with this parameter
#plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
######## we can encode whatever to the size!!


# Tag nodes with stock names
# V(net)$label <- stockNames[,2]  -----> doesnt seem to store


# Set edge width based on weight  (only makes sense with weighted matrices)
E(net)$width <- E(net)$weight/6
E(net)$arrow.size <- .2




# Set color for each sector (encoded by numeric class in 'Sector.type' in the Stocks dataset)

# nSectors <- length(unique(Stocks$Sector.type))
# colrs <- palette(rainbow(nSectors))   #Bug detected: sometimes it doesnt save this specifications at first attempt 
# V(net)$color <- colrs[Stocks$Sector.type]
# plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
# legend(x=1, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=colrs,
#        pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)
library(RColorBrewer)
nSectors <- length(unique(Stocks$Sector.type))
col = list(color = brewer.pal(11, 'Paired'))   #Take 11 colors from sample called 'Paired'
V(net)$color <- col$color[Stocks$Sector.type]
plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
legend(x=1, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)
# REMEMBER: issues with color when trimming the dataset (AAL wasnt in the sample), thats why in the legend Im using
# to identify them StocksFull (if not it gives color given the Sector.type but the 1st tag doesnt correspond to materials)



###################################################################################################################
# Size: out degree.   Directed links (color).
# Color edges after source node
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

# E(net)$width <- E(net)$weight/6 <---- just when using weighted matrix
deg <- degreeMatrix[,1]
V(net)$size <- 3*log(deg)

# plot(net, edge.color=edge.col, edge.curved=.1, edge.arrow.size=0, edge.arrow.width=0, vertex.label=as.character(stockNames[,2]),
#        vertex.label.color = 'black')
# plot(net, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
#      vertex.label.color = 'black')
plot(net, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label='',
     vertex.label.color = 'black')
legend(x=1, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)
###################################################################################################################

# Fruchterman-Reinhold layout (force directed)
l <- layout_with_fr(net)
plot(net, layout=l, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')
legend(x=1, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)

###################################################################################################################

# to rescale the network manually we need to give it the limits we want and multiply layout by the desired coefficient
# REMEMBER to use argument 'rescale = F'
l <- layout_with_fr(net)
l <- norm_coords(l, ymin=-2, ymax=2, xmin=-2, xmax=2)
plot(net, layout=l, rescale = F, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')
legend(x=1.3, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=col$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.4)

###################################################################################################################

# Kamada-Kawai (force directed)
l1 <- layout_with_kk(net)
l1 <- norm_coords(l1, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(net, layout=l1*2, rescale = F, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')


####################################################################################################################

# Multidimensional Scaling: similarity distances = pathway
# REMEMBER: I can use my own similarity measurement!!!
plot(net, layout=layout_with_mds, edge.color=edge.col, edge.curved=.1, edge.arrow.mode = 0, vertex.label=as.character(stockNames[,2]),
     vertex.label.color = 'black')





#####################################################################################################
#####################################################################################################

par(mfrow=c(2,2), mar=c(0,0,0,0)) # plot four figures - 2 rows, 2 columns
par(mfrow=c(1,1))  # bigger!

##### example data
rm(list=ls())
setwd('C:/Users/Manuel/Desktop/Southampton/MasterThesis/testNetworkR/polnet2016/Data files/')
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)









#####################################################################################################
#####################################################################################################










#####################################################################################################

           ################################
           ######      ANALYSIS      ######
           ################################

# Explore weights
w1 <- as.matrix(weightedMatrices[[1]])
w6 <- as.matrix(weightedMatrices[[6]])
w12 <- as.matrix(weightedMatrices[[12]])

hist(w1, freq = T, axes = T)
mean(w1)
sd(w1)
max(w1)
min(w1)

hist(w1[w1<0],20)
hist(w1[w1>0],20)

plot(1:length(w1)*length(w1), w1)

length(w1[w1<-1e10])
# > (w1[w1<(-1e3)])
# [1] -4.0925e+10 -1.0538e+06 -3.1492e+03 -2.9849e+03 -6.5540e+03 -1.3014e+03 -2.1536e+03
# [8] -1.4197e+03 -2.6632e+03 -4.7288e+05 -1.1901e+03
###### buscar a que indices corresponden los mas pequeños (outliers??, porque??)







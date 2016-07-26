            
            #######################################
            #######   EXPLORATORY ANALYSIS  #######
            #######################################


# LOAD EXPLORATORY VARIABLES

folderPath <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE_0910/'
StocksFull <- read.csv(paste0(folderPath,'FTSE_Stocks.csv'))
Stocks <- Stocks[2:nrow(StocksFull),]  # remove AAL, not in sample
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
adjacency_list_signs <- list()
for (i in 1:length(weightedMatrices)) {
adjacency_list_signs[[i]] <- graph_from_adjacency_matrix(as.matrix(signedMatrices[[i]]), mode = "directed",
                    weighted = NULL, diag = TRUE, add.colnames = NULL, add.rownames = NA)
}
adjacency_list_weights <- list()
for (i in 1:length(weightedMatrices)) {
  adjacency_list_weights[[i]] <- graph_from_adjacency_matrix(as.matrix(weightedMatrices[[i]]), mode = "directed",
                    weighted = T, diag = TRUE, add.colnames = NULL, add.rownames = NA)
}


#####################################################################################
####### trying plots

net <- simplify(adjacency_list_signs[[1]], remove.multiple = F, remove.loops = T)
net2 <- simplify(adjacency_list_weights[[1]], remove.multiple = F, remove.loops = T)
plot(net)

plot(net, edge.arrow.size=.01,vertex.label=as.character(stockNames[,2]))    #arrows' size decreased, added labels

plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')



# Set vertex size proportional to degree
#deg <- degree(net)
deg <- degreeMatrix[,1]
V(net)$size <- 3*log(deg) # play with this parameter
#plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
######## we can encode whatever to the size!!

# Tag nodes with stock names
V(net)$label <- stockNames[,2]

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6
E(net)$arrow.size <- .2

# Set color for each sector (encoded by numeric class in 'Sector.type' in the Stocks dataset)
nSectors <- length(unique(Stocks$Sector.type))
require(RColorBrewer)
# colrs <- palette(rainbow(nSectors))
 colrs <- palette(rainbow(nSectors))   #Bug detected: sometimes it doesnt save this specifications at first attempt 
#colrs = list(color = brewer.pal(11, 'Paired'))
V(net)$color <- colrs[Stocks$Sector.type]
# V(net)$color <- colrs$color[Stocks$Sector.type]
plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
legend(x=1, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=colrs$color,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)


# REMEMBER: issues with color when trimming the dataset (AAL wasnt in the sample), thats why in the legend Im using
# to identify them StocksFull (if not it gives color given the Sector.type but the 1st tag doesnt correspond to materials)



nSectors <- length(unique(Stocks$Sector.type))
colrs <- palette(rainbow(nSectors))
colrs <- palette(rainbow(nSectors))   #Bug detected: sometimes it doesnt save this specifications at first attempt 
V(net)$color <- colrs[Stocks$Sector.type]
plot(net, edge.arrow.size=.05, edge.curved=.1, vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black')
legend(x=1, y=1, unique(StocksFull$Sector), pch=21,  col="#777777", pt.bg=colrs,
       pt.cex=2, cex=.8, bty="n", ncol=1, y.intersp = 0.2)






#####################################################################################################
#####################################################################################################

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






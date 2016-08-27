

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
ggcolor <- gg_color_hue(10)
pdf("Analysis_plots.pdf")
for (t in seq_along(netSparse)) {
  h1 <- qplot(deg.in[[t]],
              geom="histogram",
              #           binwidth = 0.2,  
              bins=100,
              main = paste0('t = ',t), 
              xlab = "log(In.Deg)") +
    scale_x_log10()
  h2 <- qplot(weights[[t]],
              geom="histogram",
              binwidth = 0.2,
              xlab = "In Weights") +
    ggtitle('In Weight Distribution') +
    geom_vline(data=hubs.in[[t]], aes(xintercept=avg.weight.in), color=ggcolor[hubs.in[[t]]$sector.type], show.legend=TRUE) +
    geom_text(aes(hubs.in[[t]]$avg.weight.in, 200 ,label = hubs.in[[t]]$stocks, vjust=1, hjust=0))
  #main = paste0('t = ',t), 
  #fill=I("blue"), 
  #col=I("red"), 
  #alpha=I(.2)
  #text(1:nStocks, deg.in[[t]]+2, labels = Sectors$x, col = 'black')
  
  dIn <-  ggplot(info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
          y = deg.in, fill = sectors)) + geom_bar(stat = 'identity') +
          theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
          theme_bw() + labs(x = '', y = 'In Degree') + theme(legend.position="none",
          axis.text.y =element_blank()) +  ###element_text(face=NULL, size=6, angle=0)) +
          geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3)
  
  dOut <-   ggplot(info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
            y = deg.out, fill = sectors)) + geom_bar(stat = 'identity') +
            theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
            theme_bw() + labs(x = '', y = 'Out Degree') + theme(legend.position="none",
            axis.text.y =element_blank()) +
            geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3) 
  
  wIn <-  ggplot(info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
          y = weights.in, fill = sectors)) + geom_bar(stat = 'identity') +
          theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
          theme_bw() + labs(x = '', y = 'Sum In Weights') + theme(legend.position="none",
          axis.text.y =element_blank()) + geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3)
  
  wOut <- ggplot(data= info.stocks[[t]], aes(x = reorder(stocks, as.numeric(sectors)), 
          y = weights.out, fill = sectors)) + geom_bar(stat = 'identity') +
          theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
          theme_bw() + labs(x = '', y = 'Sum Out Weights') + theme(legend.position="none",
          axis.text.y =element_blank()) + geom_text(aes(label = reorder(stocks, as.numeric(sectors))), size = 3)
  
  dSin <- ggplot(info.sectors[[t]], aes(x = sector, y = deg.in, fill = sector)) + geom_bar(stat = 'identity') +
          theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
          theme_bw() + labs(x = '', y = 'In Degree') + ggtitle('Degree by Sectors') +
          theme(legend.position="none")
  
  dSout <- ggplot(info.sectors[[t]], aes(x = sector, y = deg.out, fill = sector)) + geom_bar(stat = 'identity') +
           theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
           theme_bw() + labs(x = '', y = 'Out Degree') + ggtitle('Degree by Sectors') +
           theme(legend.position="none")
  
  wSin <- ggplot(info.sectors[[t]], aes(x = sector, y = weight.in, fill = sector)) + geom_bar(stat = 'identity') +
          theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
          theme_bw() + labs(x = '', y = 'Sum In Weights') + ggtitle('') +
          theme(legend.position="none")
  
  wSout <- ggplot(info.sectors[[t]], aes(x = sector, y = weight.out, fill = sector)) + geom_bar(stat = 'identity') +
           theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
           theme_bw() + labs(x = '', y = 'Sum Out Weights') + ggtitle('') +
           theme(legend.position="none")
  
  multiplot(h1, h2) # , layout = as.matrix(c(1,2,3,3), nrow=2, byrow=TRUE))
  multiplot(dIn, dOut, dSin, dSout, cols = 2)
  multiplot(wIn, wOut, wSin, wSout, cols = 2)
  
}
dev.off()










#------------------------------------------------------------------------------------------    


### create   info.epochs <- cbind(1:T, maxValues, minValues, means, sds) dataframe (as info.sto)


#--------------------------------#
# ANALYSIS OF NETWORK OVER TIME  #
#--------------------------------#


### Plot: avg Degree Out of the Network over time
#     sumDeg.out <- sapply(info.stocks, function(x) sum(x$deg.out))
#     meanDeg.out <- sapply(info.stocks, function(x) mean(x$deg.out))
#     sdsDeg.out <- sapply(info.stocks, function(x) sd(x$deg.out))
#     meanWeight.out <- sapply(info.stocks, function(x) mean(x$weights.out))
#     sdsWeight.out <- sapply(info.stocks, function(x) sd(x$weights.out))
#     dfDeg <- data.frame(cbind(1:T, sumDeg.out, meanDeg.out, sdsDeg.out, meanWeight.out, sdsWeight.out))
#     names(dfDeg) <- c('epoch', 'sumDeg.out', 'meanDeg.out', 'sdsDeg.out', 'meanWeight.out', 'sdsWeight.out')
#         pl1 <- ggplot(data=dfDeg, aes(x=epoch, y=meanDeg.out), colour='black') + geom_errorbar(aes(ymin=meanDeg.out-sdsDeg.out, ymax=meanDeg.out+sdsDeg.out), width=0.25) +
#                geom_line() + geom_point() + ggtitle('Average Out Degree') + ylab('Avg. Degree')


### Plot: avg Degree/Weight grouped by Sectors over time

df <- cbind(sector=rep(names(sectors),sapply(sectors,nrow)),do.call(rbind,sectors))

dIs_legend <- ggplot() + geom_line(data=df, aes(x=epoch,y=meanDeg.in, color=sector)) + #scale_colour_hue(c=45, l=80) + 
              ggtitle('Mean Degree In per Stock') + ylab('DegreeIn')

dOs_legend <- ggplot() + geom_line(data=df, aes(x=epoch,y=meanDeg.out, color=sector)) + #scale_colour_hue(c=45, l=80) + 
              ggtitle('Mean Degree Out per Stock') + ylab('Degree Out')
#         dOs <- ggplot() + geom_line(data=df, aes(x=epoch,y=meanDeg.out, color=sector)) + #scale_colour_hue(c=45, l=80) + 
#                       ggtitle('Mean Degree Out per Stock') + ylab('Degree Out') + theme(legend.position="none")
#         dIs <- ggplot() + geom_line(data=df, aes(x=epoch,y=meanDeg.in, color=sector)) + #scale_colour_hue(c=45, l=80) + 
#                       ggtitle('Mean Degree In per Stock') + ylab('DegreeIn') + theme(legend.position="none")
### Plot: variation of Weights Out of the sectors and network over time
#         pl2 <- ggplot(data=dfDeg, aes(x=epoch, y=meanWeight.out), colour='black') + geom_errorbar(aes(ymin=meanWeight.out-sdsWeight.out, ymax=meanWeight.out+sdsWeight.out), width=0.25) +
#             geom_line() + geom_point() + ggtitle('Average Out Weight') + ylab('Avg. Weight')

wOs <- ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.out.links, color=sector)) + #scale_colour_hue(c=45, l=80) +
        ggtitle('Avg. Out Weight per stock') + theme(legend.position="none")

wOe <- ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.out.edges, color=sector)) + #scale_colour_hue(c=45, l=80) +
        ggtitle('Avg. Out weight per edge') + theme(legend.position="none")

wOt <- ggplot() + geom_line(data=df, aes(x=epoch,y=weight.out, color=sector)) + #scale_colour_hue(c=45, l=80) + 
        ggtitle('Total Out weight') + theme(legend.position="none")

wIt <- ggplot() + geom_line(data=df, aes(x=epoch,y=weight.in, color=sector)) + #scale_colour_hue(c=45, l=80) 
        ggtitle('Total In weight') + theme(legend.position="none")

wIs <- ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.in.links, color=sector)) + #scale_colour_hue(c=45, l=80) + 
        ggtitle('Avg. In weight per Stock') + theme(legend.position="none")

wIe <- ggplot() + geom_line(data=df, aes(x=epoch,y=avg.weight.in.edges, color=sector)) + #scale_colour_hue(c=45, l=80) + 
        ggtitle('Avg. In weight per edge') + theme(legend.position="none")

#         library(gridExtra)
#         source('C:/Users/Manuel/Desktop/Southampton/MasterThesis/Code/g_legend.R')
#         legend <- g_legend(dI_legend)
### Export plots to working directory
pdf('AnalysisOverTime.pdf')
#           grid.arrange(legend , dIs, wIs, wIe  + guides(col = guide_legend(nrow=1, keywidth = 2, keyheight = 2)))
#           grid.arrange(legend + theme(legend.position= c(0.8, 0.8)), dOs, wOs, wOe  + 
#                        guides(col = guide_legend(nrow=1, keywidth = 2, keyheight = 2))) 
multiplot(dIs_legend, wIs, wIe, layout= matrix(c(1,2,1,3), ncol=2))
multiplot(dOs_legend, wOs, wOe, layout= matrix(c(1,2,1,3), ncol=2))
dev.off()






##### Plots in a loop (double check)

# dOs2 <- ggplot()
# wOs2 <- ggplot()
# wOe2 <- ggplot()
# dIs2 <- ggplot()
# wIs2 <- ggplot()
# wIe2 <- ggplot()
# 
# for (n in 1:nSectors) {
#   dOs2 <- dOs2 + geom_line(data=sectors[[n]], aes(x=epoch, y=meanDeg.out), colour=ggcolor[n])
#   wOs2 <- wOs2 + geom_line(data=sectors[[n]], aes(x=epoch, y=avg.weight.out.links), colour=ggcolor[n])
#   wOe2 <- wOe2 + geom_line(data=sectors[[n]], aes(x=epoch, y=avg.weight.out.edges), colour=ggcolor[n])
#   
#   dIs2 <- dIs2 + geom_line(data=sectors[[n]], aes(x=epoch, y=meanDeg.in), colour=ggcolor[n])
#   wIs2 <- wIs2 + geom_line(data=sectors[[n]], aes(x=epoch, y=avg.weight.in.links), colour=ggcolor[n])
#   wIe2 <- wIe2 + geom_line(data=sectors[[n]], aes(x=epoch, y=avg.weight.in.edges), colour=ggcolor[n])
# }


pdf('Analysis_prueba.pdf')
multiplot(dIs2, wIs2, wIe2, layout= matrix(c(1,1,2,3), ncol=2))
multiplot(dOs2, wOs2, wOe2, layout= matrix(c(1,2,1,3), ncol=2))
dev.off()




#






























#       
#       
#       
# ### Heatmap: Sectors relationships
#         library(plyr)
#         edges.info <- list()
#         sectConnectivity <- list()
#         for (t in 1:T) {
#           edges.info[[t]] <- cbind(data.frame(edge.start[[t]], as.character(info.stocks[[t]]$stocks[edge.start[[t]]]), info.stocks[[t]]$sectors[edge.start[[t]]], info.stocks[[t]]$sector.type[edge.start[[t]]], edge.end[[t]], info.stocks[[t]]$stocks[edge.end[[t]]], info.stocks[[t]]$sectors[edge.end[[t]]], info.stocks[[t]]$sector.type[edge.end[[t]]]))
#           names(edges.info[[t]]) <- c('edge.start', 'stock.start', 'sector.start', 'sectorType.start', 'edge.end', 'stock.end', 'sector.end', 'sectorType.end')  
#           links <- count(edges.info[[t]],vars = c("sectorType.start","sectorType.end"))
#           totalEdges <- nrow(edges.info[[1]])
#           sectConnectivity[[t]] <- data.frame(matrix(ncol=nSectors,nrow=nSectors))
#             for (n in 1:nSectors) {
#                 for (m in 1:nSectors) {
#                   freq <- subset(links, sectorType.start==n & sectorType.end==m)$freq
#                   if (length(freq)==0) {
#                     sectConnectivity[[t]][n,m] <- 0
#                   } else if (length(freq)==1) {
#                       sectConnectivity[[t]][n,m] <- subset(links, sectorType.start==n & sectorType.end==m)$freq
#                   }
#                 }
#             }
#           names(sectConnectivity[[t]]) <- c('Communications', 'Consumer Staples', 'Consumers Discretionary', 'Energy', 'Financials', 'Health Care', 'Industrials', 'Materials', 'Technology', 'Utilities')
#           row.names(sectConnectivity[[t]]) <- c('Communications', 'Consumer Staples', 'Consumers Discretionary', 'Energy', 'Financials', 'Health Care', 'Industrials', 'Materials', 'Technology', 'Utilities')
#           sectConnectivity[[t]] <- as.matrix(sectConnectivity[[t]])
#           }
# 
#         library(reshape2)
#         pdf('prueba_Connectivity.pdf')
# #         palf <- colorRampPalette(c('white', 'dark blue'))
#         melted_sectCon <- list()
#         palf <- colorRampPalette(colors = c("white","blue"))
#         for (t in 1:T) {
#           heatmap(sectConnectivity[[t]], Rowv = NA, Colv = NA, col = palf(10), scale="none", margins=c(10,10), ylab='Edge Start', xlab='Edge End', main= paste0('t = ', t))
# #           melted_sectCon[[t]] <- melt(sectConnectivity[[t]])
# #           ggplot(data = melted_sectCon[[t]], aes(x=Var1, y=Var2, fill = value)) + geom_tile(colour = "white") + 
# #                 scale_fill_gradient(low = "white", high = "steelblue") #+
# # #                 ggtitle('Number of links between Sectors') + ylab('Edge end') + xlab('Edge start')
#           
#           ####### same plot with weights??
#           }
#         dev.off()
#         
#         

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
  #           hc2 <- plot(dend, main=paste0('t = ', t), cex.axis=0.2)#, horiz=TRUE) # display dendogram
  #           groups <- cutree(fit, k=5)
  # draw dendogram with red borders around the 5 clusters 
  rect.hclust(fit, k=5, border="red")
  plot(as.phylo(fit), type = "fan",tip.color = ggcolor[clustSectors])
  
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
  #           hc3 <- plot(as.phylo(fit),tip.color = ggcolor[clustSectors])
  
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
#      library(rgexf)
#      for (t in 1:T) {
#      nod <- data.frame(cbind(V(netSparse[[t]]), as.character(V(netSparse[[t]]))))
#      edg <- t(Vectorize(get.edge, vectorize.args='id')(netSparse[[t]], 1:ecount(netSparse[[t]])))
#      write.gexf(nodes = nod, edges = edg, edgesWeight = E(netSparse[[t]])$weight, defaultedgetype = "directed", output = paste0('t',t,'.gexf'))
#                 #, nodesAtt = nodes_att, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, 
#      routput = ("lesmis.gexf")
#      }
#      write.gexf(nod,edg)
#      
#      



#      
#      #######prueba de dynamic networks (animations)
#      library(ndtv)
#      library(intergraph)
#      nett <- lapply(netSparse2, function(x) asNetwork(x, directed=TRUE))
#      tnet<-networkDynamic(network.list=nett)
#      render.d3movie(tnet, vertex.col='color', edge.col='gray', displaylabels=FALSE)
# #      render.animation(tnet)
# #      ani.replay()
# 
#      
#      
#      









######## Network measures

netprueba2 <- netSparse[[1]]
E(netprueba2)$weight <- absWeights[[1]]
incloseness <- igraph::closeness(netprueba2, mode='in')
clos <- data.frame(cbind(as.character(info.stocks[[1]]$stocks), as.character(info.stocks[[1]]$sectors), incloseness))
names(clos) <- c('stocks', 'sectors', 'incloseness')
ggplot(clos, aes(x=stocks, y=incloseness, color=sectors)) + geom_point() + coord_flip()


betweenness <- igraph::betweenness(netprueba2)
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
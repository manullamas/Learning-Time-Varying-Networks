
### SECTOR CONNECTIVITY HEATMAP


library(plyr)
edges.info <- list()
sectConnectivity <- list()
melted_sectCon <- list()
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
  melted_sectCon[[t]] <- melt(sectConnectivity[[t]])
}

# not identified problem in the for loop doing this


heatMap_fill <- scale_fill_gradient(low = "white", high = "steelblue")
heatMap_theme <- theme(axis.text.x=element_text(angle=45, size=12, vjust=1, hjust=1), axis.text.y=element_text(size=12, vjust=1, hjust=1), axis.title=element_text(size=14,face="bold"))
  
  pdf('sectorsHeatmap.pdf')
ggplot(data = melted_sectCon[[10]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 1')
      
ggplot(data = melted_sectCon[[2]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 2')

ggplot(data = melted_sectCon[[3]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 3')

ggplot(data = melted_sectCon[[4]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 4')
  
ggplot(data = melted_sectCon[[5]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 5')
  
ggplot(data = melted_sectCon[[6]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 6')
  
ggplot(data = melted_sectCon[[7]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 7')
  
ggplot(data = melted_sectCon[[8]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 8')
  
ggplot(data = melted_sectCon[[9]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 9')
  
ggplot(data = melted_sectCon[[10]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 10')
  
ggplot(data = melted_sectCon[[11]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 11')
  
ggplot(data = melted_sectCon[[12]], aes(x=Var1, y=Var2, fill = value)) +
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 12')
  
ggplot(data = melted_sectCon[[13]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 13')
  
ggplot(data = melted_sectCon[[14]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 14')
  
ggplot(data = melted_sectCon[[15]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed()+
      ggtitle('t = 15')
  
ggplot(data = melted_sectCon[[16]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed()+
      ggtitle('t = 16')
  
ggplot(data = melted_sectCon[[17]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed()+
      ggtitle('t = 17')
  
ggplot(data = melted_sectCon[[18]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed()+
      ggtitle('t = 18')
  
ggplot(data = melted_sectCon[[19]], aes(x=Var1, y=Var2, fill = value)) + 
      geom_tile(colour = "white") + ylab('Edge End') + xlab('Edge Start') +
      heatMap_fill + heatMap_theme + coord_fixed() +
      ggtitle('t = 19')

dev.off()



pdf("PRUEBA.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], 
              layout = Reingold[[t]], 
#               edge.color = 'grey',
              edge.color = edge.col.in[[t]], 
              edge.curved=.3, 
              edge.width= abs((E(netSparse[[t]])$weight)/10),
              vertex.label='',
              # vertex.label=as.character(stockNames[,2]), 
              vertex.label.color='black',
              edge.arrow.size = 1, 
              edge.arrow.width = 1, 
              edge.arrow.mode = 0,
              main = 'Reingold - Frutcherman', 
              sub = paste0('t = ', t), 
              vertex.size = deg.out[[t]]/5+deg.in[[t]]/18,
              vertex.label.cex=0.5,
              vertex.label.dist=0.1,
              vertex.label.family='Helvetica',
              vertex.frame.color= col$color[Sectors$Sector.type],
              edge.lty = 1 #  use 'dashed'
              
              )
  
  legend(x=0.5, y=-0.8, unique(Sectors$Sector), pch=21,  col="#777777", pt.bg=col$color,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()
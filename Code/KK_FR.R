

## Kamada - Kawai  ---> Fruchterman - Reingold


setwd("C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/networks")

# Create layouts
kk.abs <- list()
fr <- list()
fr_kk <- list()
for (t in seq_along(netSparse)) {
      kk.abs[[t]] <- layout_with_kk(netSparse[[t]], weights = absWeights[[t]])
      fr[[t]] <- layout_with_fr(netSparse[[t]])
      # Use Kamada Kawai algorithm to define the starting coordinates for the F-R
      fr_kk[[t]] <- layout_with_fr(netSparse[[t]], coords = TESLA.kk.abs[[t]])#, weights = Weights[[t]])
}

pdf("kk.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = kk.abs[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'Kamada Kawai', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("fr.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = fr[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'Fruchterman - Reingold', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("fr_kk.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = fr_kk[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'FR + KK', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()
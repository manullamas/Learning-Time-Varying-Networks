

op <- par()
par(op)



rnd <- layout_randomly(netSparse[[1]])
rnd <- layout_with_fr(netSparse[[1]])
pdf("FlowNetwork.pdf")
# png(paste0('prueba2','.png'))
# p <- par(mfrow=c(4,3))
for (t in 1:length(netSparse)) {
  plot.igraph(netSparse[[t]], 
              layout = rnd, 
              #  edge.color = 'grey',
              edge.color =  E(netSparse[[t]])$col.contagion, 
              edge.curved=.3, 
              edge.width= abs((E(netSparse[[t]])$weight)/10),
              vertex.label='',
              vertex.color=V(netSparse[[t]])$col.contagion,
              vertex.frame.color= V(netSparse[[t]])$col.contagion,
              # vertex.label=as.character(stockNames[,2]), 
              vertex.label.color='black',
              edge.arrow.size = 1, 
              edge.arrow.width = 1, 
              edge.arrow.mode = 0,
              main = paste0('t = ', t), 
#               sub = paste0('t = ', t), 
              vertex.size = deg.out[[t]]/5+deg.in[[t]]/18,
              vertex.label.cex=0.5,
              vertex.label.dist=0.1,
              vertex.label.family='Helvetica',
              edge.lty = 1 #  use 'dashed'
  )
}
# par(p)
dev.off()


# ggplot.objects = list(p2,p2,p2)




### Contagion GIF
library('animation') 
library('igraph')
L <- layout_randomly(netSparse[[1]])
ani.options(interval=1)
saveGIF({
  for(t in 1:length(netSparse)){
    plot.igraph(netSparse[[t]], 
                layout = rnd, 

#                 vertex.label='',
                vertex.size = deg.out[[t]]/5+deg.in[[t]]/18,
                vertex.color=V(netSparse[[t]])$col.contagion,
                vertex.frame.color= V(netSparse[[t]])$col.contagion,
                vertex.label=as.character(stockNames[,2]),
                vertex.label.color='black',
                vertex.label.cex=1,
                vertex.label.dist=0.1,
                vertex.label.family='Helvetica',
                #  edge.color = 'grey',
                edge.color =  E(netSparse[[t]])$col.contagion, 
                edge.curved=.3, 
                edge.width= abs((E(netSparse[[t]])$weight)/10),
                edge.arrow.size = 1, 
                edge.arrow.width = 1, 
                edge.arrow.mode = 0,
                main = 'Reingold - Frutcherman', 
                sub = paste0('t = ', t), 

                edge.lty = 1) #  use 'dashed' 
  }
}, interval = 3, movie.name = "GainLoss.gif", ani.width = 1000, ani.height = 1000)


####################################################################################################################

##    GRAPHS


# Run first NetworksAnalysis.R, aesthetics of graphs encoded there #

pdf("ReingoldTilford.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = Reingold[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color='black',
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'Reingold - Frutcherman', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off() 

pdf("lgl.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = TESLA.lgl2[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color='black',
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'lgl', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("kk_absWeights.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = TESLA.kk.abs[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'TESLA   -   kk layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

pdf("fr.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = TESLA.fr[[5]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'TESLA   -   fr layout', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

#   pdf("TESLA_fr_absWeights.pdf")
#   for (t in seq_along(netSparse)) {
#     plot.igraph(netSparse[[t]], layout = TESLA.fr.abs[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'TESLA   -   fr_abs', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
#                   legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#                          pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()

pdf("MDS.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = TESLA.MDS[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'TESLA   -   MDS', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

#   pdf("TESLA_MDSd.pdf")
#   for (t in seq_along(netSparse)) {
#     plot.igraph(netSparse[[t]], layout = TESLA.MDSdist[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'TESLA   -   MDS (distance matrix)', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
# legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#        pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()

pdf("MDSc.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = TESLA.MDScorr[[t]], edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1], 
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'TESLA   -   MDS (correlation matrix)', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()

rnd <- layout_randomly(netSparse[[1]])
pdf("randomFixed.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = rnd, edge.color = edge.col.in[[t]], edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
              main = 'TESLA   -   Random', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
         pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
}
dev.off()










#------------------------------------------------------------------------------------------------------   
#       FLOW STUDY

pdf("GainLoss.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = rnd, edge.color = E(netSparse[[t]])$col.contagion, edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black',  #'yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0, vertex.color=V(netSparse[[t]])$col.contagion,
              main = 'Gain/Loss Net', sub = paste0('t = ', t),vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)   #log((Sectors$MC)/1e6))  , vertex.size=GainLossMatrix[,t])
  legend("topright", col=col.GL(2), pch=19, legend=c(round(c(-max(abs(GainLossMatrix[,t])), max(abs(GainLossMatrix[,t]))), 2)))
}
dev.off()



# only useful if there is not outlyers (like WOS) -> solve by computing the average returns!!
pdf("GainLossHomogenous.pdf")
for (t in seq_along(netSparse)) {
  plot.igraph(netSparse[[t]], layout = rnd, edge.color = E(netSparse[[t]])$col.contagion.homog, edge.curved=.3, edge.width= (E(netSparse[[t]])$weight)/3,
              vertex.label=as.character(stockNames[,2]), #vertex.label.color = c('black','yellow')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
              edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0, vertex.color=V(netSparse[[t]])$col.contagion.homog,
              main = 'Gain/Loss Net (normalized)', sub = paste0('t = ', t), vertex.size = deg.out[[t]]/5+deg.in[[t]]/18)
  legend("topright", col=col.GL(2), pch=19,
         legend=c(round(c(-max(abs(GainLossMatrix)), max(abs(GainLossMatrix))), 2)))
}
dev.off()


### Contagion GIF
library('animation') 
library('igraph')
L <- layout_randomly(netSparse[[1]])
ani.options(interval=1)
saveGIF({
  count =0
  for(t in 1:length(netSparse)){
    plot(Glist[[i]], layout = L,
         vertex.label = NA,
         vertex.size = 10,
         vertex.color= V(G)$color,
         vertex.frame.color= "white",
         edge.arrow.size = 1,
         edge.color=E(G)$color)
    count = count +1
    title(main="Contagion", 
          sub=paste("Time = ",count), cex.main = 3, cex.sub = 2)
  }
}, interval = 1, movie.name = "demo.gif", ani.width = 1000, ani.height = 1000)




### plot finance nodes as different shape (maybe they are better propagators)
### plot hubs networks as different shape and encode Company or Stock Value on size





#------------------------------------------------------------------------------------------------------   


# CORRELATION TESTS
#   
#   pdf("CORR_kk_absWeights.pdf")
#   for (t in seq_along(netCorrSparse)) {
#     plot.igraph(netCorrSparse[[t]], layout = CORR.kk.abs[[t]], edge.color = edge.col.in.C[[t]], edge.curved=.3, edge.width= (E(netCorrSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in.C[[t]]>mean(deg.in.C[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'CORR   -   kk layout', sub = paste0('t = ', t), vertex.size = deg.out.C[[t]]+deg.in.C[[t]]/10)
#                   legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#                          pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()
#   
#   pdf("CORR_fr.pdf")
#   for (t in seq_along(netCorrSparse)) {
#     plot.igraph(netCorrSparse[[t]], layout = CORR.fr[[t]], edge.color = edge.col.in.C[[t]], edge.curved=.3, edge.width= (E(netCorrSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in.C[[t]]>mean(deg.in.C[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'CORR   -   fr layout', sub = paste0('t = ', t), vertex.size = deg.out.C[[t]]+deg.in.C[[t]]/10)
#       legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#              pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()
#   
#   pdf("CORR_fr_absWeights.pdf")
#   for (t in seq_along(netCorrSparse)) {
#     plot.igraph(netCorrSparse[[t]], layout = CORR.fr.abs[[t]], edge.color = edge.col.in.C[[t]], edge.curved=.3, edge.width= (E(netCorrSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in.C[[t]]>mean(deg.in.C[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'CORR   -   fr_abs', sub = paste0('t = ', t), vertex.size = deg.out.C[[t]]+deg.in.C[[t]]/10)
#       legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#              pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()
#   
#   pdf("CORR_MDS.pdf")
#   for (t in seq_along(netCorrSparse)) {
#     plot.igraph(netCorrSparse[[t]], layout = CORR.MDS[[t]], edge.color = edge.col.in.C[[t]], edge.curved=.3, edge.width= (E(netCorrSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in.C[[t]]>mean(deg.in.C[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'CORR   -   MDS', sub = paste0('t = ', t), vertex.size = deg.out.C[[t]]+deg.in.C[[t]]/10)
#     legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#            pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()
#   
#   pdf("CORR_MDSdist.pdf")
#   for (t in seq_along(netCorrSparse)) {
#     plot.igraph(netCorrSparse[[t]], layout = CORR.MDSdist[[t]], edge.color = edge.col.in.C[[t]], edge.curved=.3, edge.width= (E(netCorrSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in.C[[t]]>mean(deg.in.C[[t]]))+1], 
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'CORR   -   MDS (distance matrix)', sub = paste0('t = ', t), vertex.size = deg.out.C[[t]]+deg.in.C[[t]]/10)
#     legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#            pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()
#   
#   rnd2 <- layout_randomly(netCorrSparse[[1]])
#   pdf("CORR_randomFixed.pdf")
#   for (t in seq_along(netCorrSparse)) {
#     plot.igraph(netCorrSparse[[t]], layout = rnd, edge.color = edge.col.in.C[[t]], edge.curved=.3, edge.width= (E(netCorrSparse[[t]])$weight)/3,
#                 vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','yellow')[(deg.in.C[[t]]>mean(deg.in.C[[t]]))+1],
#                 edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
#                 main = 'CORR   -   Random', sub = paste0('t = ', t), vertex.size = deg.out.C[[t]]+deg.in.C[[t]]/10)
#     legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
#            pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
#   }
#   dev.off()
#   
#   
#   


















# 
# layouts <- c('layout_with_fr', 'layout_with_mds')
# l <- list()
# for (layout in layouts) {
#   print(paste0('l = ', layout))
#   l[[which(layouts==layout)]] <- lapply(netSparse, function(x) do.call(layout, list(x)))
# 
# }
# 
#        layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] # Remove layouts that do not apply to our graph. 
#        layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree|star|circle|nicely|gem", layouts)]
# #       par(mfrow=c(3,3), mar=c(1,1,1,1))
# #       pdf(file = 'pruebaSparse_In_layouts2.pdf', width = 16, height = 15)
# #       E(netSparse)$weight <- absWeights
#       for (layout in layouts) { 
#         print(layout)
#         l <- do.call(layout, list(netSparse)) 
# #         plot.igraph(netSparse, layout = l, edge.color = edge.in.col, edge.curved=.3, edge.width= (E(netSparse)$weight)/3,
# #                     vertex.label=as.character(stockNames[,2]), vertex.label.color = 'black', vertex.size = (deg.out+deg.in/10),
# #                     edge.arrow.size = 1, edge.arrow.width = 1,
# #                     main = layout, edge.arrow.mode = 0)
#        }
# #       E(netSparse)$weight <- signWeights
# #       dev.off()
#       
#       
# 
# 
# 
# 
# 
#       




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
                vertex.label=as.character(stockNames[,2]), vertex.label.color = c('black','red')[(deg.in[[t]]>mean(deg.in[[t]]))+1],
                edge.arrow.size = 1, edge.arrow.width = 1, edge.arrow.mode = 0,
                vertex.size = deg.out[[t]]/5+deg.in[[t]]/18, sub = paste0('t = ', t))
    legend(x=0.5, y=-0.8, sort(unique(Sectors$Sector)), pch=21,  col="#777777", pt.bg=ggcolor,
           pt.cex=1.3, cex=0.7, bty="n", ncol=1, y.intersp = 1, x.intersp = 1)
  }
},
interval = 1, movie.name="network_rnd.gif" 
)

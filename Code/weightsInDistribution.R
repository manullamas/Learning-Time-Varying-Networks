

library('animation') 
library('igraph')
# ani.options(interval=1)
saveGIF({
  for(t in 1:length(weights)){
    hist(weights[[t]])
    qplot(weights[[t]],
          geom="histogram")#,
#           binwidth = 0.2,
#           xlab = "Weights In Distribution") +
#       geom_vline(data=hubs.in[[t]], aes(xintercept=avg.weight.in), color=ggcolor[hubs.in[[t]]$sector.type], show.legend=TRUE) +
#       geom_text(aes(hubs.in[[t]]$avg.weight.in, 200 ,label = hubs.in[[t]]$stocks, vjust=1, hjust=0))
  }
}, interval = 2, movie.name = "weightInDistribution.gif", ani.width = 1000, ani.height = 1000)










for(t in 1:length(weights)){
  png(paste0(t,'weightDist.png'))
  qplot(weights[[t]],
        geom="histogram",
        binwidth = 0.2,
        xlab = "Weights In Distribution") +
        geom_vline(data=hubs.in[[t]], aes(xintercept=avg.weight.in), color=ggcolor[hubs.in[[t]]$sector.type], show.legend=TRUE) +
        geom_text(aes(hubs.in[[t]]$avg.weight.in, 200 ,label = hubs.in[[t]]$stocks, vjust=1, hjust=0)) #+
#         ggtitle('In Weights Distribution') + sub(paste0('t = ', t))
  dev.off()
}

my_command <- 'convert *weightDist.png -delay 2 -loop 0 weightDistribution.gif'
system(my_command)

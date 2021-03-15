p<-pbtree(1, n = 6)
p$node.label<-paste("node", seq(1:(Ntip(p)-1)))

plot(p, show.node.label = T, cex = .8)
nodelabels(frame = "none", col = "red", adj = c(1.5,-0.5), cex = 0.8)
edgelabels(round(p$edge.length,5), frame = "none", col = "blue", adj = c(1,-0.5), cex = .8)

p$node.label[10-Ntip(p)]
round(p$edge.length,4)

plot(extract.clade(p, 10), show.node.label = T, cex = .8)
nodelabels(frame = "none", col = "red", adj = c(1.5,-0.5), cex = 0.8)
edgelabels(round(extract.clade(p, 10)$edge.length,5), frame = "none", col = "blue", adj = c(1,-0.5), cex = .8)

plot(tre_noded,  edge.width = 2, cex = 0.4, 
      show.tip.label = F, type = "fan")
nodelabels(tre_noded$node.label,frame = "none", col = "red", adj = c(1.1,-0.4),cex = 0.6)

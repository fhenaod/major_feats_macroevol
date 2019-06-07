grep("Dilkea" , seed_tre$node.label)
grep("Dilkea", seed_tre_new$node.label)

sub_tr<-as(geospiza, "phylo")
plot(sub_tr, show.tip.label = T, no.margin = TRUE, edge.width = 2, cex = 0.8, show.node.label = T, root.edge = FALSE)
nodelabels(frame = "none", col = "blue", cex=.7, adj = c(1,1.2))
tiplabels(frame = "none", col = "blue", cex = .8, adj = c(1,1.2))
edgelabels(frame = "none", col = "darkgreen", cex = .8, adj = c(1,1.2))
edgelabels(round(t$edge.length,4),frame = "none", col = "black", cex = .8, adj = c(1,-1.2))
axisPhylo(las = .6)
mrca_node<-getMRCA(sub_tr, tip =  c(23, 25))
sub_tr$node.label[mrca_node-Ntip(sub_tr)]

my_tree <- rtree(10)
my_tree$node.label <- paste0("node", seq(1:9))
plot(my_tree, show.node.label = T)
nodelabels(frame = "none", col = "red", adj = c(1.1,-0.4))
mrca_node <-getMRCA(my_tree, tip = c("t1", "t2"))
my_tree$node.label[mrca_node-Ntip(my_tree)]

tr <- rtree(30)
ch <- rcoal(30)
plot(tr,"p", show.tip.label = F, FALSE, root.edge = T)
axisPhylo(1, las = 1)

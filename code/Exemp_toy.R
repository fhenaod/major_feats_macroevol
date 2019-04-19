grep("Dilkea" , seed_tre$node.label)
grep("Dilkea", seed_tre_new$node.label)

sub_tr<-as(geospiza, "phylo")
plot(sub_tr, show.tip.label = T, no.margin = TRUE, edge.width = 2, cex = 0.8, show.node.label = T)
nodelabels(frame = "none", col = "blue", cex=.7, adj = c(1,1.2))
mrca_node<-getMRCA(sub_tr, tip =  c(23, 25))
sub_tr$node.label[mrca_node-Ntip(sub_tr)]



my_tree <- rtree(10)
my_tree$node.label <- paste0("node", seq(1:9))
plot(my_tree, show.node.label = T)
nodelabels(frame = "none", col = "red", adj = c(1.1,-0.4))
mrca_node <-getMRCA(my_tree, tip = c("t1", "t2"))
my_tree$node.label[mrca_node-Ntip(my_tree)]

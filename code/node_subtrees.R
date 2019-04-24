library(phytools)
library(parallel)


# Subseting trees by node labels #####
classes<-unique(tree$node.label)[grep("ales",unique(tree$node.label))] # Class

orders<-unique(tree$node.label)[grep("ales",unique(tree$node.label))] # Order
orders<-orders[-13]

families<-unique(tree$node.label)[grep("ceae",unique(tree$node.label))] # Family
families<-families[-9]

seed_ord<-mclapply(orders, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
saveRDS(seed_ords, "node_subtrees/seed_ord_trees.rds")

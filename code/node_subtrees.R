library(phytools)
library(parallel)
seed_tre<-e.trees[[4]]

orders<-unique(seed_tre$node.label)[grep("ales",unique(seed_tre$node.label))] # orders
orders<-orders[-13]
unique(seed_tre$node.label)[grep("ceae",unique(seed_tre$node.label))] # families

seed_ord<-mclapply(orders, function(x) extract.clade(phy = seed_tre, node = x), mc.cores = 2)
saveRDS(seed_ords, "node_subtrees/seed_ord_trees.rds")

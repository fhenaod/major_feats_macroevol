library(phytools)
library(parallel)

# Subseting #####
orders<-unique(seed_tre_new$node.label)[grep("ales",unique(seed_tre_new$node.label))] # orders
orders<-orders[-13]
families<-unique(seed_tre_new$node.label)[grep("ceae",unique(seed_tre_new$node.label))] # families
families<-families[-9]

seed_ord<-mclapply(orders, function(x) extract.clade(phy = seed_tre_new, node = x), mc.cores = 2)
saveRDS(seed_ords, "node_subtrees/seed_ord_trees.rds")

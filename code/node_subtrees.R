library(phytools)
library(parallel)

tree<-tre_noded

# Subseting trees by node labels
classes<-unique(tree$node.label)[grep("",unique(tree$node.label))] # Class

orders<-unique(tree$node.label)[grep("",unique(tree$node.label))] # Order
orders<-c("Anura", "Caudata", "Gymnophiona") # Squamates

families<-unique(tree$node.label)[grep("ceae",unique(tree$node.label))] # Family

# extract and save clades ####
sampl_clas<-mclapply(classes, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
sampl_ords<-mclapply(orders, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
sampl_fams<-mclapply(families, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
saveRDS(sampl_clas, "rank_sampling/_clas_trees.rds")
saveRDS(sampl_ords, "rank_sampling/_ords_trees.rds")
saveRDS(sampl_fams, "rank_sampling/_fams_trees.rds")

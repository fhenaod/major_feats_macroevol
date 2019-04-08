library(phytools)
library(parallel)
seed_tre<-read.tree("data_megaPhylos/tree_S2018.cr.bi_.txt")

orders<-unique(seed_tre$node.label)[grep("ales",unique(seed_tre$node.label))] # orders
orders<-orders[-13]
families<-unique(seed_tre$node.label)[grep("ceae",unique(seed_tre$node.label))] # families
families<-families[-9]
species<-unique(seed_tre$node.label)[grep("_",unique(seed_tre$node.label))] # species

seed_ord<-mclapply(orders, function(x) extract.clade(phy = seed_tre, node = x), mc.cores = 2)
saveRDS(seed_ords, "node_subtrees/seed_ord_trees.rds")

seed_tre_new<-seed_tre
for(i in 1:length(species)){
  t_labs<-extract.clade(phy = seed_tre_new, node = species[i])$tip.label
  t_drop<-t_labs[1:length(t_labs)-1]
  seed_tre_new<-drop.tip(seed_tre_new, t_drop)
}
length(seed_tre$tip.label)
length(seed_tre_new$tip.label)

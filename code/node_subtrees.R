library(phytools)
library(parallel)

tree<-tre_noded

# Subseting trees by node labels
classes<-unique(tree$node.label)[grep("",unique(tree$node.label))] # Class
classes<-unique(tree$node.label)[grep("opsida\\>",unique(tree$node.label))] # Class plants

orders<-unique(tree$node.label)[grep("ales\\>",unique(tree$node.label))] # Order
orders<-c("Anura", "Caudata", "Gymnophiona") # Squamates
orders<-c("Monotremata", "Eulipotyphla", "Perissodactyla", "Artiodactyla", 
          "Carnivora", "Pholidota", "Chiroptera", "Dermoptera", 
          "Scandentia", "Primates", "Rodentia",  "Lagomorpha",      
          "Cingulata", "Pilosa", "Sirenia",  "Proboscidea", 
          "Hyracoidea", "Tubulidentata", "Macroscelidea", 
          "Afrosoricida", "Paucituberculata", "Didelphimorphia", 
          "Microbiotheria", "Notoryctemorphia", "Peramelemorphia", 
          "Dasyuromorphia", "Diprotodontia" ) # Mammals
orders <- tree$node.label[match(orders, tree$node.label)[!is.na(match(orders, tree$node.label))]]

families<-unique(tree$node.label)[grep("aceae",unique(tree$node.label))] # Family

# extract and save clades ####
sampl_clas<-mclapply(classes, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
sampl_ords<-mclapply(orders, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)

for(i in 1:length(orders)){
  sampl_ords[[i]] <- extract.clade(tree, orders[[i]])
}

sampl_fams<-mclapply(families, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)

saveRDS(sampl_clas, "rank_sampling/seed_clas_trees.rds")
saveRDS(sampl_ords, "rank_sampling/seed_ords_trees.rds")
saveRDS(sampl_fams, "rank_sampling/seed_fams_trees.rds")

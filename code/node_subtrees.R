library(phytools)
library(parallel)

tre_noded <- readRDS("data_megaPhylos/fern_noded_tre.rds")
tree <- tre_noded

# Subseting trees by node labels
classes <- unique(tree$node.label)[grep("",unique(tree$node.label))] # Class
classes <- unique(tree$node.label)[grep("opsida\\>",unique(tree$node.label))] # Class plants, fern

orders <- unique(tree$node.label)[grep("ales$",unique(tree$node.label))] # plant, fern and fungi orders
orders <- unique(tree$node.label)[grep("formes$",unique(tree$node.label))] # bird, fish, shark orders
orders <- c("Anura", "Caudata", "Gymnophiona") # amphibia orders
orders <- c("Monotremata", "Eulipotyphla", "Perissodactyla", "Artiodactyla", 
          "Carnivora", "Pholidota", "Chiroptera", "Dermoptera", 
          "Scandentia", "Primates", "Rodentia",  "Lagomorpha",      
          "Cingulata", "Pilosa", "Sirenia",  "Proboscidea", 
          "Hyracoidea", "Tubulidentata", "Macroscelidea", 
          "Afrosoricida", "Paucituberculata", "Didelphimorphia", 
          "Microbiotheria", "Notoryctemorphia", "Peramelemorphia", 
          "Dasyuromorphia", "Diprotodontia" ) # mammal orders 
# squamates is an order
orders <- tree$node.label[match(orders, tree$node.label)[!is.na(match(orders, tree$node.label))]]

families <- unique(tree$node.label)[grep("idae$", unique(tree$node.label))] # Family
families <- unique(tree$node.label)[grep("aceae$",unique(tree$node.label))] # plant, fern, fungi families

# extract and save clades ####
sampl_clas <- mclapply(classes, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
sampl_ords <- mclapply(orders, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
#sampl_ords <- mclapply(orders, function(x) castor::get_subtree_at_node(tree = tree, node = x)$subtree, mc.cores = 2)
sampl_fams <- mclapply(families, function(x) extract.clade(phy = tree, node = x), mc.cores = 2)
#sampl_fams <- readRDS("rank_sampling/seed_fams/sliced_trees/seed_fams_trees.rds")

rep_nods=function(trees, q2s){
  q2s <- as.character(q2s)
  lts_ds <- c()
  for(i in 1:length(trees)){
    lts_ds[i] <- trees[[i]]$node.label %>% unique() %>% 
      grep(q2s, .) %>% length() >1 
  }
  lts_ds
}

# prune down class
trees <- sampl_clas
q2s <- "opsida$" # plants
rep_nods(sampl_clas, q2s)
for(q in 1:length(trees)){
  lts_o <- trees[[q]]$node.label %>% unique() %>% grep(q2s, .) %>% 
    length() >1 
  if(lts_o) {
    rep_ls <- trees[[q]]$node.label %>% .[grep(q2s, .)]
    ts2pr <- c()
    for(f in 2:length(rep_ls)){
      tp_tm <- try(castor::get_subtree_at_node(trees[[q]], rep_ls[f])$subtree$tip.label)
      ts2pr <- c(ts2pr, tp_tm)
    }
    trees[[q]] <- drop.tip(trees[[q]], ts2pr)
  } else {}
}
sampl_clas <- trees
rep_nods(sampl_clas, q2s) %>% table()

# prune down orders
trees <- sampl_ords
#q2s <- "ormes" # changes acording to taxon
q2s <- "ales$" # plants, ferns, fungi

rep_nods(sampl_ords, q2s)
  for(q in 1:length(trees)){
    lts_o <- trees[[q]]$node.label %>% unique() %>% grep(q2s, .) %>% 
      length() >1 
    if(lts_o) {
      rep_ls <- trees[[q]]$node.label %>% .[grep(q2s, .)]
      ts2pr <- c()
        for(f in 2:length(rep_ls)){
        tp_tm <- try(castor::get_subtree_at_node(trees[[q]], rep_ls[f])$subtree$tip.label)
        ts2pr <- c(ts2pr, tp_tm)
      }
      trees[[q]] <- drop.tip(trees[[q]], ts2pr)
    } else {}
  }
sampl_ords <- trees
rep_nods(sampl_ords, q2s) %>% table()

### amphibians and mammals ##
q2s <- c("Anura", "Caudata", "Gymnophiona") # amphibians
q2s <- c("Monotremata", "Eulipotyphla", "Perissodactyla", "Artiodactyla", 
         "Carnivora", "Pholidota", "Chiroptera", "Dermoptera", 
         "Scandentia", "Primates", "Rodentia",  "Lagomorpha",      
         "Cingulata", "Pilosa", "Sirenia",  "Proboscidea", 
         "Hyracoidea", "Tubulidentata", "Macroscelidea", 
         "Afrosoricida", "Paucituberculata", "Didelphimorphia", 
         "Microbiotheria", "Notoryctemorphia", "Peramelemorphia", 
         "Dasyuromorphia", "Diprotodontia" ) # mammals

trees <- sampl_ords
for(q in 1:length(trees)){
  lts_o <- trees[[q]]$node.label %>% unique() %>% intersect(q2s, .)
  if(length(lts_o)>1) {
    rep_ls <- lts_o
    ts2pr <- c()
    for(f in 2:length(rep_ls)){
      tp_tm <- try(castor::get_subtree_at_node(trees[[q]], rep_ls[f])$subtree$tip.label)
      ts2pr <- c(ts2pr, tp_tm)
    }
    trees[[q]] <- drop.tip(trees[[q]], ts2pr)
  } else {}
}
sampl_ords <- trees
rep_nods(sampl_ords, q2s) %>% table()


# prune down families
trees <- sampl_fams
#q2s <- "idae" # changes acording to taxon
q2s <- "aceae" # plant and fungi
rep_nods(sampl_fams, q2s)
for(q in 1:length(trees)){
  lts_o <- trees[[q]]$node.label %>% unique() %>% grep(q2s, .) %>% 
    length() >1 
  if(lts_o) {
    rep_ls <- trees[[q]]$node.label %>% .[grep(q2s, .)]
    ts2pr <- c()
    for(f in 2:length(rep_ls)){
      tp_tm <- try(castor::get_subtree_at_node(trees[[q]], rep_ls[f])$subtree$tip.label)
      ts2pr <- c(ts2pr, tp_tm)
    }
    trees[[q]] <- drop.tip(trees[[q]], ts2pr)
  } else {}
}
sampl_fams <- trees
rep_nods(sampl_fams, q2s) %>% table()

saveRDS(sampl_clas, "rank_sampling/fern_clas_trees.rds")
saveRDS(sampl_ords, "rank_sampling/fern_ords_trees.rds")
saveRDS(sampl_fams, "rank_sampling/fern_fams_trees.rds")

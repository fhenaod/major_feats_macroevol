library(apTreeshape)
library(RPANDA)
library(parallel)
library(tidyverse)
library(castor)

source('../get_DR.R')
source('../funct_mets.R')

clad <- c("mamm", "amph", "bird_erick", "bird_hack", "shark", "squa")
results <- c()

for(j in 1:length(clad)) {
#clad_trees <- readRDS(paste0("../../data_megaPhylos/", clad[j],"_trees_clean.rds"))
clad_trees <- readRDS(paste0("data_megaPhylos/", clad[j],"_trees_clean.rds"))
samp_trees <- clad_trees[sample(length(clad_trees), 10)] 

res <- c()
for(i in 1:length(samp_trees)){
    clad_trees_i <- samp_trees[[i]]
    
    names_sp <- strsplit(clad_trees_i$tip.label, "_")
    genera <- unique(get_genus(names_sp))
    clad_trees_i$tip.label <- gsub("_", " ", clad_trees_i$tip.label)
  
    tre_g_noded <- name_genus_nodes(clad_trees_i, genera)
    m <- read.csv(paste0("taxonomy/", clad[j],"_tax.csv"), header = T, row.names = 1)
    tre_noded <- name_htaxa_nodes(tre_g_noded, m)
    
    if (clad[j] == "bird_erick") {
      orders <- unique(tre_noded$node.label)[grep("ormes$", unique(tre_noded$node.label))] 
    } else if (clad[j] == "bird_hack") {
      orders <- unique(tre_noded$node.label)[grep("ormes$", unique(tre_noded$node.label))]
    } else if (clad[j] == "shark") {
      orders <- unique(tre_noded$node.label)[grep("ormes$", unique(tre_noded$node.label))]
    } else if (clad[j] == "amph") {
      orders <- c("Anura", "Caudata", "Gymnophiona") # amphibia orders
    } else if (clad[j] == "mamm") {
      orders <- c("Monotremata", "Eulipotyphla", "Perissodactyla", "Artiodactyla", 
                  "Carnivora", "Pholidota", "Chiroptera", "Dermoptera", 
                  "Scandentia", "Primates", "Rodentia",  "Lagomorpha",      
                  "Cingulata", "Pilosa", "Sirenia",  "Proboscidea", 
                  "Hyracoidea", "Tubulidentata", "Macroscelidea", 
                  "Afrosoricida", "Paucituberculata", "Didelphimorphia", 
                  "Microbiotheria", "Notoryctemorphia", "Peramelemorphia", 
                  "Dasyuromorphia", "Diprotodontia" ) # mammal orders 
    } else if (clad[j] == "squa") {
      orders <- c("Squamata") # squamate orders
    } else print("error")
    families <- unique(tre_noded$node.label)[grep("idae$",unique(tre_noded$node.label))]

    sampl_ords <- mclapply(orders, function(x) extract.clade(phy = tre_noded, node = x), mc.cores = 2)
    sampl_fams <- mclapply(families, function(x) extract.clade(phy = tre_noded, node = x), mc.cores = 2)
    
    rank.sl <- c(sampl_ords, sampl_fams)
    
  for(f in 1:length(rank.sl)){
    # Tree metrics 
    tree_metrics_sum <- round(tree_metrics(rank.sl), 2)

    # DR 
    trees_dr <- sapply(rank.sl, get_DR)
    trees_mean_dr <- sapply(trees_dr, mean)

    # Imbalance metrics 
    imbalance.metrics <- imbalance_metrics(rank.sl, 2)

    # RPANDA Estimate tree's spectrum from a list of trees
    trees_spectR <- mclapply(rank.sl, spectR, mc.cores = 10)

    # Extract tree's spectrum summary stats
    trees_spec_sum <- extract_spect(trees_spectR)

    sum_stats <- data.frame(tree_metrics_sum, trees_mean_dr, 
                            imbalance.metrics, trees_spec_sum)
    
    sum_stats$n_tree <- paste0(clad[j], "_", i)
    sum_stats$tax_rank <- c(rep("order", length(sampl_ords)), rep("family", length(sampl_fams)))
    
    saveRDS(sum_stats, paste0("output/", clad[j], i,"_sum_stats.rds"))
    }
  }
res <- rbind(res, sum_stats)
saveRDS(res, paste0("output/", clad[j],"_all_sum.rds"))
}
results <- rbind(results, res)
saveRDS(results, "output/all_clad_sum.rds")



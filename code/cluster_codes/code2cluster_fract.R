library(apTreeshape)
library(RPANDA)
library(parallel)
library(tidyverse)
library(castor)

source('../get_DR.R')
source('../funct_mets.R')

clad <- c("mamm", "amph", "bird_erick", "bird_hack", "shark", "squa")

results_final <- c()
for(j in 1:length(clad)) {
  clad_trees <- readRDS(paste0("../../data_megaPhylos/", clad[j],"_trees_clean.rds"))
  samp_trees <- clad_trees[sample(length(clad_trees), 10)] 
  
res <- c()
for(i in 1:length(samp_trees)){
    clad_trees_i <- samp_trees[[i]]
    
    fr_tm <- c()
    t_h2samp <- c(10, 20, 30, 40, 50)
    nsamps <- c(100, 80, 50, 30, 10)
    for(h in 1:length(t_h2samp)) {
      rand_trees_raw <- random_tree_samp(clad_trees_i, t_height = t_h2samp[h], n_samps = nsamps[h])
      rand_trees <- plyr::compact(rand_trees_raw[[1]])
      orig_brake_age <- rand_trees_raw[[2]][!is.na(rand_trees_raw[[2]])]
      
      rand_trees_f <- rand_trees[sapply(rand_trees, function(x) Ntip(x) >= 4)]
      orig_brake_age_f <- orig_brake_age[sapply(rand_trees, function(x) Ntip(x) >= 4)]
      
        # Tree metrics 
        tree_metrics_sum <- round(tree_metrics(rand_trees_f), 2)
        
        # DR 
        trees_dr <- sapply(rand_trees_f, get_DR)
        trees_mean_dr <- sapply(trees_dr, mean)
        
        # Imbalance metrics 
        #imbalance.metrics <- imbalance_metrics(rand_trees_f, 2)
        
        # RPANDA Estimate tree's spectrum from a list of trees
        trees_spectR <- mclapply(as.list(rand_trees_f), spectR, mc.cores = 10)
        
        # Extract tree's spectrum summary stats
        trees_spec_sum <- extract_spect(trees_spectR)
        
        sum_stats <- data.frame(tree_metrics_sum, trees_mean_dr, trees_spec_sum)
        
        sum_stats$or_brake_age_f <- orig_brake_age_f
        sum_stats$n_tree <- paste0(clad[j], "_", i)
        sum_stats$ht_smp <- rep(paste0("t_wind_", t_h2samp[h]))
        
        fr_tm <- rbind(fr_tm, sum_stats)
        }
    res <- rbind(res, fr_tm)
  }

saveRDS(res, paste0("output/", clad[j],"_sum.rds"))

results_final <- rbind(results_final, res)
}
saveRDS(results_final, "output/all_clad_frct_sum.rds")

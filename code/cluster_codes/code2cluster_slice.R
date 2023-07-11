library(apTreeshape)
library(RPANDA)
library(parallel)
library(tidyverse)
library(castor)

source('../get_DR.R')
source('../funct_mets.R')

sl_mth <- "sturges_jenks" #regular

#clad <- c("mamm", "amph", "bird_erick", "bird_hack", "shark", "squa")
clad <- names(emp_clean_trs)
results <- c()

emp_clean_trs <- readRDS("emp_clean_trs.rds")
for(j in 1:length(clad)) {
#clad_trees <- readRDS(paste0("../../data_megaPhylos/", clad[j],"_trees_clean.rds"))
#samp_trees <- clad_trees[sample(length(clad_trees), 10)] 
samp_trees <- emp_clean_trs

res <- c()
for(i in 1:length(samp_trees)){
  clad_trees_i <- samp_trees[[i]]
  
  if(sl_mth=="sturges_jenks"){
    ages2cut <- round(BAMMtools::getJenksBreaks(branching.times(clad_trees_i), 
                                                nclass.Sturges(branching.times(clad_trees_i))),3) # Sturges-Jenks intervals
    clad.sl <- slice_tree_ages(clad_trees_i, ages2cut[2:length(ages2cut)])
  } else {
    ages2cut <- seq(from = 4, to = max(branching.times(clad_trees_i)), by = 5) # regular
    clad.sl <- slice_tree_ages(clad_trees_i, ages2cut[1:length(ages2cut)])
  }

  # Tree metrics 
  tree_metrics_sum <- round(tree_metrics(clad.sl), 2)
  #saveRDS(tree_metrics_sum, paste0("output/", clad[j], "_tree_metrics.rds"))
  #tree_metrics_sum<-readRDS(paste0("output/", clad , "_tree_metrics.rds"))
  
  # DR 
  trees_dr <- sapply(clad.sl, get_DR)
  trees_mean_dr <- sapply(trees_dr, mean)
  #trees_mean_dr <- colMeans(trees_dr) %>% data.frame() %>% 
    #rownames_to_column(var = "t_names") %>% rename(dr = ".")
  #saveRDS(trees_mean_dr, paste0("output/", clad[j], "_trees_mean_dr.rds"))
  #trees_mean_dr<-readRDS(paste0("output/", clad , "_trees_mean_dr.rds"))
  
  # Imbalance metrics 
  imbalance.metrics <- imbalance_metrics(clad.sl, 2)
  #saveRDS(imbalance.metrics, paste0("output/", clad , "_imbalance.metrics.rds"))
  #imbalance.metrics<-readRDS(paste0("output/", clad, "_imbalance.metrics.rds"))
  
  # RPANDA Estimate tree's spectrum from a list of trees
  trees_spectR <- mclapply(clad.sl, spectR, mc.cores = 10)
  #trees_spectR <- readRDS(paste0("output/", clad, "_tree_spectR.rds"))
  #trees_spectR <- Filter(Negate(is.null), trees_spectR)
  #saveRDS(trees_spectR, paste0("output/", clad[j], "_tree_spectR.rds"))
  
  # Extract tree's spectrum summary stats
  trees_spec_sum <- extract_spect(trees_spectR)
  #saveRDS(cbind(names(trees_spectR), trees_spec_sum), paste0("output/", clad , "_trees_spec_sum.rds"))
  #trees_spec_sum<-readRDS(paste0("output/", clad , "_trees_spec_sum.rds"))
  
  sum_stats <- data.frame(tree_metrics_sum, trees_mean_dr, 
                          imbalance.metrics, trees_spec_sum)
  
  sum_stats$n_tree <- paste0(clad[j], "_",i)
  
  saveRDS(sum_stats, paste0("output/", clad[j], i,"_sum_stats.rds"))
  }
res <- rbind(res, sum_stats)
saveRDS(res, paste0("output/", clad[j],"_all_sum.rds"))
}
results <- rbind(results, res)
saveRDS(results, "output/all_clad_sum.rds")



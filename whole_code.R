library(apTreeshape)
library(RPANDA)
library(parallel)

amph_5.samp<-rand_trees_f

# Tree metrics ####
tree_metrics_sum<-round(tree_metrics(amph_5.samp),2)
#saveRDS(tree_metrics_sum,"output/amph_5_tree_metrics.rds")

# Branch length time stats####
branch_stats<-br_len.t_stats(amph_5.samp)

# DR ####
trees_dr<-sapply(amph_5.samp,get_DR)
trees_mean_dr<-sapply(trees_dr,mean)

# RPANDA ####
# Estimate tree's spectrum from a list of trees
trees_spectR<-mclapply(amph_5.samp,spectR, mc.cores = 3)

# Extract tree's spectrum summary stats ####
trees_spec_sum<-extract_spect(trees_spectR)
#saveRDS(trees_spec_sum, "output/amph_5_trees_spec_sum.rds")

sum_stats<-data.frame(tree_metrics_sum, trees_mean_dr, ln_dr=log(trees_mean_dr), trees_spec_sum, branch_stats, orig_brake_age_f)
saveRDS(sum_stats,"self_sim/sim_bird_50_sum_stats.rds")

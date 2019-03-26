library(phytools)
library(parallel)

ntips<-c(100, 1000, 10000, 50000, 100000)
strees<-mcmapply(pbtree, n = ntips, scale = 1 , SIMPLIFY = F, mc.cores = 10)
saveRDS(strees, "strees.rds")

s.trees<-readRDS("Slicing/strees/strees.rds")

library(castor)
slice_tree_ages<-function(tree, ages2cut){
  cutted.trees<-list()
  for(i in 1:length(ages2cut)){
    cutted.trees[[i]]<-trim_tree_at_height(tree,ages2cut[[i]])$tree 
  }  
  return(cutted.trees)
}
ages2cut<-seq(from=.3, to=max(branching.times(s.trees[[1]])), by=0.02)
stree1.sl<-slice_tree_ages(s.trees[[1]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=.3, to=max(branching.times(s.trees[[2]])), by=0.02)
stree2.sl<-slice_tree_ages(s.trees[[2]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=.3, to=max(branching.times(s.trees[[3]])), by=0.02)
stree3.sl<-slice_tree_ages(s.trees[[3]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=.3, to=max(branching.times(s.trees[[4]])), by=0.02)
stree4.sl<-slice_tree_ages(s.trees[[4]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=.3, to=max(branching.times(s.trees[[5]])), by=0.02)
stree5.sl<-slice_tree_ages(s.trees[[5]],ages2cut[1:length(ages2cut)])

saveRDS(stree1.sl,"Slicing/simtrees/s100/stree1.sl.rds")
saveRDS(stree2.sl,"Slicing/simtrees/s1k/stree2.sl.rds")
saveRDS(stree3.sl,"Slicing/simtrees/s10k/stree3.sl.rds")
saveRDS(stree4.sl,"Slicing/simtrees/s50k/stree4.sl.rds")
saveRDS(stree5.sl,"Slicing/simtrees/s100k/stree5.sl.rds")

s100_sl_stat<-readRDS("Slicing/simtrees/s100/s100_sum_stats.rds")
s100_sl_stat$taxon<-"s100"
s1k_sl_stat<-readRDS("Slicing/simtrees/s1k/s1k_sum_stats.rds")
s1k_sl_stat$taxon<-"s1k"
s10k_sl_stat<-readRDS("Slicing/simtrees/s10k/s10k_sum_stats.rds")
s10k_sl_stat$taxon<-"s10k"
s50k_sl_stat<-readRDS("Slicing/simtrees/s50k/")

s50k_sl_stat<-readRDS("Slicing/simtrees/s50k/")

sum_stats<-rbind(s100_sl_stat, s1k_sl_stat, s10k_sl_stat)

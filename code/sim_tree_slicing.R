library(phytools)
library(parallel)

# Pure-birth simulated trees ####
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

scale_edges<-function(phylo.list,old_edge,new_edge){
  for(i in 1:length(stree4.sl)){
    phylo.list[[i]]$edge.length[phylo.list[[i]]$edge.length<old_edge] <-new_edge
    phylo.list[[i]]<-castor:::extend_tree_to_height(phylo.list[[i]])$tree
  }
  return(phylo.list)
}
stree4.sl<-scale_edges(stree4.sl,1e-6, 1e-6)

ages2cut<-seq(from=.3, to=max(branching.times(s.trees[[5]])), by=0.02)
stree5.sl<-slice_tree_ages(s.trees[[5]],ages2cut[1:length(ages2cut)])
stree5.sl<-scale_edges(stree5.sl,1e-6, 1e-6)

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

sum_stats<-rbind(s100_sl_stat, s1k_sl_stat, s10k_sl_stat)

# Birth-death simulated trees, age conditioned ####
# Birds tree used as example
sum_stats<-readRDS("Slicing/simtrees/bd_bird_simtrees/bd_sim_sum_stats.rds")
sum_stats$taxon<-"bd_sim"
sum_stats$rel_age<-sum_stats$tree.max.age/113.25

#Add empirical bird tree stats
sum_stats<-rbind(bird_sl_stat,sum_stats)
sum_stats$taxon<-as.factor(sum_stats$taxon)

# Phylospace graph ####
library(plotly)
plot_ly(sum_stats, x = ~log(principal_eigenvalue), y = ~asymmetry, z = ~peakedness,
        type = "scatter3d", mode = "markers",
        marker = list(symbol = ~taxon , sizemode = 'area',  symbols = c("circle", "diamond"),
                      color = ~rel_age, size = ~ntips,
                      colorbar = list(title = 'Clade age (Myr)'), colorscale='Viridis', reversescale = T)) %>%
  layout(
    title = "Bird",
    scene = list( xaxis = list(title = "Ln Principal eigenvalue"),
                  yaxis = list(title = "Asymmetry"),
                  zaxis = list(title = "Peakedness"))
  )



plot_ly(data = sum_stats,
        x = ~log(principal_eigenvalue), y  = ~asymmetry, z = ~peakedness,
        type='scatter3d',mode = 'markers', symbol = ~taxon,  symbols = c("diamond", "circle"),
        size = ~ntips, sizemode = 'area') %>%
  add_markers(marker = list(
    color = ~ rel_age, colorbar = list(title = 'Clade age (Myr)'), colorscale='Viridis', reversescale = T)) %>%
  
  layout(
    title = "Bird",
    scene = list( xaxis = list(title = "Ln Principal eigenvalue"),
                  yaxis = list(title = "Asymmetry"),
                  zaxis = list(title = "Peakedness"))
  ) 


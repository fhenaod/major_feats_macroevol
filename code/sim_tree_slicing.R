library(phytools)
library(parallel)

## sim tree from bird empirical parameters ####
age<-113.25
lambda<-round(exp((-0.53356*log(age))+0.29817),3)
mu<-round(exp((-0.5257*log(age))-0.5016),3)

pars<-list(birth_rate_factor = lambda,
           death_rate_factor = mu)

bd_sim_tr<-generate_random_tree(pars, max_time = age)$tree

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
sum_stats<-rbind(sum_stats,bird_sl_stat)

# Phylospace graph 
library(plotly)
plot_ly(data = sum_stats,
        x = ~log(principal_eigenvalue[1:1018]), y  = ~asymmetry[1:1018], z = ~peakedness[1:1018],
        type='scatter3d', mode = 'markers', name = 'Simulated', 
        marker = list(symbol = 'circle', sizemode = 'area', size = ~ntips, 
                      color = ~rel_age, opacity = 0.7, 
                      colorbar = list(title = 'Relative age (Myr)', thickness = 15), colorscale='Viridis',
                      reversescale = T)) %>%
  
  add_trace(x = ~log(principal_eigenvalue[1019:1040]), y  = ~asymmetry[1019:1040], z = ~peakedness[1019:1040], 
            mode = 'lines+markers', name = 'Empirical', 
            line = list(color= "#EA3770", width = 4),
            marker = list(symbol = 'diamond', sizemode = 'area', size = ~ntips, 
                          color = ~rel_age)) %>%
  layout(
    title = "Bird",
    scene = list(xaxis = list(title = "λ*"),
                 yaxis = list(title = "ψ"),
                 zaxis = list(title = "η")),
    legend = list(orientation = 'h')
  ) 

# No age
plot_ly(data = sum_stats,
        x = ~log(principal_eigenvalue[1:1018]), y  = ~asymmetry[1:1018], z = ~peakedness[1:1018],
        type='scatter3d', mode = 'markers', name = 'Simulated', 
        marker = list(symbol = 'circle', sizemode = 'area', size = ~ntips, 
                      color = '#2ca02c', opacity = 0.7)) %>%
  
  add_trace(x = ~log(principal_eigenvalue[1019:1040]), y  = ~asymmetry[1019:1040], z = ~peakedness[1019:1040], 
            mode = 'lines+markers', name = 'Empirical', 
            line = list(color= "#EA3770", width = 4),
            marker = list(symbol = 'diamond', sizemode = 'area', size = ~ntips, 
                          color = '#ff7f0e')) %>%
  layout(
    title = "Bird",
    scene = list(xaxis = list(title = "λ*"),
                 yaxis = list(title = "ψ"),
                 zaxis = list(title = "η")),
    legend = list(orientation = 'h')
  ) 

e_tips <- sapply(e.trees, Ntip)
e_ages <- sapply(e.trees, branching.times) %>% sapply(., max) %>% round(2)


####
## Simulate trees with constant net diversification or turnover #####
####

# phytools ####
# conditioning on net divers
birth <- c(.7)
r <- c(.1, .25, .7)
death <- abs(birth - r)

sim_r_tr <- list()
for(i in 1:length(r)){
  strs <- pbtree(b = birth, d = death[i], n = 1000, scale = 100, 
                 nsim = 10, type = "continuous", extant.only = T)
  names(strs) <- rep(r[i], length(strs)) %>% paste0("r",.)
  
  sim_r_tr <- c(sim_r_tr, strs)
}

# conditionin on extinction fract
birth <- c(.9)
eps <- c(.01, .1, .25, .7)
death <- c(eps/birth) %>% round(2)

sim_r_tr2 <- list()
for(i in 1:length(eps)){
  strs <- pbtree(b = birth, d = death[i], n = 1000, scale = 100, 
                 nsim = 10, type = "continuous", extant.only = T)
  names(strs) <- rep(death[i], length(strs)) %>% paste0("eps",.)
  
  sim_r_tr2 <- c(sim_r_tr2, strs)
}

names(sim_r_tr2) %>% table()
sapply(sim_r_tr2, class)
sapply(sim_r_tr2, Ntip) %>% hist()

plot(sim_r_tr2[[2]], show.tip.label = F)

# TreeSim ####
library(TreeSim)
# conditionin on extinction fract
birth <- c(.9)
eps <- c(.01, .1, .25, .7)
death <- c(eps/birth) %>% round(2)

sim_r_tr3 <- list()
for(i in 1:length(eps)){
  strs <- sim.bd.taxa(n = 1000, numbsim = 10, 
                      lambda = birth, mu = death[i], 
                      frac = 1, complete = F, stochsampling = FALSE)
  names(strs) <- rep(death[i], length(strs)) %>% paste0("eps_",.)
  
  sim_r_tr3 <- c(sim_r_tr3, strs)
}

names(sim_r_tr3) %>% table()

sim_r_tr3_sc <- sim_r_tr3
for(i in 1:length(sim_r_tr3)){
  sim_r_tr3_sc[[i]]$edge.length <- sim_r_tr3[[i]]$edge.length*20
}
lapply(sim_r_tr3, branching.times) %>% 
  sapply(., max) %>% round(2)

lapply(sim_r_tr3_sc, branching.times) %>% 
  sapply(., max) %>% round(2) %>% summary()

# slicing and adding to list, not necessry for the general cluster code
sl_sim_trs <- list()
for(i in 1:length(sim_r_tr3_sc)){
  strs <- slice_tree_ages(sim_r_tr3_sc[[i]], 
                  ages2cut = seq(from = 5, to = max(branching.times(sim_r_tr3_sc[[i]])), 
                                 by = 5))
  names(strs) <- names(sim_r_tr3_sc)[[i]]
  
  sim_r_tr3 <- c(sim_r_tr3, strs)
}

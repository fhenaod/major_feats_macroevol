library(phytools)
library(castor)
library(tidyverse)

tree <- readRDS("data_megaPhylos/seed_noded_tre.rds")

# randomly sample trees from a megatree based on age with stem edge ####
random_tree_samp=function(t, t_height, n_samps){
  l_trees<-list()
  t_brake_age<-c()
  ed<-t$edge
  inter<-ed[-which(ed[,2]<= Ntip(t)), ]
  s_n_edge<-sample(1:dim(inter)[1], n_samps, replace = F)
  for(i in 1:length(s_n_edge)){
  s_edge<-inter[s_n_edge[i],]
  s_edge_len<-t$edge.length[which(grepl(s_edge[2], ed[,2]))]
  s_edge_point<-sample(seq(from = 0, to = s_edge_len, by = .2),1)
  new_clade<-extract.clade(t,s_edge[2])
  new_len<-(t_height-s_edge_point)
  
  if(new_len<=0) {
    } else {
    trim_tre<-trim_tree_at_height(new_clade, new_len)$tree
    new_tre<-trim_tre
    new_tre$edge<-matrix(data = c(Ntip(new_tre)+1, new_tre$edge[,1]+1, Ntip(new_tre)+2, ifelse(new_tre$edge[,2]>Ntip(new_tre),new_tre$edge[,2]+1, new_tre$edge[,2])), ncol = 2)
    new_tre$Nnode<-new_tre$Nnode+1
    new_tre$edge.length<-c(s_edge_point,new_tre$edge.length)
    l_trees[[i]]<-new_tre
    t_brake_age[i]<-get_tree_span(new_clade)$max_distance
  }
    }
  return(list(l_trees, t_brake_age))
}

rand_trees_raw<-random_tree_samp(tree, t_height = 10, n_samps = 20)
rand_trees<-plyr::compact(rand_trees_raw[[1]])
orig_brake_age<-rand_trees_raw[[2]][!is.na(rand_trees_raw[[2]])]

rand_trees_f<-rand_trees[sapply(rand_trees, function(x) Ntip(x) >= 4)]
orig_brake_age_f<-orig_brake_age[sapply(rand_trees, function(x) Ntip(x) >= 4)]
  
nt<-sapply(rand_trees_f, Ntip)
length(which(nt >= 4))

clad <- "seed"
saveRDS(rand_trees_f, paste0("self_sim/", clad,"_50_trees.rds"))
saveRDS(orig_brake_age_f, paste0("self_sim/", clad, "_50_brake_ages.rds"))

par(mfrow = c(3,3))
sapply(sample(rand_trees, 9), plot, cex = .7, no.margin = TRUE)
sapply(sample(rand_trees_f, 9), plot, cex = .7, no.margin = TRUE)


# loop over
clad <- "seed"
ages2cut <- c(10, 20, 30, 40, 50)
nsamps <- c(100, 80, 50, 30, 10)
for(i in 1:length(ages2cut)){
  rand_trees_raw<-random_tree_samp(tree, t_height = ages2cut[i], n_samps = nsamps[i])
  rand_trees<-plyr::compact(rand_trees_raw[[1]])
  orig_brake_age<-rand_trees_raw[[2]][!is.na(rand_trees_raw[[2]])]
  
  rand_trees_f<-rand_trees[sapply(rand_trees, function(x) Ntip(x) >= 4)]
  orig_brake_age_f<-orig_brake_age[sapply(rand_trees, function(x) Ntip(x) >= 4)]
  
  nt<-sapply(rand_trees_f, Ntip)
  length(which(nt >= 4))
  
  saveRDS(rand_trees_f, paste0(clad,"_",ages2cut[i],"_trees.rds"))
  saveRDS(orig_brake_age_f, paste0(clad,"_",ages2cut[i],"_brake_ages.rds"))
}



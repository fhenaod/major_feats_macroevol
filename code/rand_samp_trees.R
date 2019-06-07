library(phytools)
library(castor)

## sim trees from bird empirical parameters ####
age<-113.25
lambda<-round(exp((-0.53356*log(age))+0.29817),3)
mu<-round(exp((-0.5257*log(age))-0.5016),3)

pars<-list(birth_rate_factor = lambda,
           death_rate_factor = mu)

bd_sim_tr<-generate_random_tree(pars, max_time = age)$tree

# function to randomly re-sample trees from a megatree based on age ####
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

rand_trees_raw<-random_tree_samp(bd_sim_tr, t_height = 5, n_samps = 1900)
rand_trees<-plyr::compact(rand_trees_raw[[1]])
orig_brake_age<-rand_trees_raw[[2]][!is.na(rand_trees_raw[[2]])]

rand_trees_f<-rand_trees[sapply(rand_trees, function(x) Ntip(x) >= 4)]
orig_brake_age_f<-orig_brake_age[sapply(rand_trees, function(x) Ntip(x) >= 4)]
  
nt<-sapply(rand_trees_f, Ntip)
length(which(nt >= 4))
saveRDS(rand_trees_f, "self_sim/chon_10_trees.rds")
saveRDS(orig_brake_age_f, "self_sim/chon_5_brake_ages.rds")

par(mfrow = c(3,3))
sapply(sample(rand_trees,9), plot, cex = 1, no.margin = TRUE)

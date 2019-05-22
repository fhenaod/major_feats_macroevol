library(phytools)
library(castor)

random_tree_samp=function(t, t_height, n_samps){
  l_trees<-list()
  ed<-t$edge
  inter<-ed[-which(ed[,2]<=Ntip(t)), ]
  for(i in 1:n_samps){
  s_n_edge<-sample(1:dim(inter)[1],1)
  s_edge<-inter[s_n_edge,]
  s_edge_len<-t$edge.length[which(grepl(s_edge[2], ed[,2]))]
  s_edge_point<-sample(seq(from = 0, to = s_edge_len, by = .2),1)
  new_clade<-extract.clade(t,s_edge[2])
  new_len<-(t_height-s_edge_point)
  
  if(new_len<=0) {
    } else {
    trim_tre<-trim_tree_at_height(new_clade, new_len)$tree
    new_tre<-trim_tre
    new_tre$root.edge<-s_edge_point
    l_trees[[i]]<-new_tre
  }
    }
  return(l_trees)
}

rand_trees<-random_tree_samp(J2012, t_height = 5, n_samps = 200)
rand_trees<-plyr::compact(rand_trees)
rand_trees<-rand_trees[sapply(rand_trees, function(x) Ntip(x) >=4)]
nt<-sapply(rand_trees, Ntip)
length(which(nt>=4))
saveRDS(rand_trees, "self_sim/bird_5.rds")

par(mfrow=c(3,3))
sapply(sample(rand_trees,9), plot, cex = 1, no.margin = TRUE, root.edge = T)

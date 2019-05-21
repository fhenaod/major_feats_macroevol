library(phytools)
library(castor)
t<-pbtree(n = 30, b = 1.3, scale = 10, extant.only = T)

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

rand_trees<-random_tree_samp(t, t_height = 6, n_samps = 100)
plot(rand_trees)

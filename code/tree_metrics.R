library(ape)

tree_metrics=function(tt){
  ntips<-c()
  tree.min.age<-c()
  tree.max.age<-c()
  gamma.stat<-c()
  trees.metrics<-c()
  for (i in 1:length(tt)){
    phylogs<-tt[[i]]
    ntips[i]<-Ntip(phylogs)
    tree.min.age[i]<-min(branching.times(phylogs))
    tree.max.age[i]<-max(branching.times(phylogs))
    gamma.stat[i]<-gammaStat(phylogs)
    trees.metrics<-data.frame(ntips,tree.min.age,tree.max.age,gamma.stat)
  }  
  return(trees.metrics)
}
tree_metrics_sum<-round(tree_metrics(bird.sl),2)

source('code/get_DR.R')
trees_dr<-sapply(bird.sl,get_DR)
trees_mean_dr<-sapply(trees_dr,mean)

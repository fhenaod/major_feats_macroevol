library(ape)
br_len.t_stats=function(trees){
  df<-data.frame()
  br.len_min<-c()
  br.len_mean<-c()
  br.t_mean<-c()
  for (i in 1:length(trees)){
    tree<-trees[[i]]
    br.len_min[i]<-min(tree$edge.length)
    br.len_mean[i]<-mean(tree$edge.length)
    br.t_mean[i]<-mean(branching.times(tree))
  }
  df<-data.frame(br.len_min, br.len_mean, br.t_mean)
  return(df)
}
branch_stats<-br_len.t_stats(x.sl)

saveRDS(branch_stats,"Slicing/branch_stats/x_branch_stats.rds")

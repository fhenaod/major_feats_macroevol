library(ape)
br_len.t_stats=function(trees){
  df<-data.frame()
  br.len_mean<-c()
  br.t_mean<-c()
  for (i in 1:length(trees)){
    tree<-trees[[i]]
    br.len_mean[i]<-mean(tree$edge.length)
    br.t_mean[i]<-mean(branching.times(tree))
  }
  df<-round(data.frame(br.len_mean, br.t_mean),3)
  return(df)
}
branch_stats<-br_len.t_stats(squa.sl)

saveRDS(branch_stats,"Slicing/branch_stats/squa_branch_stats.rds")

library(apTreeshape)
library(RPANDA)
library(parallel)

seed_ords<-readRDS("seed_ords_trees.rds")

# Tree metrics ####
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
tree_metrics_sum<-round(tree_metrics(seed_ords),2)
saveRDS(tree_metrics_sum,"seed_ords_metrics.rds")

# DR ####
source('get_DR.R')
trees_dr<-sapply(seed_ords,get_DR)
trees_mean_dr<-sapply(trees_dr,mean)
saveRDS(trees_mean_dr,"seed_ords_mean_dr.rds")

# Imbalance metrics ####
imbalance_metrics=function(e.trees,n.mc){
  tree.ts<-c()
  
  shape.yule<-c()
  colles.yule<-c()
  p.coless.t.y.less<-c()
  p.coless.t.y.great<-c()
  sackin.yule<-c()
  p.lt.yule<-c()
  
  shape.pda<-c()
  colles.pda<-c()
  p.coless.t.pda.less<-c()
  p.coless.t.pda.great<-c()
  sackin.pda<-c()
  p.lt.pda<-c()
  
  imbalance_metrics<-c()
  for (i in 1:length(e.trees)){
    tr<-e.trees[[i]]
    
    if (length(tr$tip.label)<= 4) {
      shape.yule[i]<-NA
      colles.yule[i]<-NA
      p.coless.t.y.less[i]<-NA
      p.coless.t.y.great[i]<-NA
      sackin.yule[i]<-NA
      p.lt.yule[i]<-NA
      shape.pda[i]<-NA
      colles.pda[i]<-NA
      p.coless.t.pda.less[i]<-NA
      p.coless.t.pda.great[i]<-NA
      sackin.pda[i]<-NA
      p.lt.pda[i]<-NA
    } else 
    {
      tree.ts<-as.treeshape(tr)
      
      shape.yule[i]<-shape.statistic(tree.ts, norm = "yule")
      colles.yule[i]<-colless(tree.ts, norm = "yule")
      sackin.yule[i]<-sackin(tree.ts, norm = "yule")
      
      shape.pda[i]<-shape.statistic(tree.ts, norm = "pda")
      colles.pda[i]<-colless(tree.ts,norm = "pda")
      sackin.pda[i]<-sackin(tree.ts, norm = "pda")
      
      df<-data.frame(colless.test(tree.ts, model = "yule", alternative = "less", n.mc = n.mc))
      p.coless.t.y.less[i]<-df$p.value
      df1<-data.frame(colless.test(tree.ts, model = "yule", alternative = "greater", n.mc = n.mc))
      p.coless.t.y.great[i]<-df1$p.value
      df2<-data.frame(likelihood.test(tree.ts, model = "yule", alternative = "two.sided"))
      p.lt.yule[i]<-df2$p.value
      
      df3<-data.frame(colless.test(tree.ts, model = "pda", alternative = "less", n.mc = n.mc))
      p.coless.t.pda.less[i]<-df3$p.value
      df4<-data.frame(colless.test(tree.ts, model = "pda", alternative = "greater", n.mc = n.mc))
      p.coless.t.pda.great[i]<-df4$p.value
      df5<-data.frame(likelihood.test(tree.ts, model = "pda", alternative = "two.sided"))
      p.lt.pda[i]<-df5$p.value
    }
    
    imbalance.metrics<-data.frame(shape.yule,colles.yule,p.coless.t.y.less,p.coless.t.y.great,sackin.yule,
                                  p.lt.yule,shape.pda,colles.pda,p.coless.t.pda.less,p.coless.t.pda.great,sackin.pda,p.lt.pda)
  }
  
  return(imbalance.metrics)
}
imbalance.metrics<-imbalance_metrics(seed_ords,1000)
saveRDS(imbalance.metrics,"seed_ords_imbalance.metrics.rds")

# RPANDA ####
# Estimate tree's spectrum from a list of trees
trees_spectR<-mclapply(seed_ords,spectR, mc.cores = 10)
saveRDS(trees_spectR, "seed_ords_spectR.rds")

# Extract tree's spectrum summary stats ####
extract_spect=function(lap){
  principal_eigenvalue<-c()
  asymmetry<-c()
  peakedness<-c()
  modalities<-c()
  
spec_sum<-data.frame()  
  for(i in 1:length(lap)){
    if (is.null(trees_spectR[[i]])) {
      principal_eigenvalue[i]<-NA
      asymmetry[i]<-NA
      peakedness[i]<-NA
      modalities[i]<-NA
      }
    else {
      principal_eigenvalue[i]<-lap[[i]]$principal_eigenvalue
      asymmetry[i]<-lap[[i]]$asymmetry
      peakedness[i]<-lap[[i]]$peakedness
      modalities[i]<-lap[[i]]$eigengap}
    
  spec_sum<-data.frame(principal_eigenvalue,asymmetry,peakedness,modalities)
  }
return(spec_sum)
}
trees_spec_sum<-extract_spect(trees_spectR)
saveRDS(trees_spec_sum, "seed_ords_spec_sum.rds")

sum_stats<-data.frame(tree_metrics_sum, trees_mean_dr, ln_dr=log(trees_mean_dr), imbalance.metrics, trees_spec_sum)
saveRDS(sum_stats,"seed_ords_sum_stats.rds")

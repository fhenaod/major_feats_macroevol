library(apTreeshape)
library(RPANDA)
library(parallel)

clad <- "amph"

clad_trees <- readRDS(paste0("../data_megaPhylos/", clad,"_trees_clean.rds"))
clad_trees <- readRDS(paste0("data_megaPhylos/", clad,"_trees_clean.rds"))

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
tree_metrics_sum<-round(tree_metrics(clad_trees),2)
saveRDS(tree_metrics_sum, paste0("output/", clad , "_tree_metrics.rds"))

# DR ####
source('code/get_DR.R')
trees_dr<-sapply(clad_trees, get_DR)
#trees_dr<-mcmapply(get_DR, ts, mc.cores = 2)
trees_mean_dr<-sapply(trees_dr,mean)
saveRDS(trees_mean_dr, paste0("output/", clad , "_trees_mean_dr.rds"))

# Branch length stats ####
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
branch_stats<-br_len.t_stats(clad_trees)
saveRDS(branch_stats,"output/_branch_stats.rds")

# ML-Beta ####

# from a list of trees get the maximum likelihood estimate of the Beta-splitting model
# and CI:95% with bootstrap replicates or ML profile
trees<-bird.sl
get_ml_beta=function(trees, n_bootstrap){
  beta<-c()
  b_low_ci<-c()
  b_up_ci<-c()
  for(i in 1:length(trees)){
    if(Ntip(trees[[i]])<=2) {
      beta[i]<-NA
      b_low_ci[i]<-NA
      b_up_ci[i]<-NA
    } else {
      if(Ntip(trees[[i]])<=50) {
        ml_beta<-maxlik.betasplit(trees[[i]], confidence.interval = "bootstrap", conf.level = 0.95, size.bootstrap = n_bootstrap)
        beta[i]<-ml_beta$max_lik
        b_low_ci[i]<-ml_beta$conf_interval[[1]]
        b_up_ci[i]<-ml_beta$conf_interval[[2]]
      } else {
        ml_beta<-maxlik.betasplit(trees[[i]], confidence.interval = "profile", conf.level = 0.95, size.bootstrap = n_bootstrap)
        beta[i]<-ml_beta$max_lik
        b_low_ci[i]<-ml_beta$conf_interval[[1]]
        b_up_ci[i]<-ml_beta$conf_interval[[2]]
      }
    }
  }
  df<-round(data.frame(beta, b_low_ci, b_up_ci),3)
  return(df)
}

bird_beta<-get_ml_beta(bird.sl)

# parallel version
bird_beta<-mclapply(list(bird.sl),  function(x) get_ml_beta(x, 1000), mc.cores = 3)[[1]]
saveRDS(bird_beta, file = "bird_ml_beta.rds")

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
imbalance.metrics<-imbalance_metrics(clad_trees,1000)
#imbalance.metrics<-mclapply(list(ts), function(x) imbalance_metrics(x, 100), mc.cores=3)[[1]]
saveRDS(imbalance.metrics, paste0("output/", clad , "_imbalance.metrics.rds"))

# RPANDA ####
# Estimate tree's spectrum from a list of trees
trees_spectR<-mclapply(clad_trees[1], spectR, mc.cores = 2)
saveRDS(trees_spectR, paste0("output/", clad , "_tree_spectR.rds"))

# Extract tree's spectrum summary stats #
extract_spect=function(lap){
  principal_eigenvalue<-c()
  asymmetry<-c()
  peakedness<-c()
  modalities<-c()
  
  spec_sum<-data.frame()  
  for(i in 1:length(lap)){
    principal_eigenvalue[i]<-lap[[i]]$principal_eigenvalue
    asymmetry[i]<-lap[[i]]$asymmetry
    peakedness[i]<-lap[[i]]$peakedness
    modalities[i]<-lap[[i]]$eigengap
    spec_sum<-data.frame(principal_eigenvalue,asymmetry,peakedness,modalities)
  }
  return(spec_sum)
}
trees_spec_sum<-extract_spect(trees_spectR)
saveRDS(trees_spec_sum, paste0("output/", clad , "_trees_spec_sum.rds"))

# Estimate BIC trees modality number 
bic.compare=function(tr,e.gap){
  bicc<-c() 
  bic.test.r<-c()
  bicRatio<-c()
  df<-data.frame()
  for (i in 1:length(tr)){
    bicc<-BICompare(tr[[i]],e.gap[[i]])
    bic.test.r[i]<-(bicc$BIC_test$`tree BIC`/bicc$BIC_test$`random BIC`)
    bicRatio[i]<-bicc$`BSS/TSS`
    df<-data.frame(bic.test.r,bicRatio)
  }
  return(df)
}
#sum_bic_compare<-bic.compare(bird.sl,trees_spec_sum$modalities)


# save results ####
sum_stats<-data.frame(tree_metrics_sum, trees_mean_dr, ln_dr=log(trees_mean_dr), imbalance.metrics, trees_spec_sum)
saveRDS(sum_stats, paste0("output/", clad , "_sum_stats.rds"))

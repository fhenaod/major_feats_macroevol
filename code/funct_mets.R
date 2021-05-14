
# prepare trees
get_genus=function(names_sp){
  query<-c()  
  for(i in 1:length(names_sp)){
    query[i]<-names_sp[[i]][1]
  }
  return(query)
}

name_genus_nodes=function(tree, names2nodes){
  for(i in 1:length(names2nodes)){
    tips<-tree$tip.label[grep(paste0("\\b", names2nodes[i] ,"\\b"), tree$tip.label)]
    if(length(tips)!=1){
      mrca_node<-getMRCA(tree, tip = tips)
      tree$node.label[mrca_node-Ntip(tree)]<-names2nodes[i]
    } else {
      
    }
  }
  return(tree)
}

name_htaxa_nodes=function(tree, m){
  fams<-unique(m$family)
  for(i in 1:length(fams)){
    fam_g<-as.character(m[grep(fams[i],m$family),]$genus)
    mrca_node<-get_mrca_of_set(tree, fam_g)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(fams[i])
    } 
  }
  
  ords<-unique(m$order)
  for(j in 1:length(ords)){
    ord_f<-as.character(m[grep(ords[j],m$order),]$family)
    mrca_node<-get_mrca_of_set(tree, ord_f)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(ords[j])
    } 
  }
  
  clas<-unique(m$class)
  for(k in 1:length(ords)){
    cla_o<-as.character(m[grep(clas[k],m$class),]$order)
    mrca_node<-get_mrca_of_set(tree, cla_o)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(clas[k])
    } 
  }
  
  return(tree)
}

# sampling functions
slice_tree_ages<-function(tree, ages2cut){
  cutted.trees<-list()
  for(i in 1:length(ages2cut)){
    cutted.trees[[i]]<-trim_tree_at_height(tree,ages2cut[[i]])$tree 
  }  
  return(cutted.trees)
}

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
  if (length(e.trees) == 1) {e.trees<-list(e.trees=e.trees)
  } else 
  {
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
  }
  
  return(imbalance.metrics)
}

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

# metric functions
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

imbalance_metrics=function(e.trees, n.mc){
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

# from a list of trees get the maximum likelihood estimate of the Beta-splitting model
# and CI:95% with bootstrap replicates or ML profile
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
  df<-data.frame(beta, b_low_ci, b_up_ci) %>% 
    mutate_if(is.numeric, round, 3)
  return(df)
}

extract_spect=function(trees_spectR){
  principal_eigenvalue<-c()
  asymmetry<-c()
  peakedness<-c()
  modalities<-c()
  
  spec_sum<-data.frame()
  for(i in 1:length(trees_spectR)){
    if(is.null(trees_spectR[[i]]) | class(trees_spectR[[i]]) == "try-error") {
      principal_eigenvalue[i]<-NA
      asymmetry[i]<-NA
      peakedness[i]<-NA
      modalities[i]<-NA
    }
    else {
      principal_eigenvalue[i]<-trees_spectR[[i]]$principal_eigenvalue
      asymmetry[i]<-trees_spectR[[i]]$asymmetry
      peakedness[i]<-trees_spectR[[i]]$peakedness
      modalities[i]<-trees_spectR[[i]]$eigengap}
    
    spec_sum<-data.frame(principal_eigenvalue,asymmetry,peakedness,modalities)
  }
  return(spec_sum)
}

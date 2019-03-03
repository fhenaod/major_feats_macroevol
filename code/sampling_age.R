library(diversitree)
library(rncl)
source('code/ageGenerator.R', chdir = TRUE)

j.phylo<-read_newick_phylo("data_megaPhylos/tree_J2012_.txt")
r.phylo<-read_newick_phylo("data_megaPhylos/tree_R2018.cr_.txt")
s.phylo<-read_newick_phylo("data_megaPhylos/tree_S2018.cr.bi_.txt")

j.ages<-rep(25,200)
r.ages<-rep(80,200)
s.ages<-rep(40,200)

age_subtrees=function(phylo,ages,tolerance,N){
  sampled.trees<-list()

  for (e in 1:N){
    new_age_generator<-ageGenerator(ages=ages,tree=phylo,tolerance=tolerance, fixed = T)
    new_age_generator$newGenera<-new_age_generator$newGenera[!sapply(new_age_generator$newGenera, anyNA)] 
    
    for (i in 1:length(new_age_generator$newGenera)){
      clade.tree<-get_subtree_with_tips(phylo,new_age_generator$newGenera[[i]])$subtree
      if (Ntip(clade.tree)<=19) {next(i+1)} else 
      {sampled.trees[[i]]<-clade.tree}
    }  
    
  }
  
  return(sampled.trees)
}

j.sampled<-age_subtrees(j.phylo,j.ages, 10, 1)
j.sampled<-j.sampled[which(!j.sampled=="NULL")]

r.sampled<-age_subtrees(r.phylo,r.ages, 10, 1)
r.sampled<-r.sampled[which(!r.sampled=="NULL")]

s.sampled<-age_subtrees(r.phylo,s.ages, 10, 1)
s.sampled<-s.sampled[which(!s.sampled=="NULL")]

##
tree_metrics=function(tt){
  phylogs<-c()
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

j.sampled.metrics<-tree_metrics(j.sampled)
r.sampled.metrics<-tree_metrics(r.sampled)
s.sampled.metrics<-tree_metrics(s.sampled)

# 
source('code/Rpanda.R', chdir = TRUE)

## Jetz bird-tree
j.trees_spectR<-mclapply(j.sampled,spectR, mc.cores = 2)
j.trees_spec_sum<-extract_spect(j.trees_spectR)
j.table<-data.frame(j.sampled.metrics, j.trees_spec_sum)

summary(lm(log(principal_eigenvalue)~log(tree.max.age), data=j.table))
summary(lm(log(asymmetry)~log(tree.max.age), data=j.table))
summary(lm(log(peakedness)~log(tree.max.age), data=j.table))
summary(lm(log(modalities)~log(tree.max.age), data=j.table))

par(mfrow=c(2,2))
visreg(lm(log(principal_eigenvalue)~log(tree.max.age), data=j.table),xlab= "Ln Clade age (My)", ylab= "Ln Principal eigenvalue")
visreg(lm(log(asymmetry)~log(tree.max.age), data=j.table), xlab= "Ln Clade age (My)", ylab= "Ln Asymmetry")
visreg(lm(log(peakedness)~log(tree.max.age), data=j.table), xlab= "Ln Clade age (My)", ylab= "Ln Peakedness")
visreg(lm(log(modalities)~log(tree.max.age), data=j.table), xlab= "Ln Clade age (My)", ylab= "Ln Modalities")

## Smith angiosperms-tree
s.trees_spectR<-mclapply(s.sampled,spectR, mc.cores = 2)
s.trees_spec_sum<-extract_spect(s.trees_spectR)
s.table<-data.frame(s.sampled.metrics, s.trees_spec_sum)

summary(lm(log(principal_eigenvalue)~log(tree.max.age), data=s.table))
summary(lm(log(asymmetry)~log(tree.max.age), data=s.table))
summary(lm(log(peakedness)~log(tree.max.age), data=s.table))
summary(lm(log(modalities)~log(tree.max.age), data=s.table))

par(mfrow=c(2,2))
visreg(lm(log(principal_eigenvalue)~log(tree.max.age), data=s.table), xlab= "Ln Clade age (My)", ylab= "Ln Principal eigenvalue")
visreg(lm(log(asymmetry)~log(tree.max.age), data=s.table), xlab= "Ln Clade age (My)", ylab= "Ln Asymmetry")
visreg(lm(log(peakedness)~log(tree.max.age), data=s.table), xlab= "Ln Clade age (My)", ylab= "Ln Peakedness")
visreg(lm(log(modalities)~log(tree.max.age), data=s.table), xlab= "Ln Clade age (My)", ylab= "Ln Modalities")

## Rabosky fish-tree
r.trees_spectR<-mclapply(r.sampled,spectR, mc.cores = 2)
r.trees_spec_sum<-extract_spect(r.trees_spectR)
r.table<-data.frame(r.sampled.metrics, r.trees_spec_sum)

summary(lm(log(principal_eigenvalue)~log(tree.max.age), data=r.table))
summary(lm(log(asymmetry)~log(tree.max.age), data=r.table))
summary(lm(log(peakedness)~log(tree.max.age), data=r.table))
summary(lm(log(modalities)~log(tree.max.age), data=r.table))

par(mfrow=c(2,2))
visreg(lm(log(principal_eigenvalue)~log(tree.max.age), data=r.table), xlab= "Ln Clade age (My)", ylab= "Ln Principal eigenvalue")
visreg(lm(log(asymmetry)~log(tree.max.age), data=r.table), xlab= "Ln Clade age (My)", ylab= "Ln Asymmetry")
visreg(lm(log(peakedness)~log(tree.max.age), data=r.table), xlab= "Ln Clade age (My)", ylab= "Ln Peakedness")
visreg(lm(log(modalities)~log(tree.max.age), data=r.table), xlab= "Ln Clade age (My)", ylab= "Ln Modalities")


library(diversitree)
library(tidyverse)
library(rncl)
library(ggpubr)
source('code/ageGenerator.R', chdir = TRUE)
source('code/funct_mets.R', chdir = TRUE)

# node sample N subtrees given a min. ntips and tolerance, from a mega phylo  ####
age_subtrees=function(phylo, ages, tolerance, min_tip, N){
  sampled.trees<-list()
  phylo$node.label <- NULL
    new_age_generator<-ageGenerator(tree = phylo, ages = rep(ages, N),  
                                    tolerance = tolerance, fixed = T)
    if(is.na(new_age_generator$newAges)[1]==T) stop("Nodes not found under current conditions")
    new_age_generator$newGenera<-new_age_generator$newGenera[!sapply(new_age_generator$newGenera, anyNA)] 
    
    for (i in 1:length(new_age_generator$newGenera)){
      clade.subtree<-castor::get_subtree_with_tips(phylo,new_age_generator$newGenera[[i]])
      clade.subtree$subtree$old.stem.len <- 
        phylo$edge.length[match(setdiff(clade.subtree$new2old_clade, clade.subtree$new2old_tip)[1], phylo$edge[,2])]
      clade.subtree$subtree$root_shift <- clade.subtree$root_shift
      if (Ntip(clade.subtree$subtree)<min_tip) {next(i+1)} else 
      {sampled.trees[[i]]<-clade.subtree$subtree}
    } 
      return(sampled.trees)
}

clad <- "seed"
tree <- read_newick_phylo("data_megaPhylos/tree_S2018.cr.pr.bi_.txt")

ages2cut <- c(10, 20, 30, 40, 50)
ns2sampl <- c(500, 1000, 1000, 1000, 1250)
for(i in 1:length(ages2cut)){
  ssnt <- age_subtrees(tree, ages = ages2cut[i], tolerance = 5, min_tip = 10, N = ns2sampl[i])
  ssnt <- ssnt[which(!ssnt=="NULL")]
  print(paste0("Ages:", ages2cut[i], "; Ntrees = ", length(ssnt)))
  saveRDS(ssnt, paste0(clad,"_",ages2cut[i],"_trees.rds"))
}

# constant-age clade  metrics####
j.phylo<-read_newick_phylo("data_megaPhylos/tree_J2012_.txt")
r.phylo<-read_newick_phylo("data_megaPhylos/tree_R2018.cr_.txt")
s.phylo<-read_newick_phylo("data_megaPhylos/tree_S2018.cr.pr.bi_.txt")

j.sampled<-age_subtrees(j.phylo, ages = 10 , tolerance = 2, 
                        N = 3, min_tip = 2)
j.sampled<-j.sampled[which(!j.sampled=="NULL")]

r.sampled<-age_subtrees(r.phylo, ages = 80 , tolerance = 5, N = 200, min_tip = 20)
r.sampled<-r.sampled[which(!r.sampled=="NULL")]

s.sampled<-age_subtrees(r.phylo, ages = 40, tolerance = 5, N = 200, min_tip = 20)
s.sampled<-s.sampled[which(!s.sampled=="NULL")]

j.sampled.metrics<-tree_metrics(j.sampled)
r.sampled.metrics<-tree_metrics(r.sampled)
s.sampled.metrics<-tree_metrics(s.sampled)

# Jetz bird-tree
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

# random node-age sampling proportional to empirical distribution ####
mega_tree <- read.tree("data_megaPhylos/tree_V2019.cr.pr_.txt")
clade <- "agar"
rank <- "ords"
clade_rank <- readRDS(paste0("rank_sampling/",clade, "_", rank, "/sliced_trees/", clade, "_", rank, "_trees.rds"))

clade_rank %>% tree_metrics() %>% 
  ggplot(aes(x = tree.max.age, y = (ntips))) + geom_point() + 
  theme_classic() + geom_smooth(method = "lm")

tb_ms <- clade_rank %>% tree_metrics()
p_hist <- hist(tb_ms$tree.max.age, 
               breaks = seq(0, round(max(tb_ms$tree.max.age), -1)+20, 20), 
               main = paste0(clade,"_", rank))

ss_tt <- list()
for(i in 2:length(p_hist$breaks)){
  if(p_hist$counts[i-1]==0){next(i+1)} else{
    s_t <- age_subtrees(mega_tree, ages = p_hist$breaks[i], 
                        tolerance = 10, N = p_hist$counts[i-1], min_tip = 2)
    ss_tt <- c(ss_tt, s_t)
  }
}

sapply(ss_tt, is.null) %>% table()

ss_tt %>% tree_metrics()

ss_tt_cc <- compact(ss_tt)
ss_tt_cc %>% sapply(is.null) %>% table()
ss_tt_cc %>% tree_metrics()

par(mfrow = c(1, 2)) 
plot(p_hist, main = paste0("emp: ",clade,"_", rank))
hsp <- ss_tt_cc %>% tree_metrics() %>% pull(tree.max.age) %>% 
  hist(breaks = seq(0, round(max(.), -1)+20, 20), 
       main = paste0("samp: ",clade,"_", rank))
par(mfrow = c(1, 1))

saveRDS(ss_tt, paste0("rand_age_rank/",clade, "_", rank,"_rdm_trs.rds"))

##### empirical rank named clade metrics compared with aprox. age random clades #####
mets <- rank_sum_stats %>% select(ntips, tree.max.age, trees_mean_dr, 
                                         shape.yule, colles.yule, sackin.yule,
                                         shape.pda, colles.pda, sackin.pda,
                                         principal_eigenvalue, asymmetry, peakedness)
p_wik_tb <- list()
for(i in 1:length(mets)){
  d_tb_tp <- rbind(
    rank_sum_stats %>% mutate(type = "empirical") %>% 
      select(ntips, tree.max.age, trees_mean_dr, 
             shape.yule, colles.yule, sackin.yule,
             shape.pda, colles.pda, sackin.pda,
             principal_eigenvalue, asymmetry, peakedness,
             taxon, rank, type),
    
    rank_rdm_sum %>% mutate(type = "random_age_rank") %>% 
      select(ntips, tree.max.age, trees_mean_dr, 
             shape.yule, colles.yule, sackin.yule, 
             shape.pda, colles.pda, sackin.pda,
             principal_eigenvalue, asymmetry, peakedness,
             taxon, rank, type)
  ) %>% mutate(new_type = paste0(rank,"_", type)) %>% 
    nest(data = -taxon)
  
  p_wik_tb[[i]] <- d_tb_tp %>%
    mutate(test = map(data, ~ pairwise.wilcox.test(eval(parse(text = paste0(".x$",mets[i]))), .x$new_type, 
                                                   method = "bonferroni")),  
           tidied = map(test, broom::tidy)) %>% unnest(tidied) %>% 
    select(- data, -test) %>% 
    filter(str_detect(group1, 'random_age'), str_detect(group2, 'empirical')) %>% 
    mutate(mtr = paste0(mets[i]))
}
do.call(rbind, lapply(p_wik_tb, data.frame, stringsAsFactors = F)) %>% 
  write.csv(file = "emp_rand_pval.csv")
 
eval(parse(text = paste0(".x$",mets[4])))

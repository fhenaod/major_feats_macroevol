library(diversitree)
library(tidyverse)
library(rncl)
source('code/ageGenerator.R', chdir = TRUE)
source('code/funct_mets.R', chdir = TRUE)

# node sample N subtrees given a min. ntips and tolerance, from a mega phylo  ####
age_subtrees=function(phylo,ages,tolerance,min_tip, N){
  sampled.trees<-list()
  
    new_age_generator<-ageGenerator(tree = phylo, ages = rep(ages, N),  
                                    tolerance = tolerance, fixed = T)
    if(is.na(new_age_generator$newAges)[1]==T) stop("Nodes not found under current conditions")
    new_age_generator$newGenera<-new_age_generator$newGenera[!sapply(new_age_generator$newGenera, anyNA)] 
    
    for (i in 1:length(new_age_generator$newGenera)){
      clade.tree<-castor::get_subtree_with_tips(phylo,new_age_generator$newGenera[[i]])$subtree
      if (Ntip(clade.tree)<min_tip) {next(i+1)} else 
      {sampled.trees[[i]]<-clade.tree}
    } 
  return(sampled.trees)
}

clad <- "seed"
tree <- read_newick_phylo("data_megaPhylos/tree_S2018.cr.pr.bi_.txt")

ages2cut <- c(10, 20, 30, 40, 50)
ns2sampl <- c(300, 100, 1000, 1000, 1250)
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

# rank-age bias metrics ####

mega_tree <- read.tree("data_megaPhylos/tree_TE2016.cr.pr_.txt")
clade <- "fern"
rank <- "fams"
clade_rank <- readRDS(paste0("rank_sampling/",clade, "_", rank, "/sliced_trees/", clade, "_", rank, "_trees.rds"))

clade_rank %>% tree_metrics() %>% 
  ggplot(aes(x = tree.max.age, y = (ntips))) + geom_point() + 
  theme_classic() + geom_smooth(method = "lm")

tb_ms <- clade_rank %>% tree_metrics()
p_hist <- hist(tb_ms$tree.max.age, 
               breaks = seq(0, round(max(tb_ms$tree.max.age), -1)+10, 20), 
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
  hist(breaks = seq(0, round(max(.), -1)+10, 20), 
       main = paste0("samp: ",clade,"_", rank))
par(mfrow = c(1, 1))

saveRDS(ss_tt, paste0("rand_age_rank/",clade, "_", rank,"_rdm_trs.rds"))



# gets the edge length leading to a named node #####
edge_nam_nods=function(tree){
  int_nd <- ((Ntip(tree)+1):(Ntip(tree)+tree$Nnode))[which(!is.na(tree$node.label))]
  data.frame(taxon = tree$node.label[which(!is.na(tree$node.label))], 
             ed.len = tree$edge.length[match(int_nd, tree$edge[,2])])
}

nod_tre_ls <- list(J2012_nd, J2018_nd, R2018_nd, 
                   #S2018_nd, 
                   ST2018_nd, T2016_nd, TE2016_nd, U2019_nd
                   #, V2019_nd
                   )

# are branch lengths from named clades different from a random draw?
grp_ls <- vector('list', length(nod_tre_ls))
for(h in 1:length(nod_tre_ls)){
  message(h)
  
  mn_bl <- c()
  for(z in 1:1000){
    mn_bl[z] <- mean(sample(x = nod_tre_ls[[h]]$edge.length, 
                            size = dim(edge_nam_nods(nod_tre_ls[[h]]))[1]))
  }
  
  grp_ls[[h]] <- local({
    h <- h
    p1 <- ggplot(data.frame(mn_bl), aes(x = mn_bl)) + 
      geom_histogram(color = "black", fill = "white") +  
      geom_vline(aes(xintercept = mean(edge_nam_nods(nod_tre_ls[[h]])$ed.len, na.rm = T)),
                 color = "red", linetype = "dashed") +
      labs(subtitle = paste0(nod_tre_ls[[h]]$node.label[1]), 
           x = "Mean branch length", y = "Count") + theme_classic()
    print(p1)
  })
  #print(p1)
}

library(ggpubr)
ggarrange(grp_ls[[1]], grp_ls[[2]], grp_ls[[3]],
          grp_ls[[4]], grp_ls[[5]], grp_ls[[6]],
          grp_ls[[7]], #grp_ls[[8]], grp_ls[[9]],
          ncol = 3, nrow = 3)

# are branch lengths from the leading edge to named clades truncated or a subsample?
grp_ls2 <- vector('list', length(nod_tre_ls))
for(h in 1:length(nod_tre_ls)){
  message(h)
  
  grp_ls2[[h]] <- local({
    h <- h
    eg_ln_tab <- edge_nam_nods(nod_tre_ls[[h]])
    mn_tbl <- data.frame(br_lns = c(nod_tre_ls[[h]]$edge.length,
                                    eg_ln_tab$ed.len),
                         br_type = c(rep("full_tr", length(nod_tre_ls[[h]]$edge.length)),
                                     rep("name_nd", length(eg_ln_tab$ed.len))))
    p1 <- ggplot(mn_tbl, aes(x = br_lns, color = br_type)) + 
      geom_histogram(position = "identity", fill = "white") +
      scale_color_brewer(palette = "Dark2") +
      labs(subtitle = paste0(nod_tre_ls[[h]]$node.label[1]), 
           x = "Branch length", y = "Count") + theme_classic() + 
      theme(legend.position = "none")
    print(p1)
  })
  #print(p1)
}

ggarrange(grp_ls2[[1]], grp_ls2[[2]], grp_ls2[[3]],
          grp_ls2[[4]], grp_ls2[[5]], grp_ls2[[6]],
          grp_ls2[[7]], #grp_ls2[[8]], grp_ls2[[9]],
          ncol = 3, nrow = 3)


# violin/boxplot
grp_ls3 <- vector('list', length(nod_tre_ls))
for(h in 1:length(nod_tre_ls)){
  message(h)
  
  grp_ls3[[h]] <- local({
    h <- h
    eg_ln_tab <- edge_nam_nods(nod_tre_ls[[h]])
    mn_tbl <- data.frame(br_lns = c(nod_tre_ls[[h]]$edge.length,
                                    eg_ln_tab$ed.len),
                         br_type = c(rep("full_tr", length(nod_tre_ls[[h]]$edge.length)),
                                     rep("name_nd", length(eg_ln_tab$ed.len))))
    p1 <- ggplot(mn_tbl, aes(x = br_type, y = br_lns, color = br_type)) + 
      geom_boxplot(fill = "white") +
      #geom_violin(fill = "white") +
      scale_color_brewer(palette = "Dark2") +
      labs(subtitle = paste0(nod_tre_ls[[h]]$node.label[1]), 
           x = "", y = "Branch length") + theme_classic() + 
      theme(legend.position = "none")  
    #+geom_boxplot(width = .1, col = "black")
    #+stat_summary(fun.data = mean_sdl, mult = 1, geom = "crossbar", color = "black", width = .05)
    #+stat_summary(fun.data = mean_sdl, mult = 1, geom = "pointrange", color = "red")
    print(p1)
    })
  #print(p1)
}

ggarrange(grp_ls3[[1]], grp_ls3[[2]], grp_ls3[[3]],
          grp_ls3[[4]], grp_ls3[[5]], grp_ls3[[6]],
          grp_ls3[[7]], #grp_ls3[[8]], grp_ls3[[9]],
          ncol = 3, nrow = 3)

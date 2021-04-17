library(diversitree)
library(tidyverse)
#library(rncl)
library(ggpubr)
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
      clade.subtree<-castor::get_subtree_with_tips(phylo,new_age_generator$newGenera[[i]])
      clade.subtree$subtree$old.stem.len <- 
        phylo$edge.length[match(setdiff(clade.subtree$new2old_clade, clade.subtree$new2old_tip)[1], phylo$edge[,2])]
      if (Ntip(clade.subtree$subtree)<min_tip) {next(i+1)} else 
      {sampled.trees[[i]]<-clade.subtree$subtree}
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

# gets the stem edge length from a named node #####
edge_nam_nods=function(tree){
  int_nd <- ((Ntip(tree)+1):(Ntip(tree)+tree$Nnode))[which(!is.na(tree$node.label))]
  data.frame(taxon = tree$node.label[which(!is.na(tree$node.label))], 
             ed.len = tree$edge.length[match(int_nd, tree$edge[,2])])
}

nod_tre_ls <- list(agar_nd, amph_nd, bird_nd, chon_nd, 
                   fern_nd, fish_nd, mamm_nd,
                   #seed_nd, 
                   squa_nd)
names(nod_tre_ls) <- c("agar", "amph", "bird", "chon", 
                             "fern",  "fish", "mamm",
                  #seed_nd, 
                   "squa")

# are stem branch lengths from named clades different from random trees ones?
files <- dir("rand_age_rank/samp_trees/") # load random sample trees
stem_len_rnd <- c()
for(j in 1:length(files)){
  clad_trees <- readRDS(paste0("rand_age_rank/samp_trees/", files[j]))
  clade_nm <- strsplit(files[j], split="_", fixed=TRUE)[[1]][1]
  clade_rk <- strsplit(files[j], split="_", fixed=TRUE)[[1]][2]
  tm_res <- data.frame(clade_nm, clade_rk, old.stem.len = sapply(clad_trees, "[[", 8))
  stem_len_rnd <- rbind(stem_len_rnd, tm_res)
}
stem_len_rnd$type <- "rand_age_rank"
stem_len_rnd <- stem_len_rnd %>% mutate(clade_rk = recode(clade_rk, fams = "family"), 
                                        clade_rk = recode(clade_rk, ords = "order"))

ggplot(stem_len_rnd, aes(x = clade_nm, y = log(old.stem.len+1), fill = clade_rk)) + 
  geom_violin() + theme_classic() # graph to inspect numbers

names(nod_tre_ls)==names(tax_lst)

# paste taxonomy 2 cols tab with stem branch len tab
tr_tx_ls <- list()
for(a in 1:length(nod_tre_ls)){
  tr_tx_ls[[a]] <- edge_nam_nods(nod_tre_ls[[names(nod_tre_ls)[a]]]) %>% 
    mutate(type = "empirical") %>% 
    left_join(tax_trf(tax_lst[[a]]), by = ("taxon")) %>% 
    mutate(clade_nm = names(nod_tre_ls)[a])
}

select(tr_tx_ls[[5]], clade_nm, clade_rk = rank, ed.len, type) %>% 
  filter(clade_rk != "genus") %>%
  ggplot(aes(x = clade_rk, y = log(ed.len+1), fill = clade_rk)) + geom_violin() + theme_classic()

# histograms
grp_ls4 <- vector('list', length(nod_tre_ls))
for(h in 1:length(grp_ls4)){
  message(h)
  
  grp_ls4[[h]] <- local({
    h <- h
    mn_bl <- rbind(select(tr_tx_ls[[h]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                     filter(clade_rk != "genus"),
    stem_len_rnd %>% filter(clade_nm == names(nod_tre_ls)[h]) %>% 
      select(clade_nm, clade_rk, ed.len = old.stem.len, type))
    
    p1 <- ggplot(data.frame(mn_bl), aes(x = log(ed.len), col = type)) + 
      geom_histogram(fill = "white") +  
      scale_color_brewer(palette = "Dark2") +
      #geom_vline(aes(xintercept = mean(edge_nam_nods(nod_tre_ls[[h]])$ed.len, na.rm = T)), color = "red", linetype = "dashed") +
      labs(subtitle = paste0(names(nod_tre_ls)[h]), 
           x = "Branch length", y = "Count") + theme_classic() + theme(legend.position = "none")
  })
  print(p1)
}

ggarrange(grp_ls4[[1]], grp_ls4[[2]], grp_ls4[[3]],
          grp_ls4[[4]], grp_ls4[[5]], grp_ls4[[6]],
          grp_ls4[[7]], grp_ls4[[8]], #grp_ls4[[9]],
          ncol = 3, nrow = 3)

# boxplots
grp_ls4.1 <- vector('list', length(nod_tre_ls))
for(h in 1:length(grp_ls4)){
  message(h)
  
  grp_ls4.1[[h]] <- local({
    h <- h
    mn_bl <- rbind(select(tr_tx_ls[[h]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                     filter(clade_rk != "genus"),
                   stem_len_rnd %>% filter(clade_nm == names(nod_tre_ls)[h]) %>% 
                     select(clade_nm, clade_rk, ed.len = old.stem.len, type))
    
    mn_bl <- group_by(mn_bl, type) %>% 
      filter(!(abs(ed.len - median(ed.len, na.rm = T)) > 2*sd(ed.len, na.rm = T))) %>%
      ungroup() # remove outliers by group
    
    p1 <- ggplot(data.frame(mn_bl), aes(x = clade_rk, y = log(ed.len), fill = type)) + 
      geom_boxplot() + 
      #stat_summary(fun.data = mean_sdl, mult = 1, geom = "pointrange", width = 0.01, color = "black") + 
      scale_color_brewer(palette = "Dark2") +
      #geom_vline(aes(xintercept = mean(edge_nam_nods(nod_tre_ls[[h]])$ed.len, na.rm = T)), color = "red", linetype = "dashed") +
      labs(subtitle = paste0(names(nod_tre_ls)[h]), 
           x = "Type", y = "Ln Branch length") + theme_classic() + theme(legend.position = "none")
  })
  print(p1)
}

ggarrange(grp_ls4.1[[1]], grp_ls4.1[[2]], grp_ls4.1[[3]],
          grp_ls4.1[[4]], grp_ls4.1[[5]], grp_ls4.1[[6]],
          grp_ls4.1[[7]], grp_ls4.1[[8]], #grp_ls4.1[[9]],
          ncol = 3, nrow = 3)

# normality test
mn_shp_rs <- list()
for(w in 1:length(names(nod_tre_ls))){
  temp_dfr <- rbind(select(tr_tx_ls[[w]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                   filter(clade_rk != "genus"),
                 stem_len_rnd %>% filter(clade_nm == names(nod_tre_ls)[w]) %>% 
                   select(clade_nm, clade_rk, ed.len = old.stem.len, type))
  
  temp_dfr <- group_by(temp_dfr, type) %>% 
    filter(!(abs(ed.len - mean(ed.len, na.rm = T)) > 3*sd(ed.len, na.rm = T))) %>% 
    ungroup() # remove outliers by group
  
    temp_dfr <- temp_dfr %>% nest(data = - type) %>% 
    mutate(test = map(data, ~ shapiro.test((.x$ed.len))), 
           tidied = map(test, broom::tidy)) %>% unnest(tidied) %>% select(type, p.value)
  
  mn_shp_rs[[w]] <- temp_dfr
}
mn_shp_rs

# statistical diff. between empirical and random sampled
m_w_ts <- list()
for(w in 1:length(names(nod_tre_ls))){
  temp_data <- rbind(select(tr_tx_ls[[w]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                      filter(clade_rk != "genus"),
                    stem_len_rnd %>% filter(clade_nm == names(nod_tre_ls)[w]) %>% 
                      select(clade_nm, clade_rk, ed.len = old.stem.len, type)) 
  
  temp_dfr <-  group_by(temp_data, type) %>% 
    filter(!(abs(ed.len - mean(ed.len, na.rm = T)) > 3*sd(ed.len, na.rm = T))) %>% 
    ungroup() # remove outliers by group
  
  temp_dfr <- temp_dfr %>% nest(data = - clade_nm) %>% 
    mutate(test = map(data, ~ kruskal.test(.x$ed.len~.x$type)), 
           tidied = map(test, broom::tidy)) %>% unnest(tidied) %>% select(statistic, p.value)
  
  temp_dfr <- mutate(temp_dfr, n = dim(temp_data)[1])
  
  m_w_ts[[w]] <- temp_dfr
}
data.frame(clade = names(nod_tre_ls), do.call(rbind.data.frame, m_w_ts)) %>% 
  mutate(eta = (statistic-2+1)/(n - 2)) %>% arrange(desc(eta))

# are branch lengths from named clades different from a random edge draw?
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
  })
  print(p1)
}

ggarrange(grp_ls[[1]], grp_ls[[2]], grp_ls[[3]],
          grp_ls[[4]], grp_ls[[5]], grp_ls[[6]],
          grp_ls[[7]], grp_ls[[8]], grp_ls[[9]],
          ncol = 3, nrow = 3)

# are stem branches to named clades truncated or a subsample from all tree edges?
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
  })
  print(p1)
}

ggarrange(grp_ls2[[1]], grp_ls2[[2]], grp_ls2[[3]],
          grp_ls2[[4]], grp_ls2[[5]], grp_ls2[[6]],
          grp_ls2[[7]], grp_ls2[[8]], #grp_ls2[[9]],
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
  })
  print(p1)
}

ggarrange(grp_ls3[[1]], grp_ls3[[2]], grp_ls3[[3]],
          grp_ls3[[4]], grp_ls3[[5]], grp_ls3[[6]],
          grp_ls3[[7]], grp_ls3[[8]], #grp_ls3[[9]],
          ncol = 3, nrow = 3)

# plot stem edge from named node
plot(chon_nd, show.tip.label = F, type = "fan", no.margin = T)
#nodelabels(node = Ntip(chon_nd) + which(!is.na(chon_nd$node.label)), frame = "none", col = "red") # node numbers
nodelabels(chon_nd$node.label, frame = "none",cex = .7,  col = "darkblue") # node names
nodelabels(node = Ntip(chon_nd) + which(!is.na(chon_nd$node.label)), pch = 20, col = "red") # nodes with red points

int_nd <- ((Ntip(chon_nd)+1):(Ntip(chon_nd)+chon_nd$Nnode))[which(!is.na(chon_nd$node.label))]
plotSimmap(phytools::paintBranches(chon_nd, int_nd[2:165], anc.state = "1", state = "2"), 
           colors = setNames(c("black", "red"), c(1,2)), 
           fsize = .0001, lwd = 1, type = "fan")
nodelabels(node = Ntip(chon_nd) + which(!is.na(chon_nd$node.label)), pch = 20, col = "red")

##### empirical rnak named clade metrics compared with aprox. age random clades
mets <- rank_sum_stats %>% select(ntips, tree.max.age, trees_mean_dr, 
                                         shape.yule, colles.yule, sackin.yule,
                                         shape.pda, colles.pda, sackin.pda,
                                         principal_eigenvalue, asymmetry, peakedness) %>% names()
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
  ) %>% #filter(taxon == "mamm") %>% 
    mutate(new_type = paste0(rank,"_", type)) %>% 
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

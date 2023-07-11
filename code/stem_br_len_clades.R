library(diversitree)
library(tidyverse)
library(rncl)
library(ggpubr)
source('code/ageGenerator.R', chdir = TRUE)
source('code/funct_mets.R', chdir = TRUE)

# rank-age bias metrics ####
mega_tree <- read.tree("data_megaPhylos/tree_GBOTBsper.cr.bi.pr._.txt")
clade <- "seed"
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

saveRDS(ss_tt, paste0("rand_age_rank/samp_trees/", clade, "_", rank,"_rdm_trs.rds"))

# gets the edge length leading to a named node #####
edge_nam_nods=function(tree){
  int_nd <- ((Ntip(tree)+1):(Ntip(tree)+tree$Nnode))[which(!is.na(tree$node.label))]
  data.frame(taxon = tree$node.label[which(!is.na(tree$node.label))], 
             ed.len = tree$edge.length[match(int_nd, tree$edge[,2])])
}

nod_tre_ls <- list(agar_nd, amph_nd, bird_nd, chon_nd, 
                   fern_nd, fish_nd, mamm_nd, seed_nd, squa_nd)
names(nod_tre_ls) <- c("agar", "amph", "bird", "chon", 
                       "fern",  "fish", "mamm", "seed", "squa")

nams_ranks <- c("Agaricomycetes", "Amphibia", "Aves", 
                "Chondrichthyes", "Polypodiopsida", "Actinopterygii", 
                "Mammalia", "Spermatophyta" ,"Squamata")

# are stem branch lengths from named clades different from random trees ones? ####
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
    filter(!is.na(rank)) %>%
    mutate(clade_nm = names(nod_tre_ls)[a])
}
names(tr_tx_ls) <- names(nod_tre_ls)

tr_tx_ls[[8]] %>% dplyr::select(clade_nm, clade_rk = rank, ed.len, type) %>% 
  filter(clade_rk != "genus", clade_rk != "class") %>%
  ggplot(aes(x = clade_rk, y = log(ed.len+1), fill = clade_rk)) + 
  geom_violin() + theme_classic() + labs(subtitle = names(tr_tx_ls[8]))

# histograms
grp_ls4 <- vector('list', length(nod_tre_ls))
for(h in 1:length(grp_ls4)){
  message(h)
  
  grp_ls4[[h]] <- local({
    h <- h
    mn_bl <- rbind(select(tr_tx_ls[[h]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                     filter(!clade_rk %in% c("genus", "class")),
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

ggarrange(plotlist = grp_ls4,
          ncol = 3, nrow = 3)

## figure 3 boxplots ####
grp_ls4.1 <- vector('list', length(nod_tre_ls))
for(h in 1:length(grp_ls4)){
  message(h)
  
  grp_ls4.1[[h]] <- local({
    h <- h
    mn_bl <- rbind(dplyr::select(tr_tx_ls[[h]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                     filter(!clade_rk %in% c("genus", "class")),
                   stem_len_rnd %>% filter(clade_nm == names(nod_tre_ls)[h]) %>% 
                     dplyr::select(clade_nm, clade_rk, ed.len = old.stem.len, type))
    
    if(mn_bl$clade_nm[1] == "amph"){
      mn_bl <- filter(mn_bl, clade_rk != "order")
    }
    
    mn_bl <- group_by(mn_bl, type) %>% 
      filter(!(abs(ed.len - median(ed.len, na.rm = T)) > 2*sd(ed.len, na.rm = T))) %>%
      ungroup() # remove outliers by group
    
    p1 <- ggplot(data.frame(mn_bl), aes(x = clade_rk, y = (ed.len), fill = type)) + 
      geom_boxplot(na.rm = T, varwidth = T, outlier.size = .5) + 
      #stat_summary(fun.data = mean_sdl, mult = 1, geom = "pointrange", width = 0.01, color = "black") + 
      scale_fill_manual(values = c("#335C67", "#D5A021", "#9E2A2B", "#28AFB0"), 
                        name = "", labels = c("Empirical", "Random")) +
      #geom_vline(aes(xintercept = mean(edge_nam_nods(nod_tre_ls[[h]])$ed.len, na.rm = T)), color = "red", linetype = "dashed") +
      scale_x_discrete(labels = c("family" = "Family", "order" = "Order")) +
      labs(
        #subtitle = paste0(nams_ranks[h]), 
           x = "", y = "") + theme_classic() + theme(legend.position = "")
  })
  print(p1)
}

# phylopics
#grp_ls4.1[[1]] <- grp_ls4.1[[1]] + annotation_custom(tax_pics[[nams_ranks[1]]], xmin = 2,  xmax = 2.5, ymin = 45, ymax = 55)
#grp_ls4.1[[2]] <- grp_ls4.1[[2]] + annotation_custom(tax_pics[[nams_ranks[2]]], xmin = 1.2,  xmax = 1.5, ymin = 80, ymax = 90) 
#grp_ls4.1[[3]] <- grp_ls4.1[[3]] + annotation_custom(tax_pics[[nams_ranks[3]]], xmin = 2,  xmax = 2.5, ymin = 25, ymax = 30)
#grp_ls4.1[[4]] <- grp_ls4.1[[4]] + annotation_custom(tax_pics[[nams_ranks[4]]], xmin = 2,  xmax = 2.5, ymin = 120, ymax = 140)
#grp_ls4.1[[5]] <- grp_ls4.1[[5]] + annotation_custom(tax_pics[[nams_ranks[5]]], xmin = 2,  xmax = 2.5, ymin = 145, ymax = 170)
#grp_ls4.1[[6]] <- grp_ls4.1[[6]] + annotation_custom(tax_pics[[nams_ranks[6]]], xmin = 2,  xmax = 2.5, ymin = 70, ymax = 80)
#grp_ls4.1[[7]] <- grp_ls4.1[[7]] + annotation_custom(tax_pics[[nams_ranks[7]]], xmin = 2,  xmax = 2.5, ymin = 40, ymax = 47)
#grp_ls4.1[[8]] <- grp_ls4.1[[8]] + annotation_custom(tax_pics[[nams_ranks[8]]], xmin = 2,  xmax = 2.8, ymin = 62, ymax = 82)
#grp_ls4.1[[9]] <- grp_ls4.1[[9]] + annotation_custom(tax_pics[[nams_ranks[9]]], xmin = 1.2,  xmax = 1.5, ymin = 50, ymax = 65)

# icons 
grp_ls4.1[[1]] <- grp_ls4.1[[1]] + annotation_custom(tax_pics[[nams_ranks[1]]], xmin = 2,   xmax = 2.5, ymin = 45, ymax = 55)
grp_ls4.1[[2]] <- grp_ls4.1[[2]] + annotation_custom(tax_pics[[nams_ranks[2]]], xmin = 1.3, xmax = 1.7, ymin = 70, ymax = 92) 
grp_ls4.1[[3]] <- grp_ls4.1[[3]] + annotation_custom(tax_pics[[nams_ranks[3]]], xmin = 2,   xmax = 2.5, ymin = 24, ymax = 30)
grp_ls4.1[[4]] <- grp_ls4.1[[4]] + annotation_custom(tax_pics[[nams_ranks[4]]], xmin = 2,   xmax = 2.5, ymin = 100, ymax = 135)
grp_ls4.1[[5]] <- grp_ls4.1[[5]] + annotation_custom(tax_pics[[nams_ranks[5]]], xmin = 2,   xmax = 2.5, ymin = 125, ymax = 165)
grp_ls4.1[[6]] <- grp_ls4.1[[6]] + annotation_custom(tax_pics[[nams_ranks[6]]], xmin = 2,   xmax = 2.5, ymin = 65, ymax = 82)
grp_ls4.1[[7]] <- grp_ls4.1[[7]] + annotation_custom(tax_pics[[nams_ranks[7]]], xmin = 2,   xmax = 2.5, ymin = 35, ymax = 45)
grp_ls4.1[[8]] <- grp_ls4.1[[8]] + annotation_custom(tax_pics[[nams_ranks[8]]], xmin = 2,   xmax = 2.5, ymin = 62, ymax = 80)
grp_ls4.1[[9]] <- grp_ls4.1[[9]] + annotation_custom(tax_pics[[nams_ranks[9]]], xmin = 1.2, xmax = 1.5, ymin = 48, ymax = 60)

ggarrange(plotlist = grp_ls4.1,
          ncol = 3, nrow = 3) %>% 
  annotate_figure(left = text_grob("Stem branch length (Myr)", 
                                  rot = 90, vjust = 1, size = 12) 
                  #, bottom = text_grob("", gp = gpar(cex = 1))
                  )
ggsave(file = "figures/fig_3.svg",
       width = 20, height = 20, units = "cm", dpi = 300)
ggsave(file = "figures/fig_3.pdf", bg = "white",
       width = 25, height = 25, units = "cm", dpi = 600)

# normality test ####
mn_shp_rs <- list()
for(w in 1:length(names(nod_tre_ls))){
  temp_dfr <- rbind(dplyr::select(tr_tx_ls[[w]], clade_nm, clade_rk = rank, ed.len, type) %>% 
                      filter(!clade_rk %in% c("genus", "class")),
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
                       filter(!clade_rk %in% c("genus", "class")),
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

# are branch lengths from named clades different from a random edges? ####
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

ggarrange(plotlist = grp_ls,
          ncol = 3, nrow = 3)

# are stem branches to named clades truncated or a subsample from all tree edges? ####
# histogram
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

ggarrange(plotlist = grp_ls2,
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

ggarrange(plotlist = grp_ls3, 
          ncol = 3, nrow = 3)

# plot stem edge from named node ####
library(phytools)
plot(chon_nd, show.tip.label = F, type = "fan", no.margin = T)
#nodelabels(node = Ntip(chon_nd) + which(!is.na(chon_nd$node.label)), frame = "none", col = "red") # node numbers
nodelabels(chon_nd$node.label, frame = "none", cex = .7,  col = "darkblue") # node names
nodelabels(node = Ntip(chon_nd) + which(!is.na(chon_nd$node.label)), pch = 20, col = "red") # nodes with red points

int_nd <- ((Ntip(chon_nd)+1):(Ntip(chon_nd)+chon_nd$Nnode))[which(!is.na(chon_nd$node.label))]
plotSimmap(phytools::paintBranches(chon_nd, int_nd[2:165], anc.state = "1", state = "2"), 
           colors = setNames(c("black", "red"), c(1,2)), 
           fsize = .0001, lwd = 1, type = "fan", part = .5)
nodelabels(node = Ntip(chon_nd) + which(!is.na(chon_nd$node.label)), pch = 20, col = "red")

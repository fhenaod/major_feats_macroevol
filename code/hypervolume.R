library(hypervolume)
library(tidyverse)
library(viridis)

# dataframes ####
# consensus trees
sl_sum_stats   # sliced concensus trees
rank_sum_stats # rank sampling consensus trees
fract_sum_node # self-similar sampling by nodes consensus trees
fract_sum_edg %>% group_by(s_age) %>% sample_n(200, replace = F) # self-similar sampling by edges consensus trees
rank_rdm_sum    # approximate-age-rank random trees

sampligs <- c("Time-slicing", "Rank", "Nodes", 
              "Edges", 
              "Random ranks")

res_all_ls <- list(
  sl_sum_stats   , # sliced concensus trees
  rank_sum_stats , # rank sampling consensus trees
  fract_sum_node , # self-similar sampling by nodes consensus trees
  fract_sum_edg %>% group_by(s_age) %>% sample_n(200, replace = F), # self-similar sampling by edges consensus trees
  rank_rdm_sum
)

# hypervolume estimation ####
mgl_subs <- c("principal_eigenvalue", "asymmetry", 
              "peakedness") 

yule_subs <- c("shape.yule", "colles.yule", "sackin.yule", "taxon")
pda_subs  <- c("shape.pda",  "colles.pda",  "sackin.pda",  "taxon") 

metric_nams <- mgl_subs # yule_subs pda_subs
min_tips <- c(rep(20, 3), 10, 10)

dir.create("figures/hypervols/")

plot3d_sim = F

for(i in 1:length(sampligs)){
schem <- sampligs[i]
df2vol <- res_all_ls[[i]]

tax_unq <- df2vol %>% pull(taxon) %>% unique() 

ls_plots <- c()
vols_mtx <- c()
for(j in 1:length(tax_unq)){
  if(sum(metric_nams == mgl_subs)==3){
    vols_mtx[[j]] <- 
      df2vol %>% 
      filter(taxon == tax_unq[j], ntips >= min_tips[i]) %>% #names()
      dplyr::select(any_of(mgl_subs)) %>% 
      mutate(ln_principal_eigenvalue = log(principal_eigenvalue),
             ln_peakedness = log(peakedness), .keep = "unused") %>% 
      filter(!is.na(ln_principal_eigenvalue)) %>% 
      expectation_convex(check.memory = F) %>% try()
  } else {
    vols_mtx[[j]] <- 
      df2vol %>% 
      filter(taxon == tax_unq[j], ntips >= min_tips[i]) %>% #names()
      dplyr::select(any_of(metric_nams)) %>% 
      filter(!is.na(ln_principal_eigenvalue)) %>% 
      expectation_convex(check.memory = F) %>% try()
  }
  
}

vols_mtx %>% sapply(class)
names(vols_mtx) <- tax_unq

hypervolume_join(vols_mtx) %>%
plot(show.3d = T, 
     show.random = F, show.density = T, 
     show.data = T, show.centroid = F, show.legend = F,
     contour.type = "kde", #box = FALSE,
     ylab = "y")
view3d(theta = 180, phi = 90, 
       #fov = 45, 
       zoom = .9)
hypervolume_save_animated_gif(file.name = paste0("figures/hypervols/3d_vol_", schem), 
                              rpm = 2)
rgl.close()

# empirical phylospace volume min/max
phy_emp_sum <- 
df2vol %>% 
  filter(ntips >= min_tips[i]) %>% #names()
  dplyr::select(any_of(mgl_subs)) %>% 
  mutate(ln_principal_eigenvalue = log(principal_eigenvalue),
         ln_peakedness = log(peakedness), .keep = "unused") %>% 
  filter(!is.na(ln_principal_eigenvalue)) %>% 
  gather(key, value) %>% group_by(key) %>%
  summarize(min = min(value), max = max(value), n = n())

# sample from unif dist based on empirical 
sim_vols <- c()
for(f in 1:length(tax_unq)){
  phy_emp_ls <- 
    phy_emp_sum %>% group_by(key) %>% 
    group_map(~runif(.x$n, min = .x$min, max = .x$max))
  
  names(phy_emp_ls) <- pull(phy_emp_sum, key)
  
  sim_phylo <- phy_emp_ls %>% data.frame() %>% 
    select(asymmetry, ln_principal_eigenvalue, ln_peakedness)
  
  sim_phylo_col <- expectation_convex(sim_phylo, check.memory = F)
  sim_vols[f] <- sim_phylo_col@Volume
}

if(plot3d_sim){
sim_phylo_col <- expectation_convex(sim_phylo, check.memory = F)
sim_phylo_col %>% 
  plot(show.3d = T, 
       show.random = F, show.density = T, 
       show.data = T, show.centroid = F, show.legend = F,
       #contour.type = "kde", #box = FALSE,
       ylab = "y")
view3d(theta = 180, phi = 90, 
       #fov = 45, 
       zoom = .9)
} else {}

emp_vols <- c()
for(h in 1:length(tax_unq)){
  emp_vols[h] <- vols_mtx[[h]]@Volume
}

p <- 
data.frame(Empirical = emp_vols, Simulated = sim_vols) %>% 
  gather(key, value) %>% 
  ggplot(aes(x = key, y = log(value), 
             color = key)) +
  geom_boxplot() + geom_point() +
  theme_classic(base_size = 13) + theme(legend.position = "none") +
  scale_color_viridis(discrete = TRUE, 
                      option = "viridis", begin = .25, end = .75, direction = 1) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "", y = "Ln Hypervolume")

ls_plots[[i]] <- 
p + 
  stat_compare_means(label.x = .75, 
                     label.y = max(log(p$data$value))-.5, 
                     label.sep = "\n")
}

library(ggpubr)
ggarrange(plotlist = res_plots[[1]], 
          #ncol = 3, nrow = 2,
          labels = "AUTO")
ggsave(filename = paste0("figures/hypervols/box_vols_panel.png"), 
       width = 20, height = 20, units = "cm")

# plot empirical points plus one sampled dataset
vols_mtx[[length(tax_unq)+1]] <- sim_phylo_col
hypervolume_join(vols_mtx) %>% 
  plot(show.3d = T, 
       show.random = F, show.density = T, 
       show.data = T, show.centroid = F, show.legend = F,
       contour.type = "kde", #box = FALSE,
       ylab = "y")
view3d(theta = 180, phi = 90, 
       #fov = 45, 
       zoom = .9)
hypervolume_save_animated_gif(file.name = paste0("figures/hypervols/3d_vol_2", schem), 
                              rpm = 2)
rgl.close()


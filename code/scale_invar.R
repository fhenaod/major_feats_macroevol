library(tidyverse)
library(ggpubr)
library(grid)

# results dataframes
sl_sum_stats   # sliced concensus trees
rank_sum_stats # rank sampling consensus trees
fract_sum_node # self-similar sampling by nodes consensus trees
fract_sum_edg  # self-similar sampling by edges consensus trees

all_1k_sum     # 1000 posterior trees 
all_10_slic    # sliced 10 posterior trees
all_10_rank    # rank sampled 10 posterior trees
all_10_rando   # self-similar sampled 10 posterior trees

## facet plots ####
###
x <- sl_sum_stats
gp_sl <- x %>% filter(ntips > 20) %>% 
  select(ntips, 
         #trees_mean_dr, #beta, #gamma.stat,
         #shape.yule, colles.yule,  #shape.pda, colles.pda,
         #br.len_mean, br.t_mean,
         principal_eigenvalue, 
         #asymmetry, peakedness,
         modalities,
         taxon) %>%
  gather(key, value, -taxon) %>%
  ggplot(aes(x = (value), y = 1 - ..y..
             #, colour = taxon
             )) + stat_ecdf(na.rm = T, pad = FALSE) + 
  theme_minimal() + theme(legend.position = "none") +
  scale_x_log10() + scale_y_log10() + annotation_logticks() + 
  labs(title = "Time sampling", x = "", 
       y = "") + 
  theme(#legend.position = c(.8, .2),
    #axis.text.x = element_text(size = 7),
    panel.border = element_blank(), 
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(face = "bold")) +
  facet_wrap(~key, scales = "free", ncol = 4,
             labeller = 
               labeller(key =  c("modalities" = "Modalities", 
                                 "ntips" = "Number of tips", 
                                 "principal_eigenvalue" = "Principal eigenvalue"))) +
  theme(strip.text.x = element_text(size = 11), 
        strip.text.y = element_text(size = 11))

###
x <- rank_sum_stats
gp_rk <- x %>% filter(ntips > 20) %>% 
  select(ntips, 
         #trees_mean_dr, beta, gamma.stat,
         #shape.yule, colles.yule, #shape.pda, colles.pda,
         #br.len_mean, br.t_mean,
         principal_eigenvalue, 
         #asymmetry, #peakedness, 
         modalities, 
         taxon, rank) %>%
  gather(key, value, -taxon, -rank) %>%
  ggplot(aes(x = (value), y = 1 - ..y..
             #, colour = taxon
             )) + stat_ecdf(na.rm = T, pad = FALSE) + 
  theme_minimal() + #theme(legend.position = "none") +
  scale_x_log10() + scale_y_log10() + annotation_logticks() + 
  labs(title = "Taxonomic sampling", x = "", 
       y = "") + 
  theme(#legend.position = c(.8, .2),
        #axis.text.x = element_text(size = 7),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold")) +
  facet_wrap(~key, scales = "free", ncol = 4,
             labeller = 
               labeller(key =  c("modalities" = "", 
                                 "ntips" = "", 
                                 "principal_eigenvalue" = "")))

###
x <- fract_sum_node
gp_fn <- x %>% filter(ntips > 20) %>% 
  select(ntips, 
         #trees_mean_dr, #beta, #gamma.stat,
         #shape.yule, colles.yule, #shape.pda, #colles.pda,
         #br.len_mean, br.t_mean,
         principal_eigenvalue, 
         #asymmetry, 
         modalities, taxon, s_age) %>%
  gather(key, value, -taxon, -s_age) %>%
  ggplot(aes(x = (value), y = 1 - ..y..
             #, colour = s_age
             )) + stat_ecdf(na.rm = T, pad = FALSE) +
  theme_minimal() + #theme(legend.position = "none") +
  scale_x_log10() + scale_y_log10() + annotation_logticks() + 
  labs(title = "Random node sampling", x = "", 
       y = "") + 
  theme(#legend.position = c(.8, .2),
    #axis.text.x = element_text(size = 7),
    panel.border = element_blank(), 
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(face = "bold")) +
  facet_wrap(~key, scales = "free", ncol = 4,
             labeller = 
               labeller(key =  c("modalities" = "", 
                                 "ntips" = "", 
                                 "principal_eigenvalue" = "")))

###
x <- fract_sum_edg
gp_fe <- x %>% filter(ntips > 20) %>% 
  select(ntips, 
         #trees_mean_dr, #gamma.stat,
         #br.len_mean, br.t_mean,
         principal_eigenvalue, 
         #asymmetry, 
         modalities, taxon, s_age) %>%
  gather(key, value, -taxon, -s_age) %>%
  ggplot(aes(x = (value), y = 1 - ..y..
             #, colour = taxon
  )) + stat_ecdf(na.rm = T, pad = FALSE) +
  theme_minimal() + theme(legend.position = "none") +
  scale_x_log10() + scale_y_log10() + annotation_logticks() + 
  labs(title = "Random stem sampling", x = "x", 
       y = "") + 
  theme(#legend.position = c(.8, .2),
    #axis.text.x = element_text(size = 7),
    panel.border = element_blank(), 
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(face = "bold")) +
  facet_wrap(~key, scales = "free", ncol = 4,
             labeller = 
               labeller(key =  c("modalities" = "", 
                                 "ntips" = "", 
                                 "principal_eigenvalue" = "")))

ggarrange(gp_sl, gp_rk, gp_fn, gp_fe,
          nrow = 4) %>% 
  annotate_figure(left = textGrob("CCDF - F(x)", 
                                  rot = 90, vjust = 1, gp = gpar(cex = 1)) 
                  #, bottom = textGrob("", gp = gpar(cex = 1))
  )

# with regression line
#
x <- rank_sum_stats
ccdf_p <- x %>% filter(ntips > 20) %>% 
  select(ntips, 
         #trees_mean_dr, 
         #beta, gamma.stat,
         #shape.yule, colles.yule, 
         #shape.pda, colles.pda,
         #br.len_mean, br.t_mean,
         principal_eigenvalue, 
         asymmetry, 
         peakedness, 
         modalities, 
         taxon, rank) %>%
  gather(key, value, -taxon, -rank) %>%
  ggplot(aes(x = (value), y = 1 - ..y..
             #, colour = rank
  )) + stat_ecdf(na.rm = T, pad = FALSE) + 
  theme_classic() + theme(legend.position = "none") +
  scale_x_log10() + scale_y_log10() + annotation_logticks() +
  facet_wrap(~key, scales = "free")

ggplot_build(ccdf_p)$data[[1]] %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_smooth(method = "lm", col = "red") +
  #geom_smooth(data = ggplot_build(ccdf_p)$data[[1]][ggplot_build(ccdf_p)$data[[1]]$x>2.5,], 
  #            method = "lm", col = "red") +
  theme_classic() +
  labs(x = "x", y = "Complementary Empirical Cumulative Distribution Function") +
  facet_wrap(~PANEL, scales = "free")

# individual plots ####
# empirical CDF
x <- rank_sum_stats %>% filter(ntips > 20) 
p <- ecdf(x$ntips)
data.frame(x = sort(x$ntips),
           y = 1-p(sort(x$ntips))) %>% 
  ggplot(aes(x = (x), y = (y))) + geom_line() +
  geom_point(col = "red") + theme_classic() +
  scale_x_log10() + scale_y_log10() + annotation_logticks() + 
  geom_smooth(method = lm, se = F) + 
  labs(y = "1 - f(x)", x = "x")

### model fits ####
library(poweRlaw)
x <- sl_sum_stats %>% filter(ntips > 20) %>% keep(is.numeric) %>% 
  select(ntips, tree.max.age, trees_mean_dr, 
         principal_eigenvalue, peakedness, modalities, 
         br.len_mean, br.t_mean)

get_cdf_models=function(x, n_thr, n_sims){
    res_list <- list()
    for(i in 1:dim(x)[2]){
      if(names(x)[i] == "ntips" | names(x)[i] == "modalities"){
        y <- x[,i][!is.na(x[,i])]
        m1 <- displ$new(y)
        m1$setPars(estimate_pars(m1))
        m1$setXmin(estimate_xmin(m1))
        
        m2 <- dispois$new(y)
        m2$setPars(estimate_pars(m2))
        m2$setXmin(estimate_xmin(m2))
        
        m3 <- dislnorm$new(y)
        m3$setPars(estimate_pars(m3))
        m3$setXmin(estimate_xmin(m3))
        
        m4 <- disexp$new(y)
        m4$setPars(estimate_pars(m4))
        m4$setXmin(estimate_xmin(m4))
        
        bs_ls <- list(
          bootstrap_p(m1, threads = n_thr, no_of_sims = n_sims),
          bootstrap_p(m2, threads = n_thr, no_of_sims = n_sims),
          bootstrap_p(m3, threads = n_thr, no_of_sims = n_sims),
          bootstrap_p(m4, threads = n_thr, no_of_sims = n_sims)
        ) 
        
        res_list[[i]] <- data.frame(
          mod = c("dis_pl", "dis_pois", "dis_lnorm", "dis_exp"),
          xmin = c(m1$xmin, m2$xmin, m3$xmin, m4$xmin),
          p.val = map(bs_ls, "p") %>% unlist(),
          gof = map(bs_ls, "gof") %>% unlist() %>% round(3),
          ds.ty = map(bs_ls, "distance") %>% unlist()
        )    
        
      } else {
        y <- x[,i][!is.na(x[,i])]
        m1 <- conpl$new(y)
        m1$setPars(estimate_pars(m1))
        m1$setXmin(estimate_xmin(m1))
        
        m2 <- conlnorm$new(y)
        m2$setPars(estimate_pars(m2))
        m2$setXmin(estimate_xmin(m2))
        
        m3 <- conexp$new(y)
        m3$setPars(estimate_pars(m3))
        m3$setXmin(estimate_xmin(m3))
        
        m4 <- conweibull$new(y)
        m4$setPars(estimate_pars(m4))
        m4$setXmin(estimate_xmin(m4))
        
        bs_ls <- list(
          bootstrap_p(m1, threads = n_thr, no_of_sims = n_sims),
          bootstrap_p(m2, threads = n_thr, no_of_sims = n_sims),
          bootstrap_p(m3, threads = n_thr, no_of_sims = n_sims),
          bootstrap_p(m4, threads = n_thr, no_of_sims = n_sims)
        ) 
        
        res_list[[i]] <- data.frame(
          mod = c("con_pl", "con_lnorm", "con_exp", "con_weib"),
          xmin = c(m1$xmin, m2$xmin, m3$xmin, m4$xmin),
          p.val = map(bs_ls, "p") %>% unlist(),
          gof = map(bs_ls, "gof") %>% unlist() %>% round(3),
          ds.ty = map(bs_ls, "distance") %>% unlist()
        )   
      }
      print(paste0(names(x)[i], " done!!"))
    }
    names(res_list)<-names(x)
    return(res_list)
}

cd_mods_res <- get_cdf_models(x, 2, 2)

#load rds
cd_mods_res <- readRDS("powl/rank_powl_mod_res.rds") 
cd_mods_res <- readRDS("powl/slice_polw_mod_res.rds")
cd_mods_res <- readRDS("powl/fractal_edge_polw_mod_res.rds") 
cd_mods_res <- readRDS("powl/fractal_node_polw_mod_res.rds") 

cd_mods_res$ntips %>% filter(mod == "dis_pl")
cd_mods_res$modalities %>% filter(mod == "dis_pl")

#
i = 1
m1 <- displ$new(x[,i])
m1$setPars(estimate_pars(m1))
m1$setXmin(estimate_xmin(m1))

m2 <- dispois$new(x[,i])
m2$setPars(estimate_pars(m2))
m2$setXmin(estimate_xmin(m2))

m3 <- dislnorm$new(x[,i])
m3$setPars(estimate_pars(m3))
m3$setXmin(estimate_xmin(m3))

m4 <- disexp$new(x[,i])
m4$setPars(estimate_pars(m4))
m4$setXmin(estimate_xmin(m4))

data.frame(mod = c("dis_pl", "dis_pois", "dis_lnorm", "dis_exp"), 
           xmin = c(m1$xmin, m2$xmin, m3$xmin, m4$xmin))

plot(m1, ylab = "CCDF", pch = 18)
#abline(v = estimate_xmin(m1)$xmin, col = "green", lwd = 2)
lines(m1, col = "red", lwd = 3)
lines(m2, col = "blue", lty = 2, lwd = 2)
lines(m3, col = "purple", lty = 1, lwd = 2)
lines(m4, col = "darkorange", lty = 3, lwd = 3)

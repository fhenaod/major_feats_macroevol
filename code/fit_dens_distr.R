library(fitdistrplus)
library(actuar)
library(phytools)
library(tidyverse)
library(ggpubr)

# tree load #####
e.trees <- henao_d_trs
e_trees <- e.trees [sapply(e.trees, Ntip) > 20]
#e_trees <- sim_b.1
#e_trees <- sim_b.9
e_trees <- sim_b.1d.05
e_trees <- sim_b.7d.5

# branch length histograms
par(mfrow = c(3,3))
for(i in 1:length(e_trees)){
  hist(e_trees[[i]]$edge.length, main = names(e_trees[i]), 
       xlab = "", ylab = "")
}

# branch length distribution fit #####

# fits Exponential, Log-Normal, Normal, Weibull distributions to tree branch lengths
# can remove terminal tips and/or tips below a threshold
get_dist_mods=function(trees, min_tresh, rem_t_tips){
  dens_res <- list()
  d_plots <- list()
  for(i in 1:length(trees)){
    if (rem_t_tips == T){
      e_trees_m <- trees[[i]]$edge.length[-which(trees[[i]]$edge[,2]<=Ntip(trees[[i]]))]
      e_trees_m <- e_trees_m[e_trees_m > min_tresh]
    } else {
      e_trees_m <- trees[[i]]$edge.length[trees[[i]]$edge.length > min_tresh]
    }
    fit_e <-  fitdist(e_trees_m, "exp", method = "mle")
    fit_ln <- fitdist(e_trees_m, "lnorm", method = "mle")
    fit_n <-  fitdist(e_trees_m, "norm", method = "mle")
    fit_w <-  fitdist(e_trees_m, "weibull", method = "mle")
    
    sum_dist <- gofstat(list(fit_e, fit_ln, fit_n, fit_w), 
                        fitnames = c("Exponential", "Log-Normal", "Normal", "Weibull"))
    
    dens_res[[i]] <- qpcR::akaike.weights(sum_dist$aic) %>% 
      data.frame() %>% dplyr::arrange(desc(weights))
  }
  dens_res
}

# removing terminal branch lengths
dens_res <- get_dist_mods(trees = e_trees, min_tresh = 0, rem_t_tips = F)

# fits distributions just to terminal branches
get_dist_mods_tips=function(trees, min_tresh){
  dens_res <- list()
  d_plots <- list()
  for(i in 1:length(trees)){
    e_trees_m <- trees[[i]]$edge.length[which(trees[[i]]$edge[,2]<=Ntip(trees[[i]]))]
    e_trees_m <- trees[[i]]$edge.length[trees[[i]]$edge.length > min_tresh]
    
    fit_e <-  fitdist(e_trees_m, "exp", method = "mle")
    fit_ln <- fitdist(e_trees_m, "lnorm", method = "mle")
    fit_n <-  fitdist(e_trees_m, "norm", method = "mle")
    fit_w <-  fitdist(e_trees_m, "weibull", method = "mle")
    
    sum_dist <- gofstat(list(fit_e, fit_ln, fit_n, fit_w), 
                        fitnames = c("Exponential", "Log-Normal", "Normal", "Weibull"))
    
    dens_res[[i]] <- qpcR::akaike.weights(sum_dist$aic) %>% 
      data.frame() %>% dplyr::arrange(desc(weights))
  }
  dens_res
}
dens_res <- get_dist_mods_tips(trees = e_trees, min_tresh = 0)

tall_mod_tb <- as.data.frame(do.call(rbind, lapply(dens_res, rownames))) 
colnames(tall_mod_tb) <- c("p1", "p2", "p3", "p4")

# summary count table per place
gather(tall_mod_tb, value = model) %>% 
  group_by(model) %>% count(key) %>% arrange(key, desc(n))

# summary stagered barplot   
gather(tall_mod_tb, value = model) %>% 
  group_by(model) %>% 
  ggplot(aes(x = key, color = model, fill = model)) + 
  geom_bar(stat = "count") + theme_classic() + 
  labs(title = "trees > 20 tips", x = "Distribution place", y = "Count")

# Figures as in Venditti et al. 2009 ####
cust_pal <- c("#003049","#d62828","#f77f00","#fcbf49","#eae2b7")
# 1a
pg1 <- gather(tall_mod_tb, value = model) %>% 
  group_by(model, key) %>% count() %>% filter(key == "p1") %>% 
  ggplot(aes(x = model, y = n, color = model, fill = model)) + 
  geom_bar(stat = "identity") + theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Best selected model", x = "", y = "Count") +
  scale_x_discrete(limits = c("Exponential","Weibull","Log-Normal", "Normal"))

pg1 <- pg1 + scale_color_manual(values = cust_pal) + scale_fill_manual(values = cust_pal)

# 1b
pg2 <- gather(tall_mod_tb, value = model) %>% 
  group_by(model) %>% 
  ggplot(aes(x = model, color = key, fill = key)) + 
  geom_histogram(stat = "count", position = position_dodge()) + theme_classic() + 
  labs(title = "Model’s finishing places", x = "", y = "Count") +
  scale_x_discrete(limits = c("Exponential","Weibull","Log-Normal", "Normal"))
  
pg2 <- pg2 + scale_color_manual(values = cust_pal) +
  scale_fill_manual(values = cust_pal, 
                    name = "Place", labels = c("p1" = "1st", "p2" = "2nd", "p3" = "3rd", "p4" = "4th"))

ggarrange(pg1, pg2,
          ncol = 1, nrow = 2,
          labels = c("A", "B"))

### sensitivity analysis ####
#branch length min. and/or removing terminal branch lengths
th_n <- seq(0.01, .5, .02)
ls_mods <- list()
for(j in 1:length(th_n)){
  ls_mods[[j]] <- get_dist_mods(trees = e_trees, min_tresh = th_n[j], rem_t_tips = T)
}

ls_mods %>% length()

tabs_rest <- c()
for(w in 1:length(ls_mods)){
  temp_tab <- as.data.frame(do.call(rbind, lapply(ls_mods[[w]], rownames)))
  colnames(temp_tab) <- c("p1", "p2", "p3", "p4")
  temp_tab2 <- gather(temp_tab, value = model) %>% 
    group_by(model) %>% count(key) %>% 
    mutate(mod_k = paste(model, key, sep = "_")) %>% 
    ungroup() %>% select(mod_k, n)
  
  if(w == 1){
    tabs_rest <- temp_tab2
  } else {
    tabs_rest <- full_join(tabs_rest, temp_tab2, by = "mod_k")
  }
}
tabs_rest2 <-tabs_rest %>% separate(mod_k, c("model", "key"), sep = "_")
colnames(tabs_rest2) <- c("model", "place", th_n)

# line-plot first place vs threshold 
tabs_rest2 %>% pivot_longer(cols = !model:place, 
                            names_to = "threshold",
                            values_to = "count") %>%
  mutate(threshold = as.numeric(threshold)) %>% 
  filter(place == "p1") %>% 
  ggplot(aes(x = threshold, y = count, col = model)) + 
  geom_line() + theme_classic()

# Density plots ####
par(mfrow = c(1,1))
denscomp(list(fit_e, fit_ln, fit_n, fit_w),
         legendtext = c("Exponential", "Log-Normal", "Normal", "Weibull"), 
         xlab = "br len", ylab = "Count",
         main = "", plotstyle = "graphics") + ggplot2::theme_classic()


d1 <- ggplot(sum_stats, aes(x = shape.yule)) + geom_histogram(aes(y = ..density..), colour = "darkblue", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666") + labs(title="", x = "Shape (Yule)", y = "Density") + 
  theme_tufte(base_family = "Helvetica") + geom_rangeframe(data=data.frame(x = c(0, 30),
                                                                           y = c(0, .25)), aes(x, y)) 
d2 <- ggplot(sum_stats, aes(x = shape.pda)) + geom_histogram(aes(y = ..density..), colour = "darkblue", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666") + labs(title="", x = "Shape (PDA)", y = "Density") + 
  theme_tufte(base_family = "Helvetica") + geom_rangeframe(data=data.frame(x = c(-30, 10),
                                                                           y = c(0, .3)), aes(x, y)) 

ggarrange(d1, d2,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Distribution fit tree stats
plotdist(subset(sum_stats, !is.na(shape.yule))$shape.yule, histo = TRUE, demp = TRUE)
plotdist(subset(sum_stats, !is.na(shape.pda))$shape.pda, histo = TRUE, demp = TRUE)
plotdist(subset(sum_stats, !is.na(gamma.stat))$gamma.stat, histo = TRUE, demp = TRUE)

descdist(subset(sum_stats, !is.na(shape.yule))$shape.yule, discrete = F, boot = 1000)
descdist(subset(sum_stats, !is.na(shape.pda))$shape.pda, discrete = F, boot = 1000)
descdist(subset(sum_stats, !is.na(gamma.stat))$gamma.stat, discrete = F, boot = 1000)

#z-score standarization [0,1]
x <- subset(sum_stats, !is.na(shape.pda))$shape.pda
x_scaled <- ((x-mean(x))/(sd(x)))
x_scaled[x_scaled>2]<-(2)
x_scaled[x_scaled<(-2)]<-(-2)
x_scaled <- (x_scaled/5)
x_scaled <- (x_scaled+.5)

#
fit_b <- fitdist(x_scaled, "beta")
fit_e <- fitdist(x_scaled, "exp")
fit_g <- fitdist(x_scaled, "gamma")
fit_ll <- fitdist(x_scaled, "llogis", 
                  start = list(shape = 1, scale = 500))
fit_ln <- fitdist(x_scaled, "lnorm")
fit_po <- fitdist(x_scaled, "pois")
fit_w <- fitdist(x_scaled, "weibull")
#
fit_P <- fitdist(x_scaled, "pareto", start = list(shape = 1, scale = 500))
fit_B <- fitdist(x_scaled, "burr", lower = c(0,0,0), 
                 start = list(shape1 = 1, shape2 = 1, rate = 1))

sum_dist <- gofstat(list(fit_b, fit_e, fit_w, fit_g, fit_ln, fit_ll, fit_P), 
               fitnames = c("Beta", "Weibull", "Gamma", "lnorm", "llogis", "Pareto"))


par(mfrow = c(1,1))
cdfcomp(list(fit_b, fit_w, fit_g), xlogscale = TRUE, ylogscale = TRUE,
        legendtext = c("Beta", "Weibull", "Gamma"), lwd = 2)
plot.legend <- c("Beta", "Weibull", "Gamma")
dcomp <- denscomp(list(fit_b, fit_w, fit_g), legendtext = plot.legend, xlab = "Shape (Yule)", ylab = "Count", main = "", plotstyle = "ggplot")
dcomp + ggplot2::theme_minimal()

par(mfrow = c(2,2))
plot.legend<-c("Beta", "Gamma", "Burr")
denscomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend, xlab = "Shape (Yule)", ylab = "Count", main = "")
cdfcomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend)
qqcomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend)
ppcomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend)


e.trees <- henao_d_trs
e_trees <- e.trees [sapply(e.trees, Ntip) > 20]

sim_bd.9 <- list()
for(i in 1:length(e_trees)){
  sim_bd.9[[i]] <- TreeSim::sim.bd.taxa(n = sapply(e_trees, Ntip)[i],
                                        numbsim = 1, lambda = 0.1, mu = 0)[[1]]
}
sim_b.1
sim_b.9

sim_b.1d.05
sim_b.9d.1

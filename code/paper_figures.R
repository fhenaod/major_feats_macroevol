library(ggplot2); library(RColorBrewer)
library(tidyverse); library(ggpubr)
library(cowplot); library(ggthemes)
library(plotly)

## Paper figures

cols <- c("#003f5c", "#2f4b7c", "#665191", 
          "#a05195", "#d45087", "#f95d6a", 
          "#ff7c43", "#ffa600", "#e3e418ff")
pal <- addalpha(cols, .9)

df2plot <- sl_sum_stats %>% filter(ntips > 20) #%>% filter(taxon != 'Spermatophyta')

# figure 2 3D plots, slicing shape and MGL####
# Fig2 A1
rotate <- 25 # slicing
png(file = "figures/fig_2/f2a1.png",
    width = 20, height = 20, units = "cm", res = 500)
scatter3D(x = (df2plot$shape.yule), 
          y = (df2plot$colles.yule), 
          z = (df2plot$sackin.yule),  
          colvar = df2plot$tree.max.age,
          colkey = list(plot = T, 
                        cex.clab = .9, adj.clab = 0.5,
                        side.clab = 2, side = 3),
          col = pal,
          clab = "Clade age\n (Myr)",
          xlab = "Shape (Yule)", 
          ylab = "Colless (Yule)",
          zlab = "Sackin (Yule)",
          theta = rotate,
          phi = 30,
          pch = as.numeric(factor(df2plot$taxon))+10,
          cex = log(df2plot$ntips)/2,
          bty = "b2",
          add = FALSE) 
dev.off()

f2a2 <- 
  df2plot %>% 
  ggplot(aes(x = shape.yule, y = sackin.yule)) +
  geom_point(aes(size = ntips, color = tree.max.age)) + 
  labs(title = "", x = "Shape (Yule)", y = "Sackin (Yule)",
       color = "Clade age\n   (Myr)") +
  scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none", 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(2, 'cm'))
ggsave(file = "figures/fig_2/f2a2.svg",
       width = 20, height = 20, units = "cm", dpi = 300)

f2a3 <- 
  df2plot %>%
  ggplot(aes(x = colles.yule, y = sackin.yule)) +
  geom_point(aes(size = ntips, color = tree.max.age)) + 
  labs(title = "", x = "Colless (Yule)", y = "Sackin (Yule)") +
  scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + theme(legend.position = "none")
ggsave(file = "figures/fig_2/f2a3.svg",
       width = 20, height = 20, units = "cm", dpi = 300)

f2a4 <- 
  df2plot %>% 
  ggplot(aes(x = shape.yule, y = colles.yule)) +
  geom_point(aes(size = ntips, color = tree.max.age)) + 
  labs(title = "", x = "Shape (Yule)", y = "Colless (Yule)") +
  scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + theme(legend.position = "none")
ggsave(file = "figures/fig_2/f2a4.svg",
       width = 20, height = 20, units = "cm", dpi = 300)

ggarrange(f2a2, f2a3, f2a4,
          ncol = 1)
ggsave(file = "figures/fig_2/f2a234.svg",
       width = 20, height = 20, units = "cm", dpi = 300)
ggsave(file = "figures/fig_2/f2a234.pdf",
       width = 20, height = 20, units = "cm", dpi = 300)

# Fig 2B1
rotate <- 300 # slicing
png(file = "figures/fig_2/f2b1.png",
    width = 20, height = 20, units = "cm", res = 500)
scatter3D(x = log(df2plot$principal_eigenvalue), 
          y = df2plot$asymmetry, 
          z = log(df2plot$peakedness),  
          axes = T, 
          colvar = df2plot$tree.max.age,
          col = pal,
          colkey = list(plot = T, 
                        cex.clab = .9, adj.clab = 0.5,
                        side.clab = 2, side = 3),
          clab = "Clade age\n (Myr)",
          xlab = "Expansion", 
          ylab = "Tippiness",
          zlab = "Branch length heterogeneity",
          theta = rotate, phi = 30,
          pch = as.numeric(factor(df2plot$taxon))+11,
          cex = log(df2plot$ntips)/2,
          bty = "b2",
          #type = "h",
          #ticktype = "detailed", #tick numbers
          add = FALSE)
#f2b1 <- recordPlot()
dev.off()

f2b2 <- 
  df2plot %>% 
  ggplot(aes(x = log(principal_eigenvalue), y = asymmetry)) +
  geom_point(aes(size = ntips, color = tree.max.age)) + 
  labs(title = "", x = "Expansion", y = "Tippiness",
       color = "Clade age\n   (Myr)") +
  scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + 
  theme(legend.position = "none", 
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(2, 'cm'))
ggsave(file = "figures/fig_2/f2b2.svg",
       width = 20, height = 20, units = "cm", dpi = 300)

f2b3 <- 
  df2plot %>%
  ggplot(aes(x = asymmetry, y = log(peakedness))) +
  geom_point(aes(size = ntips, color = tree.max.age)) + 
  labs(title = "", x = "Tippiness", y = "Branch length heterogeneity") +
  scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + theme(legend.position = "none")
ggsave(file = "figures/fig_2/f2b3.svg",
       width = 20, height = 20, units = "cm", dpi = 300)

f2b4 <- 
  df2plot %>% 
  ggplot(aes(x = log(principal_eigenvalue), y = log(peakedness))) +
  geom_point(aes(size = ntips, color = tree.max.age)) + 
  labs(title = "", x = "Expansion", y = "Branch length heterogeneity") +
  scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + theme(legend.position = "none")
ggsave(file = "figures/fig_2/f2b4.svg",
       width = 20, height = 20, units = "cm", dpi = 300)

ggarrange(f2b2, f2b3, f2b4,
          ncol = 1)
ggsave(file = "figures/fig_2/f2b234.svg",
       width = 20, height = 20, units = "cm", dpi = 300)
ggsave(file = "figures/fig_2/f2b234.pdf",
       width = 20, height = 20, units = "cm", dpi = 300)


# figure 3 stem branch length by taxon ####

# figure 1 Supp. 3D plots, by sampling scheme, shape and MGL ####
data_rs_ls <- 
list(sl_sum_stats  ,
     rank_sum_stats,
     fract_sum_node)

# shape
rotate <- 25
for(i in 1:length(data_rs_ls)){
  df2plot <- data_rs_ls[[i]] %>% filter(ntips > 20)
  
  png(file = paste0("figures/fig_1supp/f1_s",i, ".png"),
      width = 20, height = 20, units = "cm", res = 500)
  scatter3D(x = (df2plot$shape.yule), 
            y = (df2plot$colles.yule), 
            z = (df2plot$sackin.yule),  
            colvar = df2plot$tree.max.age,
            colkey = list(plot = T, 
                          cex.clab = .9, adj.clab = 0.5,
                          side.clab = 2, side = 3),
            col = pal,
            clab = "Clade age\n (Myr)",
            xlab = "Shape (Yule)", 
            ylab = "Colless (Yule)",
            zlab = "Sackin (Yule)",
            theta = rotate,
            phi = 30,
            pch = as.numeric(factor(df2plot$taxon))+11,
            cex = log(df2plot$ntips)/2,
            bty = "b2",
            add = FALSE) 
  dev.off()
}

# mgl
rotate <- 300 
for(i in 1:length(data_rs_ls)){
  df2plot <- data_rs_ls[[i]] %>% filter(ntips > 20)
  
  png(file = paste0("figures/fig_1supp/f1_mgl",i, ".png"),
      width = 20, height = 20, units = "cm", res = 500)
  scatter3D(x = log(df2plot$principal_eigenvalue), 
            y = df2plot$asymmetry, 
            z = log(df2plot$peakedness),  
            axes = T, 
            colvar = df2plot$tree.max.age,
            col = pal,
            colkey = list(plot = T, 
                          cex.clab = .9, adj.clab = 0.5,
                          side.clab = 2, side = 3),
            clab = "Clade age\n (Myr)",
            xlab = "Expansion", 
            ylab = "Tippiness",
            zlab = "Branch length heterogeneity",
            theta = rotate, phi = 30,
            pch = as.numeric(factor(df2plot$taxon))+11,
            cex = log(df2plot$ntips)/2,
            bty = "b2",
            #type = "h",
            #ticktype = "detailed", #tick numbers
            add = FALSE, no.margin = T)
  dev.off()
}

par(mfrow = c(3,2))

## figure 8 Supp. posterior vs mcc tree  ####
data_rs_ls <- 
  list(all_10_slic,  
       all_10_rank,  
       all_10_rando)

# shape
rotate <- 25
for(i in 1:length(data_rs_ls)){
  df2plot <- data_rs_ls[[i]] %>% filter(ntips > 15)
  
  png(file = paste0("figures/fig_8supp/f8_s",i, ".png"),
      width = 20, height = 20, units = "cm", res = 500)
  scatter3D(x = (df2plot$shape.yule), 
            y = (df2plot$colles.yule), 
            z = (df2plot$sackin.yule),  
            colvar = df2plot$tree.max.age,
            colkey = list(plot = T, 
                          cex.clab = .9, adj.clab = 0.5,
                          side.clab = 2, side = 3),
            col = pal,
            clab = "Clade age\n (Myr)",
            xlab = "Shape (Yule)", 
            ylab = "Colless (Yule)",
            zlab = "Sackin (Yule)",
            theta = rotate,
            phi = 30,
            pch = as.numeric(factor(df2plot$taxon))+14,
            cex = log(df2plot$ntips)/2,
            bty = "b2",
            add = FALSE) 
  dev.off()
}

# mgl
rotate <- 300 
for(i in 1:length(data_rs_ls)){
  df2plot <- data_rs_ls[[i]] %>% filter(ntips > 20)
  
  png(file = paste0("figures/fig_8supp/f8_mgl",i, ".png"),
      width = 20, height = 20, units = "cm", res = 500)
  scatter3D(x = log(df2plot$principal_eigenvalue), 
            y = df2plot$asymmetry, 
            z = log(df2plot$peakedness),  
            axes = T, 
            colvar = df2plot$tree.max.age,
            col = pal,
            colkey = list(plot = T, 
                          cex.clab = .9, adj.clab = 0.5,
                          side.clab = 2, side = 3),
            clab = "Clade age\n (Myr)",
            xlab = "Expansion", 
            ylab = "Tippiness",
            zlab = "Branch length heterogeneity",
            theta = rotate, phi = 30,
            pch = as.numeric(factor(df2plot$taxon))+13,
            cex = log(df2plot$ntips)/2,
            bty = "b2",
            #type = "h",
            #ticktype = "detailed", #tick numbers
            add = FALSE)
  dev.off()
}

# figure X Supp. metric vs tree age by taxon ####
i=1
df2plot %>% filter(ntips >= min_tips[i]) %>% 
  dplyr::select(any_of(n2subs)) %>% 
  mutate(ln_principal_eigenvalue = log(principal_eigenvalue),
         ln_peakedness = log(peakedness), .keep = "unused") %>% 
  gather(metric, value, -ntips, -taxon, -tree.max.age) %>% 
  ggplot(aes(x = log(tree.max.age), y = value)) +
  geom_point(aes(size = ntips, color = taxon)) +
  labs(title = "", x = "Ln Tree age (Myr)", y = "Metric") +
  #scale_colour_gradientn(colours = pal) +
  theme_classic(base_size = 13) + theme(legend.position = "none") +
  facet_wrap(~ metric, scales = "free")

###
all_1k_sum %>% select(shape.yule, colles.yule, sackin.yule, 
                      shape.pda, colles.pda, sackin.pda, taxon) %>% 
  gather(metric, value, -taxon) %>% 
  ggplot(aes(x = taxon, y = value, fill = taxon)) + 
  #geom_violin(trim = FALSE) +
  #geom_boxplot(width = 0.1, fill = "white") +
  geom_point(fill = "white")
labs(title = "", x = "", y = "") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_classic() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1)) +
  facet_wrap(~ metric, scales = "free", ncol = 2)

all_1k_sum %>% select(principal_eigenvalue, asymmetry, peakedness, modalities, taxon) %>% 
  gather(metric, value, -taxon) %>% 
  ggplot(aes(x = taxon, y = value, fill = taxon)) + 
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + 
  labs(title = "", x = "", y = "") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_classic() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1)) +
  facet_wrap(~ metric, scales = "free", ncol = 2)

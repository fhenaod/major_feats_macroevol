# RPANDA
library(RPANDA)

tree_l <- do.call(c, unlist(tree_list, recursive = F))
table(sapply(tree_l, Ntip)>20)

jsd_dist <- JSDtree(tree_l[sapply(tree_l, Ntip)>20])
saveRDS(jsd_dist, "jsd_dist.rds")
jsd_dist <- readRDS("motifs/jsd_dist.rds")

jsd_clust <- JSDtree_cluster(jsd_dist, alpha = 0.95, draw = T)
saveRDS(jsd_clust, "jsd_clust.rds")
jsd_clust <- readRDS("motifs/jsd_clust.rds")
jsd_clust$clusters

library(tidyverse)
library(vegan)
library(MASS)
library(caret)

# filter, select and clean data
fms_ss <- rank_sum_stats %>% filter(rank == 'fams') %>% 
  dplyr::select(principal_eigenvalue, asymmetry, peakedness, 
                modalities) %>% 
  filter(!is.na(principal_eigenvalue)) #%>% scale()

vert_fms <- rank_sum_stats %>% filter(rank == 'fams') %>% 
  filter(taxon %in% c("amph", "bird", "chon", "fish", "mamm", "squa")) %>% 
  dplyr::select(principal_eigenvalue, asymmetry, peakedness, 
                modalities) %>% 
  filter(!is.na(principal_eigenvalue)) #%>% scale()

# hclust method ####
dist_mat <- dist(fms_ss, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg, cex = .00001)
rect.hclust(hclust_avg , k = 5, border = 2:6)
abline(h = 3, col = 'red')

# to plot branches with k colors
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 5)
plot(avg_col_dend)

# k-medioids ####
library(cluster)
library(factoextra)
library(philentropy)

fams_dist <- JSD(fms_ss, test.na = F)
view(fams_dist)
# all data
fviz_nbclust(fms_ss, kmeans, method = "silhouette") + 
  labs(subtitle = "All data") +
  geom_hline(yintercept = .5, linetype = 3, col = "red")

fams_k <- pam(fms_ss, k = 2, metric = "euclidean", stand = FALSE)

fviz_cluster(fams_k)

# only vertebrates
fviz_nbclust(vert_fms, kmeans, method = "silhouette") + 
  labs(subtitle = "Vertebrates") +
  geom_hline(yintercept = .5, linetype = 3, col = "red")

vert_k <- pam(fms_ss, k = 2, metric = "euclidean", stand = FALSE)

fviz_cluster(vert_k)

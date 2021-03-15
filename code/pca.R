library(FactoMineR)
library(factoextra)
library(tidyverse)

# results dataframes
sl_sum_stats # sliced concensus trees
rank_sum_stats # rank sampling consensus trees
fract_sum_node # self-similar sampling by nodes consensus trees
fract_sum_edg  # self-similar sampling by edges consensus trees

df2pca <- fract_sum_edg

pca_res <- df2pca %>% keep(is.numeric) %>% 
  select(-c(p.coless.t.y.less, p.coless.t.y.great, p.lt.yule, 
            p.coless.t.pda.less, p.coless.t.pda.great, p.lt.pda,
            #shape.pda, colles.pda, sackin.pda,
            b_low_ci, b_up_ci, tree.min.age, trees_mean_dr)) %>% 
  PCA(graph = F)

fviz_eig(pca_res, addlabels = T) # scree plot
pca_vars <- get_pca_var(pca_res) # extract pca result vars

fviz_pca_var(pca_res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T) + theme_classic()

# vars contributions
fviz_contrib(pca_res, choice = "var", axes = 1, top = 10) # Vars contributions to PC1
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10) # Vars contributions to PC2

fviz_contrib(pca_res, choice = "var", axes = 1:2, top = 10) # Vars contributions to PC1&2

# pca plot most important contrubuting vars
fviz_pca_var(pca_res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             #alpha.var = "contrib",
             repel = T) + theme_classic()

# plot individuals pca by taxon 
fviz_pca_ind(pca_res, geom.ind = "point",
             col.ind = df2pca$taxon,
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = F, 
             #ellipse.type = "confidence",
             legend.title = "Taxon", repel = T) + theme_classic()

# plot individuals pca by rank
fviz_pca_ind(pca_res, geom.ind = "point", 
             col.ind = df2pca$rank,
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = F, 
             #ellipse.type = "confidence", 
             legend.title = "Rank", repel = T) + theme_classic()

# plot individuals pca by rank, col by age
fviz_pca_ind(pca_res, geom.ind = "point", 
             col.ind = df2pca$tree.max.age,
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = F, 
             #ellipse.type = "confidence", 
             legend.title = "Rank", repel = T) + theme_classic()


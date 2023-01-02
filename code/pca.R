library(FactoMineR)
library(factoextra)
library(tidyverse)

# results dataframes
sl_sum_stats # sliced concensus trees
rank_sum_stats # rank sampling consensus trees
fract_sum_node # self-similar sampling by nodes consensus trees
fract_sum_edg  # self-similar sampling by edges consensus trees=

# PCA ####
df2pca <- rank_sum_stats %>% filter(ntips >= 20)
pca_res <- df2pca %>% keep(is.numeric) %>% head()
  dplyr::select(-c(p.coless.t.y.less, p.coless.t.y.great, p.lt.yule, 
            p.coless.t.pda.less, p.coless.t.pda.great, p.lt.pda,
            #shape.pda, colles.pda, sackin.pda,
            b_low_ci, b_up_ci, tree.min.age, trees_mean_dr)) %>% 
  PCA(graph = T)

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
             alpha.var = "contrib",
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

# prepare data #####
library(caret)
library(MASS)

df2lda <- rank_sum_stats %>% filter(ntips >= 20) %>% 
  dplyr::select(gamma.stat, 
                shape.yule, colles.yule,
                shape.pda, colles.pda,
                principal_eigenvalue, asymmetry, 
                peakedness, modalities, 
                taxon) %>% 
  mutate(taxon = recode(taxon, "agar" = "Agaricomycetes", "amph" = "Amphibia", 
                        "bird" = "Aves", "chon" = "Chondrichthyes", 
                        "fern" = "Polypodiopsida", 
                        "fish" = "Actinopterygii", "seed" = "Spermatophyta",
                        "mamm" = "Mammalia", "squa" = "Squamata")) %>% 
  filter(!is.na(principal_eigenvalue))

training.samples <- df2lda %>% pull(taxon) %>% 
  createDataPartition(p = .8, list = F)

train.data <- df2lda[training.samples, ]
test.data <- df2lda[-training.samples, ]

preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))

train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

# LDA ####
m_lda <- lda(taxon~., data = train.transformed)
predictions <- m_lda %>% predict(test.transformed) # predictions
mean(predictions$class==test.transformed$taxon) # model accuracy

lda.data <- cbind(train.transformed, predict(m_lda)$x)
ggplot(lda.data, aes(LD1, LD2)) + geom_point(aes(color = taxon)) +
  theme_classic()

ggord::ggord(m_lda, train.transformed$taxon, 
             ellipse = TRUE, ellipse_pro = 0.95, grp_title = "Taxa") + 
  theme_classic()

ldahist(data = predictions$x[,1], g = train.transformed$taxon)

# QDA ####
m_qda <- qda(taxon~., data = train.transformed)

predictions <- model %>% predict(test.transformed) # predictions
mean(predictions$class == test.transformed$taxon) # accuracy

# MDA ####
m_mda <- mda::mda(taxon~., data = train.transformed)

predicted.classes <- m_mda %>% predict(test.transformed) # predictions
mean(predicted.classes == test.transformed$taxon) # accuracy


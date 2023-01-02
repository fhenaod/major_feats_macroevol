library(tidyverse)
library(broom); library(kableExtra)
library(ggpubr); library(grid)

# dataframes ####
# consensus trees
sl_sum_stats   # sliced concensus trees
rank_sum_stats # rank sampling consensus trees
fract_sum_node # self-similar sampling by nodes consensus trees
fract_sum_edg %>% group_by(s_age) %>% sample_n(200, replace = F) # self-similar sampling by edges consensus trees
rank_rdm_sum    # approximate-age-rank random trees

# flat matrix into a dataframe
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
# modified from: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[base::col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
  )
}

signed_rank = function(x) sign(x) * rank(abs(x))

# correlations ####
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

n2subs <- 
  c( "ntips"                , "tree.max.age", 
     "gamma.stat"           , "ln_dr"       ,       
     "shape.yule"           , "colles.yule" ,  "sackin.yule",                  
     "shape.pda"            , "colles.pda"  ,  "sackin.pda" ,                 
     "principal_eigenvalue" , "asymmetry"   ,  "peakedness" ,       
     "modalities"           , "taxon") 

min_tips <- c(rep(20, 3), 10, 10)
dir.create("figures/correls/")

pvals_props <- c()
res_plots <- list()
for(i in 1:length(res_all_ls)){
  
schem <- sampligs[i]
df2cor <- res_all_ls[[i]]

cor_mtx <-
df2cor %>% ungroup() %>% 
  filter(ntips >= min_tips[i]) %>% 
  dplyr::select(any_of(n2subs)) %>% 
  mutate(ln_principal_eigenvalue = log(principal_eigenvalue),
         ln_peakedness = log(peakedness), .keep = "unused") %>% 
  group_by(taxon) %>% 
  #group_map(~psych::lowerCor(.x, digits = 2, method = "spearman"))
  group_map(~Hmisc::rcorr(as.matrix(.x), type = "spearman"))

names(cor_mtx) <- df2cor %>% filter(ntips >= 20) %>% pull(taxon) %>% unique()

flat_mtx <-
lapply(X = cor_mtx, function(x) flattenCorrMatrix(x$r, x$P)) %>%
  bind_rows(., .id = "taxon") %>% 
  mutate(comp = paste(row, column, sep = " vs ")) 

# summary table
flat_mtx %>% #head()
  select(Comparison = comp, cor, taxon) %>% 
  group_by(Comparison) %>% 
  summarize(mean = mean(cor, na.rm = T) %>% round(2), 
            sd = sd(cor, na.rm = T) %>% round(2), n = n()) %>% 
  arrange(desc(mean)) %>% 
  mutate(cat = seq(1:dim(.)[1]), 
         se = sd/sqrt(n)) %>% 
  ggplot(aes(x = reorder(Comparison, cat), y = mean)) +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se)) +
  theme_classic(base_size = 12) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x.bottom = element_blank()) +
  labs(y = "Mean Spearman's rho (± SE)", x = "Paired comparison",
       subtitle = paste(schem)) +
  geom_hline(yintercept = 0, col = "red", lty = 2)
ggsave(filename = paste0("figures/correls/corr_", schem, ".png"), 
       width = 20, height = 20, units = "cm")

flat_mtx %>%
  select(Comparison = comp, cor, taxon) %>% 
  filter(Comparison == "Age vs Gamma_statistic")

#  signed test as lm
ms_ts <- 
flat_mtx %>%
  select(comp, cor) %>%
  #group_by() %>% 
  nest(data = -comp) %>% #view()
  mutate( fit = map(data, ~ wilcox.test(.x$cor, mu = 0)),
          #fit = map(data, ~ lm(signed_rank(.x$cor) ~ 1)),
         glance = purrr::map(fit, glance),
         tidied = purrr::map(fit, tidy, conf.level = .95)
         #,augmented = purrr::map(fit, augment)
         )

sign_rs <- 
ms_ts %>% unnest(tidied) %>% 
  select(Comparison = comp, p.value) %>% 
  arrange(Comparison) %>%
  mutate(row = row_number()) %>% 
  filter(p.value<=0.05) %>% 
  select(row) %>% pull(row)

ms_ts %>% unnest(tidied) %>% 
  select(Comparison = comp, p.value) %>% 
  arrange(Comparison) %>% 
  kable("html", digits = 4) %>% 
  kable_styling(c("striped", "scale_down"), full_width = F) %>% 
  row_spec(sign_rs, bold = T) %>%
  as_image(file = paste0("figures/correls/table_corr_", schem,".png"),
           density = 300)

# r boxplot 
flat_mtx %>% #arrange(row, column) %>% view()
  select(comp, row, cor, taxon) %>% 
  group_by(comp) %>% 
  ggplot(aes(x = reorder(comp, -cor, na.rm = T), y = cor)) + 
  geom_boxplot(outlier.color = "red", outlier.size = .5) + 
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x.bottom = element_blank()) +
  labs(x = "Paired comparison", y = "Spearman's rho", subtitle = paste(schem)) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  facet_wrap(~ row)
ggsave(filename = paste0("figures/correls/panell_corr_", schem, ".png"), 
       width = 20, height = 20, units = "cm")

pvals_props[[i]] <- 
ms_ts %>% unnest(tidied) %>% 
  select(Comparison = comp, p.value) %>% 
  mutate(con = p.value<=0.05) %>% group_by(con) %>% 
  summarize(n = n()) %>% mutate(freq = (n / sum(n)) %>% round(2))  
  
# p.vals plot
res_plots[[i]] <- 
ms_ts %>% unnest(tidied) %>% 
  select(Comparison = comp, p.value) %>% 
  ggplot(aes(x = p.value)) + 
  geom_histogram() + theme_classic(base_size = 12) +
  xlim(c(-0.05,1.05)) +
  labs(x = "", y = "") +
  geom_vline(xintercept = 0.05, col = "red", lty = 2)
}

# unified plot ####
length(res_plots)
ggarrange(plotlist = res_plots[1:4], 
          #ncol = 2, nrow = 2, 
          labels = "AUTO") %>% 
  annotate_figure(left = textGrob("Count", 
                                  rot = 90, vjust = 2, hjust = -0.5, gp = gpar(cex = 1.5)) 
                  , bottom = textGrob("p-value", vjust = -1, gp = gpar(cex = 1.5))
  )
ggsave(filename = paste0("figures/correls/pvals_panel.png"), 
       width = 20, height = 20, units = "cm")

pvals_props[[3]]
pvals_props %>% sapply("[[", 3, simplify = T)

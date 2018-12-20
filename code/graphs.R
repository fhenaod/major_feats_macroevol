library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(ggthemes)

sum_stats<-data.frame(tree_metrics_sum, trees_mean_dr, ln_dr=log(trees_mean_dr), imbalance.metrics, n_cherries, outer_branches=sum_stats$ntips-sum_stats$n_cherries, trees_spec_sum)
#sum_bic_compare,

## Shape
hh1<-ggplot(sum_stats, aes(x=shape.yule)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Shape (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 20), y=c(0, 20)), aes(x, y)) 

hh2<-ggplot(sum_stats, aes(x=shape.pda)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Shape (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-20, 10), y=c(0, 30)), aes(x, y)) 

ggarrange(hh1, hh2,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

## Colles & Sackin 
hh3<-ggplot(sum_stats, aes(x=colles.yule)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Colles Index (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-5, 15), y=c(0, 20)), aes(x, y)) 

hh4<-ggplot(sum_stats, aes(x=sackin.yule)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Sackin Index (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-5, 15), y=c(0, 20)), aes(x, y)) 

hh5<-ggplot(sum_stats, aes(x=colles.pda)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Colles Index (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 2), y=c(0, 20)), aes(x, y)) 

hh6<-ggplot(sum_stats, aes(x=sackin.pda)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Sackin Index (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 2), y=c(0, 20)), aes(x, y)) 

ggarrange(hh3, hh4, hh5, hh6,   
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

# Outer and Cherry branches
hh7<-ggplot(sum_stats, aes(x = n_cherries)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Cherry branches", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 1500), y=c(0, 60)), aes(x, y)) 

hh8<-ggplot(sum_stats, aes(x = outer_branches)) + 
  geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Outer branches", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 3000), y=c(0, 60)), aes(x, y)) 

ggarrange(hh7, hh8,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Prin. eigenvalue, asymmetry, peakedness and modalities
hh9<-ggplot(sum_stats, aes(x=log(principal_eigenvalue))) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Log Principal eigenvalue", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(2, 15), y=c(0, 20)), aes(x, y)) 

hh10<-ggplot(sum_stats, aes(x=asymmetry)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Asymmetry", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-1, 2.5), y=c(0, 20)), aes(x, y)) 

hh11<-ggplot(sum_stats, aes(x=peakedness)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Peakedness", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 12), y=c(0, 20)), aes(x, y)) 

hh12<-ggplot(sum_stats, aes(x=modalities)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Modalities", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(1, 8350), y=c(0, 100)), aes(x, y)) 

#hh13<-ggplot(sum_stats, aes(x=log(modalities))) + geom_histogram(color="darkblue", fill="white") + 
#  labs(title="", x="Modalities", y = "Count") + theme_tufte(base_family = "Helvetica") + 
#  geom_rangeframe(data=data.frame(x=c(-5, 10), y=c(0, 50)), aes(x, y)) 

ggarrange(hh9, hh10, hh11, hh12,   
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


sum_stats_f<-select(sum_stats, ntips, tree.min.age, tree.max.age, gamma.stat, trees_mean_dr, ln_dr,
                    shape.yule, colles.yule, sackin.yule, shape.pda, colles.pda, sackin.pda, n_cherries,
                    outer_branches, principal_eigenvalue, asymmetry, peakedness, modalities)

pairs(sum_stats_f)

# Correlation table
par(mfrow=c(1,1))
# propertCorrM
M<-psych::lowerCor(sum_stats_f,digits=2, method = "spearman")
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(sum_stats_f)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(M, method="color", col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", number.cex = .7,  # Coefficient of correlation, size
                   tl.col="black", tl.srt=45, tl.cex = .7,  #Text label color, rotation, size
                   # Combine with significance
                   p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE, mar = c(0.5,0.5,0.5,0.5))

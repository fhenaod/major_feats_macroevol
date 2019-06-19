library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(visreg)
library(plotly)
library(scales)

# Shape hists ####
hh1<-ggplot(sum_stats, aes(x=shape.yule)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept = mean(subset(sum_stats, !is.na(shape.yule))$shape.yule)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Shape (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c(min(subset(sum_stats, !is.na(shape.yule))$shape.yule), max(subset(sum_stats, !is.na(shape.yule))$shape.yule)),
                                  y = c(0, 5)), aes(x, y)) 

hh2<-ggplot(sum_stats, aes(x=shape.pda)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept= mean(subset(sum_stats, !is.na(shape.pda))$shape.pda)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Shape (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c(min(subset(sum_stats, !is.na(shape.pda))$shape.pda), max(subset(sum_stats, !is.na(shape.pda))$shape.pda)), 
                                  y = c(0, 5)), aes(x, y)) 

ggarrange(hh1, hh2,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Colless & Sackin hists ####
hh3<-ggplot(sum_stats, aes(x=colles.yule)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept = mean(subset(sum_stats, !is.na(colles.yule))$colles.yule)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Colless Index (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-2, 15), y=c(0, 10)), aes(x, y)) 

hh4<-ggplot(sum_stats, aes(x=sackin.yule)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept= mean(subset(sum_stats, !is.na(sackin.yule))$sackin.yule)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Sackin Index (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-2, 15), y=c(0, 10)), aes(x, y)) 

hh5<-ggplot(sum_stats, aes(x=colles.pda)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept= mean(subset(sum_stats, !is.na(colles.pda))$colles.pda)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Colless Index (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 2), y=c(0, 10)), aes(x, y)) 

hh6<-ggplot(sum_stats, aes(x=sackin.pda)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept= mean(subset(sum_stats, !is.na(sackin.pda))$sackin.pda)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Sackin Index (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 2), y=c(0, 10)), aes(x, y)) 

ggarrange(hh3, hh4, hh5, hh6,   
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

# (Aldous) Beta  ####
hh1.1<-ggplot(sum_stats, aes(x=beta)) + geom_histogram(color="darkblue", fill="white") +  
  geom_vline(aes(xintercept = mean(subset(sum_stats, !is.na(beta))$beta)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Beta", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(min(subset(sum_stats, !is.na(beta))$beta), max(subset(sum_stats, !is.na(beta))$beta)),
                                  y=c(0, 10)), aes(x, y)) 

hh2.1<-ggplot(subset(sum_stats, !is.na(beta)), aes(x=log(tree.max.age), y = beta)) +
  geom_point(size=1, shape = 1, color = "darkblue") + 
  labs(title="", x="Ln Clade age (Myr)", y = "Beta") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(log(min(subset(sum_stats, !is.na(beta))$tree.max.age)), log(max(subset(sum_stats, !is.na(beta))$tree.max.age))),
                                  y=c(min(subset(sum_stats, !is.na(beta))$beta), max(subset(sum_stats, !is.na(beta))$beta))), aes(x, y)) 

ggarrange(hh1.1, hh2.1,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# p-value tests ####
hist(table(sum_stats$p.coless.t.y.less<=0.05))
hist(table(sum_stats$p.coless.t.y.great<=0.05))
hist(table(sum_stats$p.coless.t.pda.less<=0.05))
hist(table(sum_stats$p.coless.t.pda.great<=0.05))
hist(table(sum_stats$p.lt.yule<=0.05))
hist(table(sum_stats$p.lt.pda<=0.05))

# Outer and Cherry branches ####
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

# Prin. eigenvalue, asymmetry, peakedness and modalities hists ####
hh9<-ggplot(sum_stats, aes(x=log(principal_eigenvalue))) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept = mean(log(subset(sum_stats, !is.na(principal_eigenvalue))$principal_eigenvalue))), color="red", linetype="dashed", size=1) +
  labs(title = "", x = "Ln Principal eigenvalue", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c(log(min(subset(sum_stats, !is.na(principal_eigenvalue))$principal_eigenvalue)), log(max(subset(sum_stats, !is.na(principal_eigenvalue))$principal_eigenvalue))), 
                                  y = c(0, 10)), aes(x, y)) 

hh10<-ggplot(sum_stats, aes(x=asymmetry)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept = mean(subset(sum_stats, !is.na(asymmetry))$asymmetry)), color="red", linetype="dashed", size=1) +
  labs(title = "", x = "Asymmetry", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c((min(subset(sum_stats, !is.na(asymmetry))$asymmetry)), (max(subset(sum_stats, !is.na(asymmetry))$asymmetry))), 
                                  y = c(0, 10)), aes(x, y)) 

hh11<-ggplot(sum_stats, aes(x=peakedness)) + geom_histogram(color="darkblue", fill="white") + 
  geom_vline(aes(xintercept = mean(subset(sum_stats, !is.na(peakedness))$peakedness)),color="red", linetype="dashed", size=1) +
  labs(title = "", x = "Peakedness", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c((min(subset(sum_stats, !is.na(peakedness))$peakedness)), (max(subset(sum_stats, !is.na(peakedness))$peakedness))),
                                  y = c(0, 10)), aes(x, y)) 

hh12<-ggplot(sum_stats, aes(x=log(modalities))) + geom_histogram(color="darkblue", fill="white") +
  geom_vline(aes(xintercept = mean(log(subset(sum_stats, !is.na(modalities))$modalities))),color="red", linetype="dashed", size=1) +
  labs(title = "", x = "Ln Modalities", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c(log(min(subset(sum_stats, !is.na(modalities))$modalities)), log(max(subset(sum_stats, !is.na(modalities))$modalities))),
                                  y = c(0, 10)), aes(x, y)) 

ggarrange(hh9, hh10, hh11, hh12,   
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

# Mean and CI95 ####
x<-log(sum_stats$modalities)
round(mean(x),2) ## MEAN Lambda
sem<-sd(x)/sqrt(length(x)) # SE
round(c(mean(x)-2*sem,mean(x)+2*sem),2) # CI95
rm(x)

# DR & Gamma-stat hist
hh13<-ggplot(sum_stats, aes(x=ln_dr)) + geom_histogram(color="darkblue", fill="white") + 
  #geom_vline(aes(xintercept = mean(subset(sum_stats, !is.na(ln_dr))$ln_dr)),color="red", linetype="dashed", size=1) +
  labs(title="", x="DR", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c(-5, 0),
                                  y = c(0, 50)), aes(x, y)) 

hh14<-ggplot(sum_stats, aes(x=gamma.stat)) + geom_histogram(color="darkblue", fill="white") + 
  #geom_vline(aes(xintercept= mean(subset(sum_stats, !is.na(gamma.stat))$gamma.stat)),color="red", linetype="dashed", size=1) +
  labs(title="", x="Gamma statistic", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x = c(-60, 30), 
                                  y = c(0, 250)), aes(x, y)) 

ggarrange(hh13, hh14,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


# Regressions Vs. Age ####

# IMBALANCE
par(mfrow=c(2,2))
visreg(lm((colles.yule)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Colless Index (Yule)")
visreg(lm((sackin.yule)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Sackin Index (Yule)")
visreg(lm((colles.pda)~log(tree.max.age),  data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Colless Index (PDA)")
visreg(lm((sackin.pda)~log(tree.max.age),  data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Sackin Index (PDA)")

# SHAPE
par(mfrow=c(1,2))
visreg(lm(shape.yule~(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab = "Shape (Yule)")
visreg(lm(shape.pda~(tree.max.age),  data=sum_stats), xlab= "Ln Clade age (My)", ylab = "Shape (PDA)")

summary(lm(shape.yule~log(tree.max.age), data=sum_stats))
summary(lm(shape.pda~log(tree.max.age), data=sum_stats))

# CHERRIES-OUTER
par(mfrow=c(1,2))
visreg(lm(log(n_cherries)~log(tree.max.age), data=sum_stats),     xlab= "Ln Clade age (My)", ylab= "Ln Cherry branches")
visreg(lm(log(outer_branches)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Ln Outer branches")

summary(lm(log(n_cherries)~log(tree.max.age), data=sum_stats))
summary(lm(log(outer_branches)~log(tree.max.age), data=sum_stats))

# BRANCH LENGTHS
hh14<-ggplot(sum_stats, aes(x = log(tree.max.age), y = br.len_mean)) +
  geom_point(size = 1, shape = 1, color = "darkblue") + 
  labs(title = "", x = "Ln Clade age (Myr)", y = "Mean branch length") + 
  theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data = data.frame(x = c(log(min(sum_stats$tree.max.age)), log(max(sum_stats$tree.max.age))), 
                                  y = c(min(sum_stats$br.len_mean), max(sum_stats$br.len_mean))), aes(x, y)) 

hh15<-ggplot(sum_stats, aes(x = log(tree.max.age), y = br.t_mean)) +
  geom_point(size = 1, shape = 1, color = "darkblue") + 
  labs(title = "", x = "Ln Clade age (Myr)", y = "Mean branch times") + 
  theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data = data.frame(x = c(log(min(sum_stats$tree.max.age)), log(max(sum_stats$tree.max.age))),
                                  y = c(min(sum_stats$br.t_mean), max(sum_stats$br.t_mean))), aes(x, y)) 

ggarrange(hh14, hh15,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# RPANDA
par(mfrow=c(2,2))
visreg(lm(log(principal_eigenvalue)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Ln Principal eigenvalue")
visreg(lm(log(asymmetry+min(asymmetry)+1)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Ln Asymmetry")
visreg(lm(log(peakedness)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Ln Peakedness")
visreg(lm(log(modalities)~log(tree.max.age), data=sum_stats), xlab= "Ln Clade age (My)", ylab= "Ln Modalities")

summary(lm(log(principal_eigenvalue)~log(tree.max.age), data=sum_stats))
summary(lm(log(asymmetry+min(asymmetry)+1)~log(tree.max.age), data=sum_stats))
summary(lm(log(peakedness)~log(tree.max.age), data=sum_stats))
summary(lm(log(modalities)~log(tree.max.age), data=sum_stats))

# Correlation matrix ####
par(mfrow=c(1,1))
# propertCorrM
M<-psych::lowerCor(sum_stats[1:28],digits = 2, method = "spearman")
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
p.mat <- cor.mtest(sum_stats[1:28])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(M, method="color", col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu"),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", number.cex = .6,  # Coefficient of correlation, size
                   tl.col="black", tl.srt=45, tl.cex = .6,  #Text label color, rotation, size
                   # Combine with significance
                   p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE, mar = c(0.06,0.06,0.06,0.06))
# 3D-plot ####
#express<-expression(paste("Ln mean ", lambda, " (species ", Myr^-1,")"),sep=" ")
plot_ly(sum_stats, x = ~log(principal_eigenvalue), y = ~asymmetry, z = ~peakedness,
        type = "scatter3d", mode = "lines+markers",
        line = list(color= "#EA3770", width = 4, dash = 'dash'),
        marker = list(symbol = 'circle', sizemode = 'area', 
                      color = ~tree.max.age, size = ~ntips,
                      colorbar = list(title = 'Clade age (Myr)'), colorscale='Viridis', reversescale = T)) %>%
  layout(
    title = "Polypodiopsida",
    scene = list( xaxis = list(title = "λ*"),
                  yaxis = list(title = "ψ"),
                  zaxis = list(title = "η"))
    )


plot_ly(sum_stats, x = ~log(principal_eigenvalue), y = ~asymmetry, z = ~(peakedness),
        type = "scatter3d", mode = "markers",
        marker = list(symbol = 'circle', sizemode = 'area', 
                      color = ~tree.max.age, size = ~ntips,
                      colorbar = list(title = 'Clade age (Myr)'), colorscale='Viridis', reversescale = T)) %>%
  layout(
    title = "",
    scene = list( xaxis = list(title = "Ln Principal eigenvalue"),
                  yaxis = list(title = "Skewness"),
                  zaxis = list(title = "Peak height"))
  )

# Break point age
plot_ly(sum_stats, x = ~log(principal_eigenvalue), y = ~asymmetry, z = ~(peakedness),
        type = "scatter3d", mode = "markers",
        marker = list(symbol = 'circle', sizemode = 'area', 
                      color = ~orig_brake_age_f, size = ~ntips,
                      colorbar = list(title = 'Break point age (Myr)'), colorscale = 'Viridis', reversescale = T)) %>%
  layout(
    title = "",
    scene = list( xaxis = list(title = "Ln λ*"),
                  yaxis = list(title = "ψ"),
                  zaxis = list(title = "η"))
  )

# Violin plots #####
sum_stats$taxon<-fct_relevel(sum_stats$taxon, 
                              "bird_5", "bird_10", "bird_20", "bird_30", "bird_40", "bird_50"
                             #"sim_bird_5", "sim_bird_10", "sim_bird_20", "sim_bird_30", "sim_bird_40", "sim_bird_50"
                             )

vpp<-ggplot(sum_stats, aes(x = taxon, y = tree.max.age, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Clade age") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp<-ggplot(sum_stats, aes(x = taxon, y = ln_dr, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Ln DR") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp1<-ggplot(sum_stats, aes(x = taxon, y = gamma.stat, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Gamma statistic") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp2<-ggplot(sum_stats, aes(x = taxon, y = beta, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Beta") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))

ggarrange(vpp, vp, vp1, vp2,    
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

vp3<-ggplot(sum_stats, aes(x = taxon, y = shape.yule, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Shape (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp4<-ggplot(sum_stats, aes(x = taxon, y = shape.pda, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Shape (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))

ggarrange(vp3, vp4,    
          labels = c("A", "B", "C", "D"),
          ncol = 2)

vp5<-ggplot(sum_stats, aes(x = taxon, y = colles.pda, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Colless (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp6<-ggplot(sum_stats, aes(x = taxon, y = sackin.pda, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Sackin (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp7<-ggplot(sum_stats, aes(x = taxon, y = colles.yule, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Colless (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp8<-ggplot(sum_stats, aes(x = taxon, y = sackin.yule, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "", x = "", y = "Sackin (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))

ggarrange(vp5, vp6, vp7, vp8,    
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

vp9<-ggplot(sum_stats, aes(x = taxon, y = log(principal_eigenvalue), fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "",x = "", y = "Ln λ") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp10<-ggplot(sum_stats, aes(x = taxon, y = asymmetry, fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "",x = "", y = "ψ") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp11<-ggplot(sum_stats, aes(x = taxon, y = log(peakedness), fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "",x = "", y = "Ln η") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))
vp12<-ggplot(sum_stats, aes(x = taxon, y = log(modalities), fill = taxon)) + geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") + labs(title = "",x = "", y = "Ln Modalities") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 6, angle = 45, vjust = 0.1))

ggarrange(vp9, vp10, vp11, vp12,    
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

## ALL clades ####

# Shape vs Absolute Age
gg1<-ggplot(sum_stats, aes(x = (tree.max.age), y = shape.yule, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Clade age (Myr)", y = "Shape (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE)

gg2<-ggplot(sum_stats, aes(x = (tree.max.age), y = shape.pda, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Clade age (Myr)", y = "Shape (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE)

# Shape vs Relative age
gg1<-ggplot(sum_stats, aes(x = (rel_age), y = shape.yule, color = taxon)) + 
  geom_point(size = .9) + labs(title = "", x = "Relative age", y = "Shape (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none")
gg2<-ggplot(sum_stats, aes(x = (rel_age), y = shape.pda,  color = taxon)) + 
  geom_point(size = .9) + labs(title = "", x = "Relative age", y = "Shape (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme()

ggarrange(gg1, gg2,   
          labels = c("A", "B"),
          ncol = 2)

# Shape vs tree size
ggg1<-ggplot(sum_stats, aes(x = log(ntips), y = shape.yule, color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Ln Tree size", y = "Shape (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") #+ geom_smooth(method=lm, se=FALSE)

ggg2<-ggplot(sum_stats, aes(x = (ntips), y = shape.pda, color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Tree size", y = "Shape (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none") #+ geom_smooth(method=lm, se=FALSE)

ggarrange(ggg1, ggg2,   
          labels = c("A", "B"),
          ncol = 2)

# Colless/Sackin vs Age
g1<-ggplot(sum_stats, aes(x = (tree.max.age), y = colles.yule, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Colless (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=lm, se=FALSE)
g2<-ggplot(sum_stats, aes(x = (tree.max.age), y = sackin.yule, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Sackin (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") #+ geom_smooth(method=lm, se=FALSE)
g3<-ggplot(sum_stats, aes(x = (tree.max.age), y = colles.pda, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Clade age (Myr)", y = "Colless (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=lm, se=FALSE)
g4<-ggplot(sum_stats, aes(x = (tree.max.age), y = sackin.pda, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Clade age (Myr)", y = "Sackin (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=lm, se=FALSE)

# Colless/Sackin vs Relative age
g1<-ggplot(sum_stats, aes(x = (rel_age), y = colles.yule, color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Colless (Yule)") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")
g2<-ggplot(sum_stats, aes(x = (rel_age), y = sackin.yule, color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Sackin (Yule)") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")
g3<-ggplot(sum_stats, aes(x = (rel_age), y = colles.pda, color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Relative clade age", y = "Colless (PDA)") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")
g4<-ggplot(sum_stats, aes(x = (rel_age), y = sackin.pda, color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Relative clade age", y = "Sackin (PDA)") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")

# Beta vs Relative age/Tree size
gp1<-ggplot(sum_stats, aes(x = (rel_age), y = log(beta+2.5), color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Relative age", y = "Ln Beta + 2.5") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=lm, se=FALSE)

gp2<-ggplot(sum_stats, aes(x = log(ntips), y = log(beta+2.5), color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Ln Tree size", y = "Ln Beta + 2.5") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme()# + geom_smooth(method=lm, se=FALSE)

ggarrange(gp1, gp2,   
          labels = c("A", "B"),
          ncol = 2)

# Colless/Sackin vs Tree size
g1<-ggplot(sum_stats, aes(x = (ntips), y = colles.yule, color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "", y = "Colless (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") # + geom_smooth(method=loess, se=FALSE)
g2<-ggplot(sum_stats, aes(x = (ntips), y = sackin.yule, color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "", y = "Sackin (Yule)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=loess, se=FALSE)
g3<-ggplot(sum_stats, aes(x = (ntips), y = colles.pda, color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Tree size", y = "Colless (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=loess, se=FALSE)
g4<-ggplot(sum_stats, aes(x = (ntips), y = sackin.pda, color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Tree size", y = "Sackin (PDA)") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none")# + geom_smooth(method=loess, se=FALSE)

# DR vs. Colless/Sackin
g1<-ggplot(sum_stats, aes(x = (colles.yule), y = log(trees_mean_dr), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Colless (Yule)", y = "Ln DR") +
  geom_smooth(method=lm, se=FALSE) + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + ylim(min(sum_stats$ln_dr, na.rm = T),3)
g2<-ggplot(sum_stats, aes(x = (sackin.yule), y = log(trees_mean_dr), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Sackin (Yule)", y = "") + 
  geom_smooth(method=lm, se=FALSE) + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + ylim(min(sum_stats$ln_dr, na.rm = T),3)
g3<-ggplot(sum_stats, aes(x = (colles.pda), y = log(trees_mean_dr), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Colless (PDA)", y = "Ln DR") +
  geom_smooth(method=lm, se=FALSE) + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + ylim(min(sum_stats$ln_dr, na.rm = T),3)
g4<-ggplot(sum_stats, aes(x = (sackin.pda), y = log(trees_mean_dr), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Sackin (PDA)", y = "") + 
  geom_smooth(method=lm, se=FALSE) + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + ylim(min(sum_stats$ln_dr, na.rm = T),3)

# DR vs. Beta
gpp1<-ggplot(sum_stats, aes(x = ln_dr, y = log(beta+2.5), color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Ln DR", y = "Ln Beta + 2.5") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + ylim(min(sum_stats$ln_dr, na.rm = T),3) # + geom_smooth(method=lm, se=FALSE)
gpp2<-ggplot(sum_stats, aes(x = (gamma.stat+50), y = log(trees_mean_dr), color = taxon)) +
  geom_point(size = .9) + labs(title = "", x = "Gamma statistic", y = "") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + ylim(min(sum_stats$ln_dr, na.rm = T),3) # + geom_smooth(method=lm, se=FALSE)

ggarrange(gpp1, gpp2,   
          labels = c("A", "B"),
          ncol = 2)

# RPANDA vs. age
g1<-ggplot(sum_stats, aes(x = log(tree.max.age), y = log(principal_eigenvalue), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Log λ*") +
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g2<-ggplot(sum_stats, aes(x = log(tree.max.age), y = asymmetry, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "ψ") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g3<-ggplot(sum_stats, aes(x = log(tree.max.age), y = log(peakedness), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Clade age (Myr)", y = "Log η") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g4<-ggplot(sum_stats, aes(x = log(tree.max.age), y = log(modalities), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Clade age (Myr)", y = "Log Modalities") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)

g1<-ggplot(sum_stats, aes(x = (rel_age), y = log(principal_eigenvalue), color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Log λ*") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")
g2<-ggplot(sum_stats, aes(x = (rel_age), y = asymmetry, color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "ψ") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")
g3<-ggplot(sum_stats, aes(x = (rel_age), y = log(peakedness), color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Relative clade age", y = "Ln η") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")
g4<-ggplot(sum_stats, aes(x = (rel_age), y = log(modalities), color = taxon)) + geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Relative clade age", y = "Ln Modalities") + scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + theme_minimal() + theme(legend.position = "none")

# RPANDA vs. brake_point_age
g1<-ggplot(sum_stats, aes(x = log(orig_brake_age_f), y = log(principal_eigenvalue), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Log λ*") +
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g2<-ggplot(sum_stats, aes(x = log(orig_brake_age_f), y = asymmetry, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "ψ") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g3<-ggplot(sum_stats, aes(x = log(orig_brake_age_f), y = log(peakedness), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Brake point age (Myr)", y = "Log η") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g4<-ggplot(sum_stats, aes(x = log(orig_brake_age_f), y = log(modalities), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln Brake point age (Myr)", y = "Log Modalities") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)

# RPANDA vs. DR
g1<-ggplot(sum_stats, aes(x = log(trees_mean_dr), y = log(principal_eigenvalue), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "Ln λ*") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g2<-ggplot(sum_stats, aes(x = log(trees_mean_dr), y = asymmetry, color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "", y = "ψ") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g3<-ggplot(sum_stats, aes(x = log(trees_mean_dr), y = log(peakedness), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln DR", y = "Ln η") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) + 
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)
g4<-ggplot(sum_stats, aes(x = log(trees_mean_dr), y = log(modalities), color = taxon)) +
  geom_point(size = .9, alpha=1/2) + labs(title = "", x = "Ln DR", y = "Ln Modalities") + 
  scale_colour_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(sum_stats$taxon)))) +
  theme_minimal() + theme(legend.position = "none") + geom_smooth(method=lm, se=FALSE, size = .7)

ggarrange(g1, g2, g3, g4,    
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
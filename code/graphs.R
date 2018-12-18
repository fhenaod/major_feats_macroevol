library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(ggthemes)

sum_stats<-data.frame(tree_metrics_sum, imbalance.metrics, n_cherries,trees_spec_sum, sum_bic_compare)

## Shape
hh1<-ggplot(sum_stats, aes(x=shape.yule)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Shape (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

hh2<-ggplot(sum_stats, aes(x=shape.pda)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Shape (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

ggarrange(hh1, hh2,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

## Colles & Sackin 
hh3<-ggplot(sum_stats, aes(x=colles.yule)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Colles Index (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

hh4<-ggplot(sum_stats, aes(x=sackin.yule)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Sackin Index (Yule)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

hh5<-ggplot(sum_stats, aes(x=colles.pda)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Colles Index (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

hh6<-ggplot(sum_stats, aes(x=sackin.pda)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Sackin Index (PDA)", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

ggarrange(hh3, hh4, hh5, hh6,   
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

# Outer and Cherry branches
hh7<-ggplot(sum_stats, aes(x=n_cherries)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Cherry branches", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 100), y=c(0, 20)), aes(x, y)) 

hh8<-ggplot(sum_stats, aes(x= sum_stats$ntips-sum_stats$n_cherries)) + 
  geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Outer branches", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 200), y=c(0, 20)), aes(x, y)) 

ggarrange(hh7, hh8,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Prin. eigenvalue, asymmetry, peakedness and modalities

hh9<-ggplot(sum_stats, aes(x=log(principal_eigenvalue))) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Log Principal eigenvalue", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-10, 10), y=c(0, 20)), aes(x, y)) 

hh10<-ggplot(sum_stats, aes(x=asymmetry)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Asymmetry", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(-5, 5), y=c(0, 20)), aes(x, y)) 

hh11<-ggplot(sum_stats, aes(x=peakedness)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Peakedness", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(1, 10), y=c(0, 20)), aes(x, y)) 

hh12<-ggplot(sum_stats, aes(x=modalities)) + geom_histogram(color="darkblue", fill="white") + 
  labs(title="", x="Modalities", y = "Count") + theme_tufte(base_family = "Helvetica") + 
  geom_rangeframe(data=data.frame(x=c(0, 30), y=c(0, 20)), aes(x, y)) 

ggarrange(hh9, hh10, hh11, hh12,   
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

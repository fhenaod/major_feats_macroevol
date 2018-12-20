library(RPANDA)
library(parallel)

d<-dir("data/")
t<-grep("phylo_",d)
tt<-d[t]
e.trees<-list()
#read trees
for(i in 1:length(tt)){
  typ<- strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][3]
  prefix<-paste(strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][2])
  tree <- read.tree(paste0("data/",tt[[i]]))
  e.trees[[i]]<-tree
}

# Estimate tree's spectrum from a list of trees
trees_spectR<-mclapply(e.trees,spectR, mc.cores = 2)

# Extract tree's spectrum summary stats
extract_spect=function(lap){
  principal_eigenvalue<-c()
  asymmetry<-c()
  peakedness<-c()
  modalities<-c()
  
spec_sum<-data.frame()  
  for(i in 1:length(lap)){
    principal_eigenvalue[i]<-lap[[i]]$principal_eigenvalue
    asymmetry[i]<-lap[[i]]$asymmetry
    peakedness[i]<-lap[[i]]$peakedness
    modalities[i]<-lap[[i]]$eigengap
  spec_sum<-data.frame(principal_eigenvalue,asymmetry,peakedness,modalities)
  }
return(spec_sum)
}
trees_spec_sum<-extract_spect(trees_spectR)
trees_spec_sum<-readRDS("trees_spec_sum.rds")
trees_spec_sum$modalities

# Estimate BIC trees modality number
bic.compare=function(tr,e.gap){
  bicc<-c() 
  bic.test.r<-c()
  bicRatio<-c()
  df<-data.frame()
   for (i in 1:length(tr)){
    bicc<-BICompare(tr[[i]],e.gap[[i]])
    bic.test.r[i]<-(bicc$BIC_test$`tree BIC`/bicc$BIC_test$`random BIC`)
    bicRatio[i]<-bicc$`BSS/TSS`
    df<-data.frame(bic.test.r,bicRatio)
  }
  return(df)
}
sum_bic_compare<-bic.compare(e.trees[1:2],trees_spec_sum$modalities[1:2])


library(RPANDA)
library(parallel)

# Estimate tree's spectrum from a list of trees
trees_spectR<-mclapply(bird.sl,spectR, mc.cores = 3)

# Extract tree's spectrum summary stats ####
extract_spect=function(lap){
  principal_eigenvalue<-c()
  asymmetry<-c()
  peakedness<-c()
  modalities<-c()
  
spec_sum<-data.frame()  
  for(i in 1:length(lap)){
    if (is.null(trees_spectR[[i]])) {
      principal_eigenvalue[i]<-NA
      asymmetry[i]<-NA
      peakedness[i]<-NA
      modalities[i]<-NA
      }
    else {
      principal_eigenvalue[i]<-lap[[i]]$principal_eigenvalue
      asymmetry[i]<-lap[[i]]$asymmetry
      peakedness[i]<-lap[[i]]$peakedness
      modalities[i]<-lap[[i]]$eigengap}
    
  spec_sum<-data.frame(principal_eigenvalue,asymmetry,peakedness,modalities)
  }
return(spec_sum)
}
trees_spec_sum<-extract_spect(trees_spectR)
trees_spec_sum<-readRDS("trees_spec_sum.rds")
trees_spec_sum$modalities

# Estimate BIC trees modality number ####
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
sum_bic_compare<-bic.compare(bird.sl,trees_spec_sum$modalities)

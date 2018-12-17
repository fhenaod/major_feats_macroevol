library(RPANDA)
library(parallel)
data("Phyllostomidae")
data("Phyllostomidae_genera")

# Estimate tree's spectrum from a list of trees
trees.spect<-lapply(Phyllostomidae_genera,spectR)

# Extract tree's spectrum summary stats
extract_spect=function(l){
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
trees.spect.sum<-extract_spect(trees.spect)
trees.spect.sum$modalities

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
bc.s<-bic.compare(Phyllostomidae_genera,trees.spect.sum$modalities)

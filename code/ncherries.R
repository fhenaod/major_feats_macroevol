library(apTreeshape)

ncherries=function(trees.spectrums){
  ncherries<-c()
  for (i in 1:length(trees.spectrums)){
    ncherries[i]<-trees.spectrums[[i]][length(trees.spectrums[[i]])]
  }
  return(ncherries)
}
n_cherries<-ncherries(trees_spectrum)

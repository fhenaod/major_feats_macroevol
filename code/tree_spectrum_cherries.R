library(apTreeshape)
## This is for the complete megaphylogenies

# Number of tips from the smaller daughter clade given the node
smaller_spect=function(trees){
  tree.spectrum<-list()
  for (i in 1:length(trees)){
    tree<-e.trees[[i]]
    tree.ts<-as.treeshape(tree)
    tree.spectrum[[i]]<-smaller.clade.spectrum(tree.ts)
  }
  return(tree.spectrum)
}
smaller_spect(e.trees[[1]])

# Number of subtrees of each size 
tree_spectrum=function(trees){
  tree.spectrum<-list()
  for (i in 1:length(trees)){
    tree<-trees[[i]]
    tree.ts<-as.treeshape(tree)
    tree.spectrum[[i]]<-spectrum.treeshape(tree.ts)
  }
  return(tree.spectrum)
}
trees_spectrum<-tree_spectrum(e.trees[1])

ncherries=function(trees.spectrums){
  ncherries<-c()
  for (i in 1:length(trees.spectrums)){
    ncherries[i]<-trees.spectrums[[i]][length(trees.spectrums[[i]])]
  }
  return(ncherries)
}
n_cherries<-ncherries(trees_spectrum)

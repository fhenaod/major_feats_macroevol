library(apTreeshape)

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
smaller_spect(e.trees)

# Number of subtrees of each size 
tree_spectrum=function(trees){
  tree.spectrum<-list()
  for (i in 1:length(trees)){
    tree<-e.trees[[i]]
    tree.ts<-as.treeshape(tree)
    tree.spectrum[[i]]<-spectrum.treeshape(tree.ts)
  }
  return(tree.spectrum)
}
trees.spectrums<-tree_spectrum(e.trees)

plot(spectrum.treeshape(as.treeshape(e.trees[[2]])), type="l")

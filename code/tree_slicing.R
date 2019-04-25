library(castor)
library(ape)

d<-dir("data_megaPhylos/")
t<-grep("tree_",d)
tt<-d[t]
e.trees<-list()
tree_names<-c()

#read trees
for(i in 1:length(tt)){
  typ<- strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][3]
  prefix<-paste(strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][2])
  tree_names[i]<-strsplit(prefix, split=".", fixed=TRUE)[[1]][1]
  tree <- read.tree(paste0("data_megaPhylos/",tt[[i]]))
  e.trees[[i]]<-tree
}

slice_tree_ages<-function(tree, ages2cut){
  cutted.trees<-list()
  for(i in 1:length(ages2cut)){
    cutted.trees[[i]]<-trim_tree_at_height(tree,ages2cut[[i]])$tree 
  }  
  return(cutted.trees)
}

data.frame(tree_names, 
           tips=sapply(e.trees,ape::Ntip),
           trees=c("bird","amphibia","fish", "seed", "chondrichthyes","squamata", "fern", "agaricomycetes"),
           slices=c(22,62,73,65,75,38,95,88))

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[1]])), by=5)
bird.sl<-slice_tree_ages(e.trees[[1]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[2]])), by=5)
amph.sl<-slice_tree_ages(e.trees[[2]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[3]])), by=5)
fish.sl<-slice_tree_ages(e.trees[[3]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[4]])), by=5)
seed.sl<-slice_tree_ages(e.trees[[4]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[5]])), by=5)
chon.sl<-slice_tree_ages(e.trees[[5]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[6]])), by=5)
squa.sl<-slice_tree_ages(e.trees[[6]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[7]])), by=5)
fern.sl<-slice_tree_ages(e.trees[[7]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[8]])), by=5)
agar.sl<-slice_tree_ages(e.trees[[8]],ages2cut[1:length(ages2cut)])

saveRDS(amph.sl,"sliced_trees/amph.sl.rds")
saveRDS(bird.sl,"sliced_trees/bird.sl.rds")
saveRDS(chon.sl,"sliced_trees/chon.sl.rds")
saveRDS(fern.sl,"sliced_trees/fern.sl.rds")
saveRDS(fish.sl,"sliced_trees/fish.sl.rds")
saveRDS(seed.sl,"sliced_trees/seed.sl.rds")
saveRDS(squa.sl,"sliced_trees/squa.sl.rds")
saveRDS(agar.sl,"sliced_trees/agar.sl.rds")

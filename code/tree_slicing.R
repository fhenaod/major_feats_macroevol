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

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[1]])), by=5) # 22
bird.sl<-slice_tree_ages(e.trees[[1]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[2]])), by=5) # 62
amph.sl<-slice_tree_ages(e.trees[[2]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[3]])), by=5) # 73
fish.sl<-slice_tree_ages(e.trees[[3]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[4]])), by=5) # 65
seed.sl<-slice_tree_ages(e.trees[[4]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[5]])), by=5) # 75
chon.sl<-slice_tree_ages(e.trees[[5]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[6]])), by=5) # 38
squa.sl<-slice_tree_ages(e.trees[[6]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[7]])), by=5) # 86
fern.sl<-slice_tree_ages(e.trees[[7]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[8]])), by=5) # 36
mamm.sl<-slice_tree_ages(e.trees[[8]],ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees[[9]])), by=5) # 88
agar.sl<-slice_tree_ages(e.trees[[9]],ages2cut[1:length(ages2cut)])

data.frame(tree_names, 
           tips=sapply(e.trees,ape::Ntip),
           trees=c("bird","amphibia","fish", "seed", 
                   "chondrichthyes","squamata", "fern", 
                   "mammals", "agaricomycetes"),
           slices=c(22, 62, 73, 65, 75, 38, 86, 36, 88))

saveRDS(amph.sl,"Slicing/sliced_trees/amph.sl.rds")
saveRDS(bird.sl,"Slicing/sliced_trees/bird.sl.rds")
saveRDS(chon.sl,"Slicing/sliced_trees/chon.sl.rds")
saveRDS(fern.sl,"Slicing/sliced_trees/fern.sl.rds")
saveRDS(fish.sl,"Slicing/sliced_trees/fish.sl.rds")
saveRDS(seed.sl,"Slicing/sliced_trees/seed.sl.rds")
saveRDS(squa.sl,"Slicing/sliced_trees/squa.sl.rds")
saveRDS(mamm.sl,"Slicing/sliced_trees/mamm.sl.rds")
saveRDS(agar.sl,"Slicing/sliced_trees/agar.sl.rds")

amph.sl<-readRDS("Slicing/sliced_trees/amph.sl.rds")
bird.sl<-readRDS("Slicing/sliced_trees/bird.sl.rds")
chon.sl<-readRDS("Slicing/sliced_trees/chon.sl.rds")
fern.sl<-readRDS("Slicing/sliced_trees/fern.sl.rds")
fish.sl<-readRDS("Slicing/sliced_trees/fish.sl.rds")
seed.sl<-readRDS("Slicing/sliced_trees/seed.sl.rds")
squa.sl<-readRDS("Slicing/sliced_trees/squa.sl.rds")
mamm.sl<-readRDS("Slicing/sliced_trees/mamm.sl.rds")
agar.sl<-readRDS("Slicing/sliced_trees/agar.sl.rds")

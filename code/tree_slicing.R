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
names(e.trees) <- tree_names

slice_tree_ages<-function(tree, ages2cut){
  cutted.trees<-list()
  for(i in 1:length(ages2cut)){
    cutted.trees[[i]]<-trim_tree_at_height(tree,ages2cut[[i]])$tree 
  }  
  return(cutted.trees)
}

ages2cut<-seq(from = 4, to = max(branching.times(e.trees$GBOTBsper)), by = 5) # 65
seed.sl <- slice_tree_ages(e.trees$GBOTBsper, ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$J2012)), by=5) # 22
bird.sl<-slice_tree_ages(e.trees$J2012,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$J2018)), by=5) # 62
amph.sl<-slice_tree_ages(e.trees$J2018,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$R2018)), by=5) # 73
fish.sl<-slice_tree_ages(e.trees$R2018,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$ST2018)), by=5) # 75
chon.sl<-slice_tree_ages(e.trees$ST2018,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$T2016)), by=5) # 38
squa.sl<-slice_tree_ages(e.trees$T2016,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$TE2016)), by=5) # 86
fern.sl<-slice_tree_ages(e.trees$TE2016,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$U2019)), by=5) # 36
mamm.sl<-slice_tree_ages(e.trees$U2019,ages2cut[1:length(ages2cut)])

ages2cut<-seq(from=4, to=max(branching.times(e.trees$V2019)), by=5) # 88
agar.sl<-slice_tree_ages(e.trees$V2019,ages2cut[1:length(ages2cut)])

data.frame(tree_nm = tree_names[-5],
           tips = sapply(e.trees[-5], ape::Ntip),
           age = sapply(e.trees[-5], function(x) max(branching.times(x))),
           slices = c(length(seed.sl), length(bird.sl), length(amph.sl), length(fish.sl),
                    length(chon.sl), length(squa.sl), length(fern.sl),
                    length(mamm.sl), length(agar.sl)) )

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

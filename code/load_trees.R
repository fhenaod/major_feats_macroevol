library(ape)

# MCC raw trees ####
d<-dir("data_megaPhylos/")
t<-grep("tree_",d)
tt<-d[t]
e.trees<-list()
nams_trs<-c()
#read trees
for(i in 1:length(tt)){
  typ<- strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][3]
  prefix<-paste(strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][2])
  nams_trs[i]<-prefix
  tree <- read.tree(paste0("data_megaPhylos/",tt[[i]]))
  e.trees[[i]]<-tree
}
names(e.trees) <- nams_trs

# MCC clean trees ####
J2012 <- read.tree("data_megaPhylos/tree_J2012_.txt")
J2018 <- read.tree("data_megaPhylos/tree_J2018.cr.pr.bi_.txt")
R2018 <- read.tree("data_megaPhylos/tree_R2018.cr_.txt")
#S2018 <- read.tree("data_megaPhylos/tree_S2018.cr.pr.bi_.txt") # Smith & Brown
S2018 <- read.tree("data_megaPhylos/tree_GBOTBsper.cr.bi.pr._.txt") # GBOTB
ST2018 <-read.tree("data_megaPhylos/tree_ST2018.tr.cr_.txt")
T2016 <- read.tree("data_megaPhylos/tree_T2016.cr.pr.bi_.txt")
TE2016<- read.tree("data_megaPhylos/tree_TE2016.cr.pr_.txt")
V2019 <- read.tree("data_megaPhylos/tree_V2019.cr.pr_.txt")
U2019 <- read.tree("data_megaPhylos/tree_U2019.cr.pr_.txt")

emp_clean_trs <- list(J2012, J2018, R2018, S2018, 
                      ST2018, T2016, TE2016, V2019, U2019)
names(emp_clean_trs) <- c("J2012", "J2018", "R2018", "S2018", 
                          "ST2018", "T2016", "TE2016", "V2019", "U2019")
saveRDS(emp_clean_trs, file = "data_megaPhylos/emp_clean_trs.rds")
  
# node named mega-phylogenies ####
bird_nd <- J2012_nd <- readRDS("data_megaPhylos/bird_noded_tre.rds")
amph_nd <- J2018_nd <- readRDS("data_megaPhylos/amph_noded_tre.rds")
fish_nd <- R2018_nd <- readRDS("data_megaPhylos/fish_noded_tre.rds")
seed_nd <- S2018_nd <- readRDS("data_megaPhylos/seed_noded_tre.rds") #GBOT tree
chon_nd <- ST2018_nd<- readRDS("data_megaPhylos/chon_noded_tre.rds")
squa_nd <- T2016_nd <- readRDS("data_megaPhylos/squa_noded_tre.rds")
fern_nd <- TE2016_nd <-readRDS("data_megaPhylos/fern_noded_tre.rds")
mamm_nd <- U2019_nd <- readRDS("data_megaPhylos/mamm_noded_tre.rds")
agar_nd <- V2019_nd <- readRDS("data_megaPhylos/agar_noded_tre.rds")

# rank trees 
path <- c("rank_sampling/")
f_names <- list.files(path = path, pattern = "_fams_trees.rds", recursive = T)

tree_list <- list()
for(i in 2:length(f_names)){
  tree <- readRDS( paste0(path, f_names[i]) )
  tree_list[[i]] <- tree
}
length(tree_list)
sapply(tree_list, length)

# 1K posterior clean trees ####
J2012_cl <- readRDS("data_megaPhylos/bird_erick_clean.rds")
J2012_cl <- readRDS("data_megaPhylos/bird_hack_clean.rds")
J2018_cl <- readRDS("data_megaPhylos/amph_trees_clean.rds")
ST2018_cl<- readRDS("data_megaPhylos/shark_trees_clean.rds")
T2016_cl <- readRDS("data_megaPhylos/squa_trees_clean.rds")
U2019_cl <- readRDS("data_megaPhylos/mamm_trees_clean.rds")

# Henao-Diaz et al. 2019 PNAS trees ####
d <- dir("henaodiaz_etal_trees-master/")
t <- grep("phylo_", d)
tt <- d[t]
henao_d_trs <- list()
nams_trs <- c()
#read trees
for(i in 1:length(tt)){
  typ<- strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][3]
  prefix<-paste(strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][2])
  nams_trs[i]<-prefix
  tree <- read.tree(paste0("henaodiaz_etal_trees-master/",tt[[i]]))
  henao_d_trs[[i]]<-tree
}


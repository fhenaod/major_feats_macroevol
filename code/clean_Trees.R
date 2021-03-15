library(geiger)
library(phytools)
library(rncl)
library(castor)
library(tidyverse)

source.path<-"megaphylos_raw/"
destination.path<-"data_megaPhylos/"

# Remove duplicate species, subspecies or varieties
remove_duplicates=function(phylo, names_list){
  for(i in 1:length(names_list)){
    t_labs<-extract.clade(phy = phylo, node = names_list[i])$tip.label
    t_drop<-t_labs[1:length(t_labs)-1]
    new_tre<-drop.tip(phylo, t_drop)
  }
  return(new_tre)
}

# individual MCC trees ####
J2012<-read.tree(paste0(source.path,"tree_J2012_.txt"))
plot(J2012,cex=.6, show.tip.label = F,no.margin = T)
is.ultrametric(J2012)
is.binary(J2012)
write.tree(J2012,file=paste0(destination.path,"tree_J2012_.txt"))

J2018<-read.tree(paste0(source.path,"tree_J2018_.txt"))
plot(J2018,cex=.6, show.tip.label = F,no.margin = T)
J2018<-phangorn::nnls.tree(cophenetic(J2018),J2018,rooted=TRUE)
is.ultrametric(J2018)
J2018.l<-ladderize(J2018)
repi.tips<-c("Homo_sapiens")
J2018.l<-drop.tip(J2018.l,repi.tips)
J2018.l<-multi2di(J2018.l)
is.binary(J2018.l)
write.tree(J2018.l,file=paste0(destination.path,"tree_J2018.cr.pr.bi_.txt"))

R2018<-read.tree(paste0(source.path,"tree_R2018_.tre"))
plot(R2018,cex=.6, show.tip.label = F,no.margin = T)
R2018<-phangorn::nnls.tree(cophenetic(R2018),R2018,rooted=TRUE)
is.ultrametric(R2018)
write.tree(R2018,file=paste0(destination.path,"tree_R2018.cr_.txt"))

S2018<-read.tree(paste0(source.path,"tree_S2018_.tre"))
S2018_spp<-unique(S2018$node.label)[grep("_",unique(S2018$node.label))]
S2018<-remove_duplicates(S2018, S2018_spp) ## runned in cluster
S2018<-read.tree(paste0(source.path,"tree_S2018.pr._.txt")) 
S2018<-drop.tip(S2018, S2018$tip.label[grep(c("Phyllites"), S2018$tip.label)]) # drop fossil species
S2018<-drop.tip(S2018, S2018$tip.label[grep(c("Baiera"), S2018$tip.label)]) # drop fossil species
S2018<-drop.tip(S2018, S2018$tip.label[grep(c("Pterophyllus_gingko"), S2018$tip.label)]) # drop repeated ginkgo
#S2018$tip.label <- gsub("_", " ", S2018$tip.label)
S2018<-drop.tip(S2018, c(grep("Homaliopsis", S2018$tip.label),
                         grep("Neuroloma", S2018$tip.label),
                         grep("Stroemia", S2018$tip.label), 
                         grep("Elharveya", S2018$tip.label) ))

S2018 <- drop.tip(S2018, grep("[[:digit:]1234567890]", S2018$tip.label)) # tips with colection or reference numbers; most of them infra spp level
S2018 <- drop.tip(S2018, grep("_var._", S2018$tip.label) ) # revome var.
S2018 <- drop.tip(S2018, grep("_aff._", S2018$tip.label) ) # revome aff.
S2018 <- drop.tip(S2018, grep("_cv._", S2018$tip.label)) # revome cv.
S2018 <- drop.tip(S2018, grep("_sp._[[:upper:]]", S2018$tip.label)) # remove unknown sp. 
S2018 <- drop.tip(S2018, grep("_cf._", S2018$tip.label)) # remove cf.
S2018 <- drop.tip(S2018, grep("_subsp._", S2018$tip.label)) # remove subsp.
S2018 <- drop.tip(S2018, grep("_f._", S2018$tip.label)) # remove f.
S2018 <- drop.tip(S2018, grep("_v\\.", S2018$tip.label)) # remove v.
S2018 <- drop.tip(S2018, grep("_ined._", S2018$tip.label)) # remove ined.
S2018 <- drop.tip(S2018, grep("_nov._", S2018$tip.label)) # remove ined.
S2018 <- drop.tip(S2018, grep("\\.__[[:upper:]]", S2018$tip.label)) # remove unknown sp
S2018 <- drop.tip(S2018, grep("haplotype", S2018$tip.label)) # remove 
S2018 <- drop.tip(S2018, grep("fossil", S2018$tip.label)) # remove fossil tips
S2018 <- drop.tip(S2018, grep("^x", S2018$tip.label)) # remove hybrids
S2018 <- drop.tip(S2018, grep("×", S2018$tip.label)) # remove hybrids special charact
S2018 <- drop.tip(S2018, grep("Goodyerinae", S2018$tip.label)) # remove subtribe
S2018 <- drop.tip(S2018, grep("Hermogenodendron", S2018$tip.label)) #
plot(S2018, cex=.6, show.tip.label = F, no.margin = T)
S2018<-multi2di(S2018)
is.binary(S2018)
min(S2018$edge.length)
S2018$edge.length[S2018$edge.length<=0] <-1e-6
S2018<-castor:::extend_tree_to_height(S2018)$tree
is.ultrametric(S2018)
write.tree(S2018,file=paste0(destination.path,"tree_S2018.cr.pr.bi_.txt"))

GBOTB_sper <- extract.clade(V.PhyloMaker::GBOTB.extended, "Spermatophyta") 
GBOTB_sper <- drop.tip(GBOTB_sper, grep("[[:digit:]1234567890]", GBOTB_sper$tip.label)) # tips with colection or reference numbers; most of them infra spp level
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_var._", GBOTB_sper$tip.label) ) # revome var.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_aff._", GBOTB_sper$tip.label) ) # revome aff.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_cv._", GBOTB_sper$tip.label)) # revome cv.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_sp._[[:upper:]]", GBOTB_sper$tip.label)) # remove unknown sp. 
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_ssp._", GBOTB_sper$tip.label)) # remove subsp. 
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_cf._", GBOTB_sper$tip.label)) # remove cf.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_subsp._", GBOTB_sper$tip.label)) # remove subsp.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_f._", GBOTB_sper$tip.label)) # remove f.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_v\\.", GBOTB_sper$tip.label)) # remove v.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_ined._", GBOTB_sper$tip.label)) # remove ined.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_nov._", GBOTB_sper$tip.label)) # remove ined.
GBOTB_sper <- drop.tip(GBOTB_sper, grep("_X_", GBOTB_sper$tip.label)) # remove X hybrids
GBOTB_sper <- drop.tip(GBOTB_sper, grep("\\.__[[:upper:]]", GBOTB_sper$tip.label)) # remove unknown sp
GBOTB_sper <- drop.tip(GBOTB_sper, grep("[:.:]", GBOTB_sper$tip.label)) # remove unknown sp.
is.binary(GBOTB_sper)
GBOTB_sper<-multi2di(GBOTB_sper)
is.ultrametric(GBOTB_sper)
min(GBOTB_sper$edge.length)
write.tree(GBOTB_sper,file=paste0(destination.path,"tree_GBOTBsper.bi.pr._.txt"))

ST2018<-read.nexus(paste0(source.path,"tree_ST2018_.nex"))
plot(ST2018,cex=.6, show.tip.label = F,no.margin = T)
is.binary(ST2018)
min(ST2018$edge.length)
ST2018$edge.length[ST2018$edge.length<=0] <-1e-6
ST2018<-phangorn::nnls.tree(cophenetic(ST2018),ST2018,rooted=TRUE)
is.ultrametric(ST2018)
ST2018$edge.length[ST2018$edge.length<=0] <-1e-6
write.tree(ST2018,file=paste0(destination.path,"tree_ST2018.tr.cr_.txt"))

T2016<-read.tree(paste0(source.path,"tree_T2016_.tre"))
plot(T2016,cex=.6, show.tip.label = F,no.margin = T)
repi.tips<-c("Sphenodon_punctatus")
T2016<-drop.tip(T2016,repi.tips)
T2016<-multi2di(T2016)
is.binary(T2016)
T2016<-castor:::extend_tree_to_height(T2016)$tree
is.ultrametric(T2016)
min(T2016$edge.length)
T2016$edge.length[T2016$edge.length==0] <-1e-6
T2016<-castor:::extend_tree_to_height(T2016)$tree
is.ultrametric(T2016)
write.tree(T2016,file=paste0(destination.path,"tree_T2016.cr.pr.bi_.txt"))

TE2016<-read.nexus(paste0(source.path,"tree_TE2016_.nex"))
plot(TE2016,cex=.6, show.tip.label = F,no.margin = T)
nodelabels(frame="none",cex=.8, col = "red")
TE2016<-extract.clade(TE2016, 4038)
TE2016<-phangorn::nnls.tree(cophenetic(TE2016),TE2016,rooted=TRUE)
is.ultrametric(TE2016)
write.tree(TE2016,file=paste0(destination.path,"tree_TE2016.cr.pr_.txt"))

U2019 <- read.nexus(paste0(source.path, "tree_U2019_.nex"))
plot(U2019, cex=.6, show.tip.label = F, no.margin = T)
U2019 <- drop.tip(U2019, "_Anolis_carolinensis")
is.binary(U2019)
U2019 <- phangorn::nnls.tree(cophenetic(U2019), U2019, rooted = TRUE)
is.ultrametric(U2019)
write.tree(U2019, file = 
             paste0(destination.path,"tree_U2019.cr.pr_.txt"))

V2019<-read.tree(paste0(source.path,"tree_V2019_.tree"))
plot(T2016,cex=.6, show.tip.label = F,no.margin = T)
V2019<-drop.tip(V2019, c(grep("Bullera", V2019$tip.label), grep("Cryptococcus", V2019$tip.label),
                         grep("Dacrymyces", V2019$tip.label), grep("Laeticorticium", V2019$tip.label),
                         grep("Trichosporon", V2019$tip.label), grep("Trimorphomyces", V2019$tip.label),
                         grep("Tsuchiyaea", V2019$tip.label), grep("Udeniomyces", V2019$tip.label) ))
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_sp$", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_sp_", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_sp[[:digit:]]", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_[[:digit:]1234567890]_", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_sp._", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_sp.$", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_sp.[[:digit:]1234567890]", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_var._", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_aff._", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_cf._", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("_for_", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("^cyphelloid", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("^Corneriellasp.Dennisiomyces", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("^DeconiCA", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("^FlAGelloscypha", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("^LepioTA", V2019$tip.label)])
V2019<-drop.tip(V2019,  V2019$tip.label[grep("^Pacychytospora", V2019$tip.label)])
V2019$tip.label[grep("^[[:digit:]1234567890]", V2019$tip.label)]<- 
  paste0(sapply(strsplit(V2019$tip.label[grep("^[[:digit:]1234567890]", V2019$tip.label) ], "_"), '[', 3),
         "_",
         sapply(strsplit(V2019$tip.label[grep("^[[:digit:]1234567890]", V2019$tip.label) ], "_"), '[', 4))
V2019<-phangorn::nnls.tree(cophenetic(V2019),V2019,rooted=TRUE)
is.ultrametric(V2019)
write.tree(V2019,file=paste0(destination.path,"tree_V2019.cr.pr_.txt"))

# 1k posterior trees ####
d <- dir("1k_trees/")

# amphibia
trees <- read.nexus("1k_trees/amphibia_1k_trees/output.nex")

t2t <- lapply(trees, function(x) drop.tip(x, "Homo_sapiens"))
sapply(t2t, is.rooted) %>% table()
sapply(t2t, is.binary) %>% table()
sapply(t2t, is.ultrametric) %>% table()

for(i in 1:length(t2t)){
  t2t[[i]] <- phangorn::nnls.tree(cophenetic(t2t[[i]]), 
                                    t2t[[i]], rooted = TRUE)
}
sapply(t2t, is.ultrametric) %>% table()
saveRDS(t2t, file = "data_megaPhylos/amph_trees_clean.rds")

# mammals
trees <- read.nexus("1k_trees/mammal_1k_trees/output.nex")

t2t <- lapply(trees, function(x) drop.tip(x, "_Anolis_carolinensis"))
sapply(t2t, is.rooted) %>% table()
sapply(t2t, is.binary) %>% table()
sapply(t2t, is.ultrametric) %>% table()

for(i in 1:length(t2t)){
  t2t[[i]] <- phangorn::nnls.tree(cophenetic(t2t[[i]]), 
                                  t2t[[i]], rooted = TRUE)
}
sapply(t2t, is.ultrametric) %>% table()
saveRDS(t2t, file = "data_megaPhylos/mamm_trees_clean.rds")

# sharks
trees <- read.nexus("1k_trees/sharks_1k_trees/output.nex")

t2t <- lapply(trees, function(x) drop.tip(x, ""))
sapply(t2t, is.rooted) %>% table()
sapply(t2t, is.binary) %>% table()
sapply(t2t, is.ultrametric) %>% table()

for(i in 1:length(t2t)){
  t2t[[i]] <- phangorn::nnls.tree(cophenetic(t2t[[i]]), 
                                  t2t[[i]], rooted = TRUE)
}
sapply(t2t, is.ultrametric) %>% table()
saveRDS(t2t, file = "data_megaPhylos/shark_trees_clean.rds")

# bird erick
trees <- read.nexus("1k_trees/bird_erick_1k_trees/output.nex")

t2t <- lapply(trees, function(x) drop.tip(x, ""))
sapply(t2t, is.rooted) %>% table()
sapply(t2t, is.binary) %>% table()
sapply(t2t, is.ultrametric) %>% table()

saveRDS(t2t, file = "data_megaPhylos/bird_erick_clean.rds")

# bird hack
trees <- read.nexus("1k_trees/bird_hacke_1k_trees/output.nex")

t2t <- lapply(trees, function(x) drop.tip(x, ""))
sapply(t2t, is.rooted) %>% table()
sapply(t2t, is.binary) %>% table()
sapply(t2t, is.ultrametric) %>% table()

saveRDS(t2t, file = "data_megaPhylos/bird_hack_clean.rds")

# squamates
trees <- read.nexus("1k_trees/squa_1k_trees/output.nex")

t2t <- lapply(trees, function(x) drop.tip(x, "Sphenodon_punctatus"))
sapply(t2t, is.rooted) %>% table()
sapply(t2t, is.binary) %>% table()
sapply(t2t, is.ultrametric) %>% table()

for(i in 1:length(t2t)){
  t2t[[i]] <- castor:::extend_tree_to_height(t2t[[i]])$tree
}

sapply(t2t, is.ultrametric) %>% table()
saveRDS(t2t, file = "data_megaPhylos/squa_trees_clean.rds")

# fungi

tip2rem <- lapply(c("Bullera", "Cryptococcus", "Dacrymyces", 
                    "Laeticorticium", "Trichosporon", "Trimorphomyces", 
                    "Tsuchiyaea", "Udeniomyces"), 
                  function(x) grep(x, V2019$tip.label)) %>% unlist()


library(geiger)
library(phytools)
library(rncl)
library(castor)

source.path<-"megaphylos_raw/"
destination.path<-"data_megaPhylos/"

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
S2018<-drop.tip(S2018, S2018$tip.label[grep("Phyllites", S2018$tip.label)]) # drop fossil species
S2018_spp<-unique(S2018$node.label)[grep("_",unique(S2018$node.label))]

plot(S2018,cex=.6, show.tip.label = F,no.margin = T)
S2018<-castor:::extend_tree_to_height(S2018)$tree
is.ultrametric(S2018)
S2018<-multi2di(S2018)
is.binary(S2018)
write.tree(S2018,file=paste0(destination.path,"tree_S2018.cr.bi_.txt"))

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
plot(T2016,cex=.6, show.tip.label = F,no.margin = T)
TE2016<-phangorn::nnls.tree(cophenetic(TE2016),TE2016,rooted=TRUE)
is.ultrametric(TE2016)
write.tree(TE2016,file=paste0(destination.path,"tree_TE2016.cr_.txt"))

V2019<-read.tree(paste0(source.path,"tree_V2019_.tree"))
plot(T2016,cex=.6, show.tip.label = F,no.margin = T)
V2019<-phangorn::nnls.tree(cophenetic(V2019),V2019,rooted=TRUE)
is.ultrametric(V2019)
write.tree(V2019,file=paste0(destination.path,"tree_V2019.cr_.txt"))

library(phytools)
library(parallel)
library(taxize)

# get genus names ####
names_sp<-strsplit(tree$tip.label, "_")
get_genus=function(names_sp){
  query<-c()  
  for(i in 1:length(names_sp)){
    query[i]<-names_sp[[i]][1]
}
return(query)
}
genera<-unique(get_genus(names_sp))

# Name genera nodes ####
name_genus_nodes=function(tree,names2nodes){
  for(i in 1:length(names2nodes)){
    tips<-tree$tip.label[grep(names2nodes[i], tree$tip.label)]
    if(length(tips)!=1){
      mrca_node<-getMRCA(tree,tip = tips)
      tree$node.label[mrca_node-Ntip(tree)]<-names2nodes[i]
    } else {
      
    }
}
return(tree)
}
tre_g_noded<-name_genus_nodes(tree, genera)
## Check genera noding ####
table(tree$node.label)
table(tre_noded$node.label)

plot(extract.clade(tre_noded, node = "Neoharriotta"), show.node.label = T, no.margin = TRUE, 
     edge.width = 2, cex = 0.8)
nodelabels(frame = "none", col = "red", adj = c(1.5,-0.7), cex = 0.7)

# get taxonomy based on genus names ####
get_taxonomy_df=function(genera, database){
  m<-matrix(data=NA, nrow = length(genera), ncol = 6)
  colnames(m)<-c("kingdom", "phylum", "class", "order", "family", "genus")
  for(i in 1:length(genera)){
    cla<-classification(genera[i], db = database)[[1]][1:6,1]
    m[i,]<-cla
  }
  m<-data.frame(m)
  return(m)
}
m<-get_taxonomy_df(genera, database = 'gbif')
summary(m)
write.csv(m,"taxonomy/chon_tax.csv")

# Name family nodes ####
m<-read.csv("taxonomy/chon_tax.csv", header = T, row.names = 1)
name_family_nodes=function(tree, m){
  fams<-unique(m$family)
  for(i in 1:length(fams)){
    fam_g<-as.character(m[grep(fams[i],m$family),]$genus)
    mrca_node<-get_mrca_of_set(tree, fam_g)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(fams[i])
      } 
  }
  return(tree)
}
tre_gf_noded<-name_family_nodes(tre_g_noded, m)
## Check family noding ####
tree$node.label[grep("Rhinochimaeridae", tree$node.label)]
tre_gf_noded$node.label[grep("Rhinochimaeridae", tre_gf_noded$node.label)]

extract.clade(tre_gf_noded, node = "Rhinochimaeridae")
plot(extract.clade(tre_noded, node = "Rhinochimaeridae"), show.node.label = T, no.margin = TRUE, 
     edge.width = 2, cex = 0.8)
table(tre_gf_noded$node.label[grep("idae", tre_gf_noded$node.label)])


# Name order nodes

#

# Subseting #####
orders<-unique(seed_tre_new$node.label)[grep("ales",unique(seed_tre_new$node.label))] # orders
orders<-orders[-13]
families<-unique(seed_tre_new$node.label)[grep("ceae",unique(seed_tre_new$node.label))] # families
families<-families[-9]

seed_ord<-mclapply(orders, function(x) extract.clade(phy = seed_tre_new, node = x), mc.cores = 2)
saveRDS(seed_ords, "node_subtrees/seed_ord_trees.rds")

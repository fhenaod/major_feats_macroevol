library(phytools)
library(tidyverse)
library(taxize)
library(castor)

tree <- TE2016 # select the tree  
clad <- "fern"
  
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
tree$tip.label <- gsub("_", " ", tree$tip.label)

# Name genera nodes, for non-monospecific genus ####
name_genus_nodes=function(tree, names2nodes){
  for(i in 1:length(names2nodes)){
    tips<-tree$tip.label[grep(paste0("\\b", names2nodes[i] ,"\\b"), tree$tip.label)]
    if(length(tips)!=1){
      mrca_node<-getMRCA(tree, tip = tips)
      tree$node.label[mrca_node-Ntip(tree)]<-names2nodes[i]
    } else {
      
    }
  }
  return(tree)
}
tre_g_noded<-name_genus_nodes(tree, genera)

# Name higher rank nodes on a genus-tip-tree, based on taxonomy table ####
m <- read.csv(paste0("taxonomy/", clad,"_tax.csv"), header = T, row.names = 1)
name_htaxa_nod_gen_tip=function(tree, m){
  fams<-unique(m$family)
  for(i in 1:length(fams)){
    fam_g<-as.character(m[grep(fams[i], m$family),]$genus)
    if(length(fam_g)<2) {next()}
    mrca_node<-get_mrca_of_set(tree, fam_g)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(fams[i])
    } 
  }
  
  ords<-unique(m$order)
  for(j in 1:length(ords)){
    ord_f<-as.character(m[grep(ords[j],m$order),]$family)
    mrca_node<-get_mrca_of_set(tree, ord_f)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(ords[j])
    } 
  }
  
  clas<-unique(m$class)
  for(k in 1:length(ords)){
    cla_o<-as.character(m[grep(clas[k],m$class),]$order)
    mrca_node<-get_mrca_of_set(tree, cla_o)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(clas[k])
    } 
  }
  
  return(tree)
}
tre_noded <- name_htaxa_nod_gen_tip(tre_g_noded, m)

# Name higher rank nodes based on taxonomy table ####
m <- read.csv(paste0("taxonomy/", clad,"_tax.csv"), header = T, row.names = 1)
name_htaxa_nodes=function(tree, m){
  tr_nam_tab <- data.frame(spp = tree$tip.label, 
                           genus = get_genus(names_sp)) %>% 
    left_join(m, by = c("genus"="genus"))
  
  fams<-unique(tr_nam_tab$family)
  for(i in 1:length(fams)){
    fam_t <- filter(tr_nam_tab, family == fams[i]) %>% pull(spp)
    if(length(fam_t)==1){next()}
    mrca_node<-get_mrca_of_set(tree, fam_t)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(fams[i])
    } 
  }
  
  ords<-unique(tr_nam_tab$order)
  for(j in 1:length(ords)){
    ord_t<-filter(tr_nam_tab, order == ords[j]) %>% pull(spp)
    if(length(ord_t)==1){next()}
    mrca_node<-get_mrca_of_set(tree, ord_t)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(ords[j])
    } 
  }
  
  clas<-unique(tr_nam_tab$class)
  for(k in 1:length(ords)){
    cla_t<-filter(tr_nam_tab, class == clas[k]) %>% pull(spp)
    mrca_node<-get_mrca_of_set(tree, cla_t)
    if(mrca_node!=1) {
      tree$node.label[mrca_node-Ntip(tree)]<-as.character(clas[k])
    } 
  }
  
  return(tree)
}
tre_noded <- name_htaxa_nodes(tre_g_noded, m)

plot(tre_noded, show.tip.label = F, no.margin = T, type = "fan")
nodelabels(tre_noded$node.label, frame = "none", col = "red", cex = .65)

saveRDS(tre_noded, paste0("data_megaPhylos/", clad,"_noded_tre.rds"))


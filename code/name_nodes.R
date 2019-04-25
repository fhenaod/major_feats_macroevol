library(phytools)
library(parallel)
library(taxize)
library(castor)

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

# Name higher rank nodes based on taxonomy table ####
m<-read.csv("taxonomy/chon_tax.csv", header = T, row.names = 1)
name_htaxa_nodes=function(tree, m){
  fams<-unique(m$family)
  for(i in 1:length(fams)){
    fam_g<-as.character(m[grep(fams[i],m$family),]$genus)
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
tre_noded<-name_htaxa_nodes(tre_g_noded, m)
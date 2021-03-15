library(rotl)
library(taxonlookup)
library(taxize)
library(dplyr)

tree <- S2018 # select the tree 

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
#genera<-sort(genera)[33:length(genera)] ## with Fungi

# get taxonomy based on genus names, automatic ####
query_seq<-seq_along(genera)
n_queries<-200
query_split<-split(genera, ceiling(query_seq/n_queries))

tnrs_contexts() ## Check available contexts
get_tax_df=function(query_split, context_name){
  m<-lapply(1:length(query_split), matrix, data=NA, nrow = n_queries, ncol = 3)
  for (k in 1:length(query_split)){
    query_match<-tnrs_match_names(query_split[[k]], context_name = context_name)
    query_pull<-tax_lineage(taxonomy_taxon_info(ott_id(query_match), include_lineage = TRUE))
    
    for (i in 1:length(query_pull)){
      m[[k]][i,1]<-query_pull[[i]][query_pull[[i]]$rank == "family",][1,2]
      m[[k]][i,2]<-query_pull[[i]][query_pull[[i]]$rank == "order",][1,2]
      m[[k]][i,3]<-query_pull[[i]][query_pull[[i]]$rank == "class",][1,2]
    }
  }
  df<-data.frame(do.call(rbind,m)[1:length(genera),])
  colnames(df)<-c("family",  "order", "class")
  df$genus<-genera
  df<-df[,c(4,1,2,3)]
  return(df)
}
tree_taxonomy<-get_tax_df(query_split, context_name = "Seed plants")
write.csv(tree_taxonomy,"taxonomy/mamm_tax.csv")

# get taxonomy based on genus names, interactive ####
get_taxonomy_df=function(genera, database, nrows){
  m<-matrix(data=NA, nrow = length(genera), ncol = 6)
  colnames(m)<-c("kingdom", "phylum", "class", "order", "family", "genus")
  for(i in 1:length(genera)){
    cla<-classification(genera[i], db = database, rows = nrows)[[1]][1:6,1]
    m[i,]<-cla
  }
  m<-data.frame(m)
  return(m)
}
m<-get_taxonomy_df(sort(genera), database = 'gbif', nrows = NA)
summary(m)
write.csv(m,"taxonomy/agar_tax.csv")

## Seed Plants
#look_tab<-lookup_table(genera)
#tax<-get_taxonomy_df(unique(look_tab$genus)[-5], database = 'gbif')
tax<-get_taxonomy_df(sort(genera), database = 'gbif')
tax<-tax[,1:4]
tax_unif<-left_join(look_tab, tax, by = "order")
tax_unif<-tax_unif[,-4]
tax_unif<-tax_unif[,c(1,2,3,6,5,4)]
write.csv(tax_unif,"taxonomy/seed_tax.csv")

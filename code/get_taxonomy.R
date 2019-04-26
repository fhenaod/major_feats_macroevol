library(rotl)
library(taxonlookup)

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
genera<-sort(genera)[33:length(genera)] ## with Fungi

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
tree_taxonomy<-get_tax_df(query_split, context_name = "Ferns")
write.csv(tree_taxonomy,"taxonomy/fern_tax.csv")

## Seed Plants
look_tab<-lookup_table(genera)
tax<-get_taxonomy_df(unique(look_tab$order)[-6], database = 'gbif')

tax[order(tax$class),]
write.csv(look_tab,"taxonomy/seed_tax.csv")

# get taxonomy based on genus names, interactive ####
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
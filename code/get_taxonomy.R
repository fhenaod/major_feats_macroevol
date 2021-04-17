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

# seed plants, tempral taxonomy refined the empty or error automatic asignements
temp_tax <- read.csv("taxonomy/seed_tax_temp.csv", row.names = F)
head(temp_tax)
genera <- temp_tax %>% filter(!is.na(genus)) %>% filter(is.na(family)) %>% pull(genus)

# get taxonomy based on genus names, interactive or automatic ####
get_taxonomy_df=function(genera, database, nrows){
  m<-matrix(data=NA, nrow = length(genera), ncol = 6)
  colnames(m)<-c("kingdom", "phylum", "class", "order", "family", "genus")
  for(i in 1:length(genera)){
    cla<-try(classification(genera[i], db = database, rows = nrows))
    if(is.na(cla[[1]])){m[i,]<-c(rep(NA, 5), genera[i])} else {m[i,]<-cla[[1]][1:6,1]}
  }
  return(data.frame(m))
}
sw <- get_taxonomy_df(sort(genera), database = 'gbif', nrows = NA)
summary(m)
write.csv(m,"taxonomy/seed_tax.csv", row.names = F)


# get taxonomy based on genus names, automatic ####
# works fine with all but plants
query_seq<-seq_along(genera)
n_queries<-200
query_split<-split(genera, ceiling(query_seq/n_queries))

tnrs_contexts() ## Check available contexts
get_tax_df=function(query_split, context_name){
  m<-lapply(1:length(query_split), matrix, data = NA, nrow = n_queries, ncol = 3)
  for (k in 1:length(query_split)){
    query_match<-(tnrs_match_names(query_split[[k]], 
                                  context_name = context_name, 
                                  do_approximate_matching = F))
    query_pull<-tax_lineage(taxonomy_taxon_info(ott_id(query_match), 
                                                include_lineage = TRUE))
    
    for (i in 1:length(query_pull)){
      m[[k]][i,1]<-query_pull[[i]][query_pull[[i]]$rank == "family",][1,2]
      m[[k]][i,2]<-query_pull[[i]][query_pull[[i]]$rank == "order",][1,2]
      m[[k]][i,3]<-query_pull[[i]][query_pull[[i]]$rank == "class",][1,2]
    }
  }
  df<-data.frame(do.call(rbind,m)[1:length(unlist(query_split)),])
  colnames(df)<-c("family",  "order", "class")
  df$genus<-genera
  df<-df[,c(4,1,2,3)]
  return(df)
}
tree_taxonomy<-get_tax_df(query_split, context_name = "Seed plants")
write.csv(tree_taxonomy,"taxonomy/mamm_tax.csv", row.names = F)


# load all taxonomies on a list
clads <- c("agar", "amph", "bird", "chon", 
           "fern",  "fish", "mamm",
           #seed, 
           "squa")
tax_lst <- list()
for(i in 1:length(clads)){
  tax_lst[[i]] <- read.csv(paste0("taxonomy/",clads[i],"_tax.csv"))
}
names(tax_lst) <- clads

# transforms a 4 col taxonomic table to a 2 col table
tax_trf=function(temp_tax){
  temp_tax %>% dplyr::select(genus, family, order, class) %>% 
    pivot_longer(everything(), names_to = "rank", values_to = "taxon") %>% 
    distinct(taxon, .keep_all = T) %>% arrange(rank, taxon)
}

lapply(tax_lst, tax_trf)

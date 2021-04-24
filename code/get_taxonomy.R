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

# get taxonomy based on genus names ####
# nrows = NA (interactive) | nrows = 1 (automatic)
get_taxonomy_df=function(genera, database, nrows){
  m<-matrix(data=NA, nrow = length(genera), ncol = 6)
  colnames(m)<-c("kingdom", "phylum", "class", "order", "family", "genus")
  for(i in 1:length(genera)){
    cla<-try(classification(genera[i], db = database, rows = nrows))
    if(is.na(cla[[1]])){m[i,]<-c(rep(NA, 5), genera[i])} else {m[i,]<-cla[[1]][1:6,1]}
  }
  return(data.frame(m))
}
sw <- get_taxonomy_df(sort(genera), database = 'gbif', nrows = 1)
write.csv(sw,"taxonomy/mamm_tax.csv", row.names = F)

#sw %>% write.csv("taxonomy/seed_tax_raw.csv", row.names = F) # original database

sw %>% mutate(genus = coalesce(genus, family, order, class, phylum)) %>%
  distinct(genus, .keep_all = T) %>% 
  write.csv("taxonomy/seed_tax_raw2.csv", row.names = F)

# get taxonomy based on genus names, automatic ####
# works fine with all but plants
query_seq<-seq_along(genera)
n_queries<-200
query_split<-split(genera, ceiling(query_seq/n_queries))

tnrs_contexts() ## Check available contexts
get_tax_df=function(query_split, context_name){
  m<-lapply(1:length(query_split), matrix, data = NA, nrow = n_queries, ncol = 5)
  for (k in 1:length(query_split)){
    query_match<-(tnrs_match_names(query_split[[k]], 
                                  context_name = context_name, 
                                  do_approximate_matching = T))
    
    query_pull <- tax_lineage(taxonomy_taxon_info(ott_id(query_match), 
                                                  include_lineage = TRUE))
    
    for(i in 1:length(query_pull)){
      m[[k]][i,1]<-query_pull[[i]][query_pull[[i]]$rank == "family",][1,2]
      m[[k]][i,2]<-query_pull[[i]][query_pull[[i]]$rank == "order",][1,2]
      m[[k]][i,3]<-query_pull[[i]][query_pull[[i]]$rank == "class",][1,2]
      m[[k]][i,4]<-query_pull[[i]][query_pull[[i]]$rank == "phylum",][1,2]
      m[[k]][,5]<-query_match$search_string
    }
    #m[[k]] <- cbind(m[[k]], gens_t) 
  }
  df<-data.frame(do.call(rbind, m)[1:length(unlist(query_split)),])
  colnames(df)<-c("family",  "order", "class", "phylum", "genus")
  df<-df[,c(5,1,2,3,4)]
  return(df)
}
tree_taxonomy<-get_tax_df(query_split, context_name = "Seed plants")
write.csv(tree_taxonomy,"taxonomy/seed_tax.csv", row.names = F)

# seed plants more clean approach
# extracts taxonomy restricting search by "Seed plants" context
gen2rem <- c("Botryophora", "Fourneaua", "Govania", "Saragodra", "Erythroxylon", "Gagernia", "Carusia", "Spondioides", 
             "Pleiosyngyne", "Eleutherocarpum", "Guatemala", "Hoogenia", "Polypetalia", "Prunus-lauro", "Achymus", "Nangha", 
             "Duretia", "Elkania", "Tramoia", "Jundzillia", "Wilckia", "Syntrophe", "Tetracocyne", "Triberta", "Knorrea", 
             "Mesonephelium", "Bucco", "Calophylloides", "Leantria", "Disporocarpa", "Cremsonella", "Sauria", "Berendtia", 
             "Berendtiella", "Acanthea", "Oroxylon", "Gisania", "Bradshawia", "Cymbochasma", "Kraschnikowia", "Origanon", 
             "Orlowia", "Oxyotis", "Tectonia", "Teucrion", "Thymos", "Neofranciella", "Calisaya", "Dicrus", "Murucoa", 
             "Adenimesa", "Eratica", "Jacobaeastrum", "Oxyphoeria", "Ballela", "Cyphium", "Descliaea", "Chondylophyllum", 
             "Mormoraphis", "Shirleyopanax", "Unjala", "Diplapsis", "Mniothamus", "Brachycheila", "Butonicoides", 
             "Arnoldoschultzea", "Peronia", "Aporia", "Cyclaminus", "Chupalon", "Sarcogonum", "Cistomorpha", "Cryptochaete", 
             "Fibra", "Geanthia", "Banglium", "Ethanium", "Gethyra", "Acophorum", "Helicotrichum", "Toxeumia", "Anomaza", "Hecaste", 
             "Isis", "Limniris", "Neuberia", "Tekelia", "Wredowia", "Xyphidium", "Callithamna",
             "Crinodonna", "Pancratio-crinum", "Sudacaste", "Orestia", "Phlebochilus", "Trichorhiza", "Chlamysporum", 
             "Gasteronema", "Maliga", "Uvulana", "Enalus", "Saivala", "Spermabolus", "Methysticum", "Quadrifaria", "Cembra")

gen2rem <- c("Myrovernix", "Gongyloglossa", "Myrrhoides", "Devendraea", "Berendtiella")
  
new_gens2tax <- genera[-match(gen2rem, genera)]

query_match <- tnrs_match_names(new_gens2tax, 
                                context_name = "Seed plants", 
                                do_approximate_matching = T)

query_pull <- tax_lineage(taxonomy_taxon_info(ott_id(query_match), 
                                              include_lineage = TRUE))

m <- matrix(data = NA, nrow = length(new_gens2tax), ncol = 5)
for(i in 1:length(query_pull)){
  m[i,1]<-query_pull[[i]][query_pull[[i]]$rank == "family",][1,2]
  m[i,2]<-query_pull[[i]][query_pull[[i]]$rank == "order",][1,2]
  m[i,3]<-query_pull[[i]][query_pull[[i]]$rank == "class",][1,2]
  m[i,4]<-query_pull[[i]][query_pull[[i]]$rank == "phylum",][1,2]
  m[,5] <-query_match$search_string
}
colnames(m) <- c("family",  "order", "class", "phylum", "genus")
m <- data.frame(m[,c(5,1,2,3,4)])
m %>% mutate(genus = stringr::str_to_title(genus)) %>% 
  mutate(class = replace(class, 
                               order == "Araucariales" | order == "Cupressales" |
                                 order == "Pinales", "Pinopsida")) %>% 
  mutate(class = replace(class, 
                         order == "Cycadales", "Cycadopsida")) %>% 
  mutate(class = replace(class, 
                         order == "Ephedrales" | order == "Gnetales"| 
                           order == "Welwitschiales" , "Gnetopsida")) %>%
  mutate(class = replace(class, 
                         order == "Ginkgoales" , "Ginkgopsida")) %>%
  mutate(order = replace(order, 
                         family == "Vahliaceae", "incertae sedis")) %>%
  mutate(family = replace(family, 
                          genus == "Mazus", "Phrymaceae")) %>%
  mutate(family = replace(family, 
                          genus == "Dodartia", "Orobanchaceae")) %>% 
  write.csv("taxonomy/seed_tax.csv", row.names = F)

# seed plants extra code merging databases
# partial taxonomy to be refined: empty or error in automatic asignements
seed_raw_tax <- read.csv("taxonomy/seed_tax_raw2.csv")
genus2tax <- seed_raw_tax %>% filter(kingdom != "Plantae") %>% pull(genus)

gen2rem <- c("Synhimantus", "Aporia", "Taeniodera", "Cymatoderma", "Berendtia", "Bucco", 
             "Euproctus", "Cerrena", "Bemella", "Carusia", "Cheilopogon", "Chilasa", "Vitrina", 
             "Athoracophorus", "Dalaca", "Hinoa", "Helicotrichum", "Isis", "Kochichthys", 
             "Leccinum", "Canis", "Macowanites", "Madera", "Umbra", "Orestia", "Dasyatis", 
             "Peronia", "Gastropacha", "Paramphithoe", "Fusicoccum", "Quiscalus", "Synuchus", 
             "Cryptobranchus", "Steganomus", "Omalaspis", "Oxoplecia", "Auchmis", "Cicindela", 
             "Ascidia", "Polygyra", "Ergastria", "Sichuaniella", "Polypedilum")

new_gens2tax <- genus2tax[-match(gen2rem, genus2tax)]

query_match <- tnrs_match_names(new_gens2tax, 
                                 context_name = "Seed plants", 
                                 do_approximate_matching = T)
  
query_pull <- tax_lineage(taxonomy_taxon_info(ott_id(query_match), 
                                                include_lineage = TRUE))

m <- matrix(data = NA, nrow = length(new_gens2tax), ncol = 5)
for(i in 1:length(query_pull)){
    m[i,1]<-query_pull[[i]][query_pull[[i]]$rank == "family",][1,2]
    m[i,2]<-query_pull[[i]][query_pull[[i]]$rank == "order",][1,2]
    m[i,3]<-query_pull[[i]][query_pull[[i]]$rank == "class",][1,2]
    m[i,4]<-query_pull[[i]][query_pull[[i]]$rank == "phylum",][1,2]
    m[,5] <-query_match$search_string
}
colnames(m) <- c("family",  "order", "class", "phylum", "genus")
m <- data.frame(m[,c(5,1,2,3,4)])

seed_tx <- rbind(
m %>% mutate(kingdom = "Plantae") %>% 
  select(kingdom, phylum, class, order, family, genus) %>% 
  mutate(genus = stringr::str_to_title(genus)),
seed_raw_tax %>% filter(kingdom == "Plantae") %>% 
  filter(!is.na(family))
)
write.csv(seed_tx,"taxonomy/seed_tax_merged.csv", row.names = F)

# toxonomies tables transformations and arragements ####
# load all taxonomies on a list
clads <- c("agar", "amph", "bird", "chon", 
           "fern",  "fish", "mamm",
           "seed", 
           "squa")
tax_lst <- list()
for(i in 1:length(clads)){
  tax_lst[[i]] <- read.csv(paste0("taxonomy/",clads[i],"_tax.csv"))
}
names(tax_lst) <- clads

# transforms a 4 col taxonomic table to a 2 col table
tax_trf=function(temp_tax){
  temp_tax %>% dplyr::select(genus, family, order, class) %>% 
    tidyr::pivot_longer(everything(), names_to = "rank", values_to = "taxon") %>% 
    distinct(taxon, .keep_all = T) %>% arrange(rank, taxon)
}

lapply(tax_lst, tax_trf)

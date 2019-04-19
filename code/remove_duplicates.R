# Remove duplicate species, subspecies or varieties
remove_duplicates=function(phylo, names_list){
  for(i in 1:length(names_list)){
    t_labs<-extract.clade(phy = phylo, node = names_list[i])$tip.label
    t_drop<-t_labs[1:length(t_labs)-1]
    new_tre<-drop.tip(phylo, t_drop)
}
return(new_tre)
}

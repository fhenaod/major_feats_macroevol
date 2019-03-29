library(castor)
library(RPANDA)
library(parallel)

age<-325.051
lambda<-round(exp((-0.53356*log(age))+0.29817),3)
mu<-round(exp((-0.5257*log(age))-0.5016),3)

pars<-list(birth_rate_factor = lambda,
           death_rate_factor = mu)

bd_sim_tr<-list()
for(i in 1:1000){
  bd_sim_tr[[i]]<-generate_random_tree(pars, max_time = age)$tree
}
saveRDS(bd_sim_tr,"bd_sim_trees.rds")

sapply(bd_sim_tr, ape::Ntip)
sapply(bd_sim_tr, is.binary)
sapply(bd_sim_tr, is.ultrametric)

library(castor)
library(RPANDA)
library(parallel)

age<-325.051
lambda<-round(exp((-0.53356*log(age))+0.29817),3)
mu<-round(exp((-0.5257*log(age))-0.5016),3)

pars<-list(birth_rate_factor = lambda,
           death_rate_factor = mu)

bd_sim_tr<-list()
t<-c()
for(i in 1:1000){
  t<-generate_random_tree(pars, max_time = age)$tree
  if (!is.null(t)){
    if (Ntip(t) >= 4) bd_sim_tr[[i]]<-t
  } else { 
    i = i-1
  }	
}

bd_sim_tr<-bd_sim_tr[sapply(bd_sim_tr, function(x) !is.null(x)==TRUE)]
saveRDS(bd_sim_tr,"bd_sim_trees.rds")

sapply(bd_sim_tr, Ntip)
sapply(bd_sim_tr, is.binary)
sapply(bd_sim_tr, is.ultrametric)

# sim trees using henao-diaz pnas ntips
e.trees <- henao_d_trs
e_trees <- e.trees [sapply(e.trees, Ntip) > 20]

sim_b.7d.5 <- list()
for(i in 1:length(e_trees)){
  sim_b.7d.5[[i]] <- TreeSim::sim.bd.taxa(n = sapply(e_trees, Ntip)[i],
                                          numbsim = 1, lambda = 0.7, mu = 0.5)[[1]]
}

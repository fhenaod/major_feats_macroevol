library(phytools)

e_tips <- sapply(e.trees, Ntip)
e_ages <- sapply(e.trees, branching.times) %>% sapply(., max) %>% round(2)

# phytools ####
# conditioning on net divers
birth <- c(.7)
r <- c(.1, .25, .7)
death <- abs(birth - r)

sim_r_tr <- list()
for(i in 1:length(r)){
  strs <- pbtree(b = birth, d = death[i], n = 1000, scale = 100, 
                 nsim = 10, type = "continuous", extant.only = T)
  names(strs) <- rep(r[i], length(strs)) %>% paste0("r",.)
  
  sim_r_tr <- c(sim_r_tr, strs)
}

# conditionin on extinction fract
birth <- c(.9)
eps <- c(.01, .1, .25, .7)
death <- c(eps/birth) %>% round(2)

sim_r_tr2 <- list()
for(i in 1:length(eps)){
  strs <- pbtree(b = birth, d = death[i], n = 1000, scale = 100, 
         nsim = 10, type = "continuous", extant.only = T)
  names(strs) <- rep(death[i], length(strs)) %>% paste0("eps",.)
  
  sim_r_tr2 <- c(sim_r_tr2, strs)
}

names(sim_r_tr2) %>% table()
sapply(sim_r_tr2, class)
sapply(sim_r_tr2, Ntip) %>% hist()

plot(sim_r_tr2[[2]], show.tip.label = F)

# TreeSim ####
library(TreeSim)
# conditionin on extinction fract
birth <- c(.9)
eps <- c(.01, .1, .25, .7)
death <- c(eps/birth) %>% round(2)

sim_r_tr3 <- list()
for(i in 1:length(eps)){
  strs <- sim.bd.taxa(n = 1000, numbsim = 10, 
                      lambda = birth, mu = death[i], 
                      frac = 1, complete = F, stochsampling = FALSE)
  names(strs) <- rep(death[i], length(strs)) %>% paste0("eps",.)
  
  sim_r_tr3 <- c(sim_r_tr3, strs)
}

names(sim_r_tr3) %>% table()

sim_r_tr3_sc <- sim_r_tr3
for(i in 1:length(sim_r_tr3)){
  sim_r_tr3_sc[[i]]$edge.length <- sim_r_tr3[[i]]$edge.length*15
}
lapply(sim_r_tr3, branching.times) %>% 
  sapply(., max) %>% round(2)

lapply(sim_r_tr3_sc, branching.times) %>% 
  sapply(., max) %>% round(2) %>% summary()

library(apTreeshape)
library(parallel)

# from a list of trees get the maximum likelihood estimate of the Beta-splitting model
# and CI:95% with bootstrap replicates or ML profile
trees<-bird.sl
get_ml_beta=function(trees, n_bootstrap){
  beta<-c()
  b_low_ci<-c()
  b_up_ci<-c()
  for(i in 1:length(trees)){
    if(Ntip(trees[[i]])<=2) {
      beta[i]<-NA
      b_low_ci[i]<-NA
      b_up_ci[i]<-NA
    } else {
      if(Ntip(trees[[i]])<=50) {
        ml_beta<-maxlik.betasplit(trees[[i]], confidence.interval = "bootstrap", conf.level = 0.95, size.bootstrap = n_bootstrap)
        beta[i]<-ml_beta$max_lik
        b_low_ci[i]<-ml_beta$conf_interval[[1]]
        b_up_ci[i]<-ml_beta$conf_interval[[2]]
      } else {
        ml_beta<-maxlik.betasplit(trees[[i]], confidence.interval = "profile", conf.level = 0.95, size.bootstrap = n_bootstrap)
        beta[i]<-ml_beta$max_lik
        b_low_ci[i]<-ml_beta$conf_interval[[1]]
        b_up_ci[i]<-ml_beta$conf_interval[[2]]
      }
    }
  }
  df<-round(data.frame(beta, b_low_ci, b_up_ci),3)
  return(df)
}

bird_beta<-get_ml_beta(bird.sl)

# parallel version
bird_beta<-mclapply(list(bird.sl),  function(x) get_ml_beta(x, 1000), mc.cores = 3)[[1]]
saveRDS(bird_beta, file = "bird_ml_beta.rds")
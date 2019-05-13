
# Load slicing summary results ####
agar_sl_stat<-readRDS("Slicing/agar/output/agar_sum_stats.rds")
agar_sl_stat$taxon<-rep("Agaricomycetes", dim(agar_sl_stat)[1])
agar_sl_stat$rel_age<-agar_sl_stat$tree.max.age/442.6426

amph_sl_stat<-readRDS("Slicing/amph/output/amph_sum_stats.rds")
amph_sl_stat$taxon<-rep("Amphibia", dim(amph_sl_stat)[1])
amph_sl_stat$rel_age<-amph_sl_stat$tree.max.age/312.7661

bird_sl_stat<-readRDS("Slicing/bird/output/bird_sum_stats.rds")
bird_sl_stat$taxon<-rep("Aves", dim(bird_sl_stat)[1])
bird_sl_stat$rel_age<-bird_sl_stat$tree.max.age/113.2497

chon_sl_stat<-readRDS("Slicing/chon/output/chon_sum_stats.rds")
chon_sl_stat$taxon<-rep("Chondrichthyes", dim(chon_sl_stat)[1])
chon_sl_stat$rel_age<-chon_sl_stat$tree.max.age/378.0406 

fern_sl_stat<-readRDS("Slicing/fern/output/fern_sum_stats.rds")
fern_sl_stat$taxon<-rep("Polypodiopsida", dim(fern_sl_stat)[1])
fern_sl_stat$rel_age<-fern_sl_stat$tree.max.age/476.0093

fish_sl_stat<-readRDS("Slicing/fish/output/fish_sum_stats.rds")
fish_sl_stat$taxon<-rep("Actinopterygii", dim(fish_sl_stat)[1])
fish_sl_stat$rel_age<-fish_sl_stat$tree.max.age/368.0270

seed_sl_stat<-readRDS("Slicing/seed/output/seed_sum_stats.rds")
seed_sl_stat$taxon<-rep("Spermatophyta", dim(seed_sl_stat)[1])
seed_sl_stat$rel_age<-seed_sl_stat$tree.max.age/325.0508

squa_sl_stat<-readRDS("Slicing/squa/output/squa_sum_stats.rds")
squa_sl_stat$taxon<-rep("Squamata", dim(squa_sl_stat)[1])
squa_sl_stat$rel_age<-squa_sl_stat$tree.max.age/189.9633

## Add ML beta spliting estimates ####
agar_beta<-readRDS("Slicing/ml_beta/agar/agar_ml_beta.rds")
amph_beta<-readRDS("Slicing/ml_beta/amph/amph_ml_beta.rds")
bird_beta<-readRDS("Slicing/ml_beta/bird/bird_ml_beta.rds")
chon_beta<-readRDS("Slicing/ml_beta/chon/chon_ml_beta.rds")
fern_beta<-readRDS("Slicing/ml_beta/fern/fern_ml_beta.rds")
fish_beta<-readRDS("Slicing/ml_beta/fish/fish_ml_beta.rds")
squa_beta<-readRDS("Slicing/ml_beta/squa/squa_ml_beta.rds")
seed_beta<-readRDS("Slicing/ml_beta/seed/seed_ml_beta.rds")

agar_sl_stat<-cbind(agar_sl_stat, agar_beta)
amph_sl_stat<-cbind(amph_sl_stat, amph_beta)
bird_sl_stat<-cbind(bird_sl_stat, bird_beta)
chon_sl_stat<-cbind(chon_sl_stat, chon_beta)
fern_sl_stat<-cbind(fern_sl_stat, fern_beta)
fish_sl_stat<-cbind(fish_sl_stat, fish_beta)
squa_sl_stat<-cbind(squa_sl_stat, squa_beta)
seed_sl_stat<-cbind(seed_sl_stat, seed_beta)

## Add branch length/time means ####
agar_branch<-readRDS("Slicing/branch_stats/agar_branch_stats.rds")
amph_branch<-readRDS("Slicing/branch_stats/amph_branch_stats.rds")
bird_branch<-readRDS("Slicing/branch_stats/bird_branch_stats.rds")
chon_branch<-readRDS("Slicing/branch_stats/chon_branch_stats.rds")
fern_branch<-readRDS("Slicing/branch_stats/fern_branch_stats.rds")
fish_branch<-readRDS("Slicing/branch_stats/fish_branch_stats.rds")
squa_branch<-readRDS("Slicing/branch_stats/squa_branch_stats.rds")
seed_branch<-readRDS("Slicing/branch_stats/seed_branch_stats.rds")

agar_sl_stat<-cbind(agar_sl_stat, agar_branch)
amph_sl_stat<-cbind(amph_sl_stat, amph_branch)
bird_sl_stat<-cbind(bird_sl_stat, bird_branch)
chon_sl_stat<-cbind(chon_sl_stat, chon_branch)
fern_sl_stat<-cbind(fern_sl_stat, fern_branch)
fish_sl_stat<-cbind(fish_sl_stat, fish_branch)
squa_sl_stat<-cbind(squa_sl_stat, squa_branch)
seed_sl_stat<-cbind(seed_sl_stat, seed_branch)

#####
sum_stats<-rbind(agar_sl_stat, amph_sl_stat, bird_sl_stat, chon_sl_stat, fern_sl_stat, fish_sl_stat, seed_sl_stat, squa_sl_stat)
head(sum_stats)

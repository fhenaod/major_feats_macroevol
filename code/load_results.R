# SLICING #####
# Load slicing summary results 
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

## Add ML beta spliting estimates
amph_beta<-readRDS("Slicing/ml_beta/amph/amph_ml_beta.rds")
bird_beta<-readRDS("Slicing/ml_beta/bird/bird_ml_beta.rds")
chon_beta<-readRDS("Slicing/ml_beta/chon/chon_ml_beta.rds")
fish_beta<-readRDS("Slicing/ml_beta/fish/fish_ml_beta.rds")
squa_beta<-readRDS("Slicing/ml_beta/squa/squa_ml_beta.rds")
seed_beta<-readRDS("Slicing/ml_beta/seed/seed_ml_beta.rds")

amph_sl_stat<-cbind(amph_sl_stat, amph_beta)
bird_sl_stat<-cbind(bird_sl_stat, bird_beta)
chon_sl_stat<-cbind(chon_sl_stat, chon_beta)
fish_sl_stat<-cbind(fish_sl_stat, fish_beta)
squa_sl_stat<-cbind(squa_sl_stat, squa_beta)
seed_sl_stat<-cbind(seed_sl_stat, seed_beta)

## Add branch length/time means 
amph_branch<-readRDS("Slicing/branch_stats/amph_branch_stats.rds")
bird_branch<-readRDS("Slicing/branch_stats/bird_branch_stats.rds")
chon_branch<-readRDS("Slicing/branch_stats/chon_branch_stats.rds")
fish_branch<-readRDS("Slicing/branch_stats/fish_branch_stats.rds")
squa_branch<-readRDS("Slicing/branch_stats/squa_branch_stats.rds")
seed_branch<-readRDS("Slicing/branch_stats/seed_branch_stats.rds")

amph_sl_stat<-cbind(amph_sl_stat, amph_branch)
bird_sl_stat<-cbind(bird_sl_stat, bird_branch)
chon_sl_stat<-cbind(chon_sl_stat, chon_branch)
fish_sl_stat<-cbind(fish_sl_stat, fish_branch)
squa_sl_stat<-cbind(squa_sl_stat, squa_branch)
seed_sl_stat<-cbind(seed_sl_stat, seed_branch)

sum_stats<-rbind(agar_sl_stat, amph_sl_stat, bird_sl_stat, chon_sl_stat, fern_sl_stat, fish_sl_stat
                 #, seed_sl_stat
                 , squa_sl_stat)
head(sum_stats)


# RANKS ####

agar_fams_stats<-readRDS("rank_sampling/agar_fams/output/agar_fams_sum_stats.rds")
agar_fams_stats$rel_age<-agar_fams_stats$tree.max.age/max(agar_fams_stats$tree.max.age)
agar_fams_stats$taxon<-rep("agar_fams", dim(agar_fams_stats)[1])

agar_ords_stats<-readRDS("rank_sampling/agar_ords/output/agar_ords_sum_stats.rds")
agar_ords_stats$rel_age<-agar_ords_stats$tree.max.age/max(agar_ords_stats$tree.max.age)
agar_ords_stats$taxon<-rep("agar_ords", dim(agar_ords_stats)[1])

amph_fams_stats<-readRDS("rank_sampling/amph_fams/output/amph_fams_sum_stats.rds")
amph_fams_stats$rel_age<-amph_fams_stats$tree.max.age/max(amph_fams_stats$tree.max.age)
amph_fams_stats$taxon<-rep("amph_fams", dim(amph_fams_stats)[1])

amph_ords_stats<-readRDS("rank_sampling/amph_ords/output/amph_ords_sum_stats.rds")
amph_ords_stats$rel_age<-amph_ords_stats$tree.max.age/max(amph_ords_stats$tree.max.age)
amph_ords_stats$taxon<-rep("amph_ords", dim(amph_ords_stats)[1])

bird_fams_stats<-readRDS("rank_sampling/bird_fams/output/bird_fams_sum_stats.rds")
bird_fams_stats$rel_age<-bird_fams_stats$tree.max.age/max(bird_fams_stats$tree.max.age)
bird_fams_stats$taxon<-rep("bird_fams", dim(bird_fams_stats)[1])

bird_ords_stats<-readRDS("rank_sampling/bird_ords/output/bird_ords_sum_stats.rds")
bird_ords_stats$rel_age<-bird_ords_stats$tree.max.age/max(bird_ords_stats$tree.max.age)
bird_ords_stats$taxon<-rep("bird_ords", dim(bird_ords_stats)[1])

chon_fams_stats<-readRDS("rank_sampling/chon_fams/output/chon_fams_sum_stats.rds")
chon_fams_stats$rel_age<-chon_fams_stats$tree.max.age/max(chon_fams_stats$tree.max.age)
chon_fams_stats$taxon<-rep("chon_fams", dim(chon_fams_stats)[1])

chon_ords_stats<-readRDS("rank_sampling/chon_ords/output/chon_ords_sum_stats.rds")
chon_ords_stats$rel_age<-chon_ords_stats$tree.max.age/max(chon_ords_stats$tree.max.age)
chon_ords_stats$taxon<-rep("chon_ords", dim(chon_ords_stats)[1])

fern_fams_stats<-readRDS("rank_sampling/fern_fams/output/fern_fams_sum_stats.rds")
fern_fams_stats$rel_age<-fern_fams_stats$tree.max.age/max(fern_fams_stats$tree.max.age)
fern_fams_stats$taxon<-rep("fern_fams", dim(fern_fams_stats)[1])

fern_ords_stats<-readRDS("rank_sampling/fern_ords/output/fern_ords_sum_stats.rds")
fern_ords_stats$rel_age<-fern_ords_stats$tree.max.age/max(fern_ords_stats$tree.max.age)
fern_ords_stats$taxon<-rep("fern_ords", dim(fern_ords_stats)[1])

fish_fams_stats<-readRDS("rank_sampling/fish_fams/output/fish_fams_sum_stats.rds")
fish_fams_stats$rel_age<-fish_fams_stats$tree.max.age/max(fish_fams_stats$tree.max.age)
fish_fams_stats$taxon<-rep("fish_fams", dim(fish_fams_stats)[1])

fish_ords_stats<-readRDS("rank_sampling/fish_ords/output/fish_ords_sum_stats.rds")
fish_ords_stats$rel_age<-fish_ords_stats$tree.max.age/max(fish_ords_stats$tree.max.age)
fish_ords_stats$taxon<-rep("fish_ords", dim(fish_ords_stats)[1])

squa_fams_stats<-readRDS("rank_sampling/squa_fams/output/squa_fams_sum_stats.rds")
squa_fams_stats$rel_age<-squa_fams_stats$tree.max.age/max(squa_fams_stats$tree.max.age)
squa_fams_stats$taxon<-rep("squa_fams", dim(squa_fams_stats)[1])

seed_fams_stats<-readRDS("rank_sampling/seed_fams/output/seed_fams_sum_stats.rds")
seed_fams_stats$rel_age<-seed_fams_stats$tree.max.age/max(seed_fams_stats$tree.max.age)
seed_fams_stats$taxon<-rep("seed_fams", dim(seed_fams_stats)[1])

seed_ords_stats<-readRDS("rank_sampling/seed_ords/output/seed_ords_sum_stats.rds")
seed_ords_stats$rel_age<-seed_ords_stats$tree.max.age/max(seed_ords_stats$tree.max.age)
seed_ords_stats$taxon<-rep("seed_ords", dim(seed_ords_stats)[1])

seed_clas_stats<-readRDS("rank_sampling/seed_clas/output/seed_clas_sum_stats.rds")
seed_clas_stats$rel_age<-seed_clas_stats$tree.max.age/max(seed_clas_stats$tree.max.age)
seed_clas_stats$taxon<-rep("seed_clas", dim(seed_clas_stats)[1])

sum_stats<-rbind(agar_fams_stats, agar_ords_stats,
                 amph_fams_stats, amph_ords_stats,
                 bird_fams_stats, bird_ords_stats,
                 chon_fams_stats, chon_ords_stats,
                 fern_fams_stats, fern_ords_stats,
                 fish_fams_stats, fish_ords_stats,
                 squa_fams_stats
                 #,seed_fams_stats, seed_ords_stats, seed_clas_stats
                 )
head(sum_stats)
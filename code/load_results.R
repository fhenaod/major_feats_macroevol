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


# SELF-SIMILAR ####
bird_5<-readRDS("self_sim/bird/bird_5/output/bird_5_sum_stats.rds")
bird_5$taxon<-rep("bird_5", dim(bird_5)[1])
bird_10<-readRDS("self_sim/bird/bird_10/output/bird_10_sum_stats.rds")
bird_10$taxon<-rep("bird_10", dim(bird_10)[1])
bird_20<-readRDS("self_sim/bird/bird_20/output/bird_20_sum_stats.rds")
bird_20$taxon<-rep("bird_20", dim(bird_20)[1])
bird_30<-readRDS("self_sim/bird/bird_30/output/bird_30_sum_stats.rds")
bird_30$taxon<-rep("bird_30", dim(bird_30)[1])
bird_40<-readRDS("self_sim/bird/bird_40/output/bird_40_sum_stats.rds")
bird_40$taxon<-rep("bird_40", dim(bird_40)[1])
bird_50<-readRDS("self_sim/bird/bird_50/output/bird_50_sum_stats.rds")
bird_50$taxon<-rep("bird_50", dim(bird_50)[1])

amph_5<-readRDS("self_sim/amph/amph_5/output/amph_5_sum_stats.rds")
amph_5$taxon<-rep("amph_5", dim(amph_5)[1])
amph_10<-readRDS("self_sim/amph/amph_10/output/amph_10_sum_stats.rds")
amph_10$taxon<-rep("amph_10", dim(amph_10)[1])
amph_20<-readRDS("self_sim/amph/amph_20/output/amph_20_sum_stats.rds")
amph_20$taxon<-rep("amph_20", dim(amph_20)[1])
amph_30<-readRDS("self_sim/amph/amph_30/output/amph_30_sum_stats.rds")
amph_30$taxon<-rep("amph_30", dim(amph_30)[1])
amph_40<-readRDS("self_sim/amph/amph_40/output/amph_40_sum_stats.rds")
amph_40$taxon<-rep("amph_40", dim(amph_40)[1])
amph_50<-readRDS("self_sim/amph/amph_50/output/amph_50_sum_stats.rds")
amph_50$taxon<-rep("amph_50", dim(amph_50)[1])

fish_5<-readRDS("self_sim/fish/fish_5/output/fish_5_sum_stats.rds")
fish_5$taxon<-rep("fish_5", dim(fish_5)[1])
fish_10<-readRDS("self_sim/fish/fish_10/output/fish_10_sum_stats.rds")
fish_10$taxon<-rep("fish_10", dim(fish_10)[1])
fish_20<-readRDS("self_sim/fish/fish_20/output/fish_20_sum_stats.rds")
fish_20$taxon<-rep("fish_20", dim(fish_20)[1])
fish_30<-readRDS("self_sim/fish/fish_30/output/fish_30_sum_stats.rds")
fish_30$taxon<-rep("fish_30", dim(fish_30)[1])
fish_40<-readRDS("self_sim/fish/fish_40/output/fish_40_sum_stats.rds")
fish_40$taxon<-rep("fish_40", dim(fish_40)[1])
fish_50<-readRDS("self_sim/fish/fish_50/output/fish_50_sum_stats.rds")
fish_50$taxon<-rep("fish_50", dim(fish_50)[1])

chon_5<-readRDS("self_sim/chon/chon_5/output/chon_5_sum_stats.rds")
chon_5$taxon<-rep("chon_5", dim(chon_5)[1])
chon_10<-readRDS("self_sim/chon/chon_10/output/chon_10_sum_stats.rds")
chon_10$taxon<-rep("chon_10", dim(chon_10)[1])
chon_20<-readRDS("self_sim/chon/chon_20/output/chon_20_sum_stats.rds")
chon_20$taxon<-rep("chon_20", dim(chon_20)[1])
chon_30<-readRDS("self_sim/chon/chon_30/output/chon_30_sum_stats.rds")
chon_30$taxon<-rep("chon_30", dim(chon_30)[1])
chon_40<-readRDS("self_sim/chon/chon_40/output/chon_40_sum_stats.rds")
chon_40$taxon<-rep("chon_40", dim(chon_40)[1])
chon_50<-readRDS("self_sim/chon/chon_50/output/chon_50_sum_stats.rds")
chon_50$taxon<-rep("chon_50", dim(chon_50)[1])

squa_5<-readRDS("self_sim/squa/squa_5/output/squa_5_sum_stats.rds")
squa_5$taxon<-rep("squa_5", dim(squa_5)[1])
squa_10<-readRDS("self_sim/squa/squa_10/output/squa_10_sum_stats.rds")
squa_10$taxon<-rep("squa_10", dim(squa_10)[1])
squa_20<-readRDS("self_sim/squa/squa_20/output/squa_20_sum_stats.rds")
squa_20$taxon<-rep("squa_20", dim(squa_20)[1])
squa_30<-readRDS("self_sim/squa/squa_30/output/squa_30_sum_stats.rds")
squa_30$taxon<-rep("squa_30", dim(squa_30)[1])
squa_40<-readRDS("self_sim/squa/squa_40/output/squa_40_sum_stats.rds")
squa_40$taxon<-rep("squa_40", dim(squa_40)[1])
squa_50<-readRDS("self_sim/squa/squa_50/output/squa_50_sum_stats.rds")
squa_50$taxon<-rep("squa_50", dim(squa_50)[1])

fern_5<-readRDS("self_sim/fern/fern_5/output/fern_5_sum_stats.rds")
fern_5$taxon<-rep("fern_5", dim(fern_5)[1])
fern_10<-readRDS("self_sim/fern/fern_10/output/fern_10_sum_stats.rds")
fern_10$taxon<-rep("fern_10", dim(fern_10)[1])
fern_20<-readRDS("self_sim/fern/fern_20/output/fern_20_sum_stats.rds")
fern_20$taxon<-rep("fern_20", dim(fern_20)[1])
fern_30<-readRDS("self_sim/fern/fern_30/output/fern_30_sum_stats.rds")
fern_30$taxon<-rep("fern_30", dim(fern_30)[1])
fern_40<-readRDS("self_sim/fern/fern_40/output/fern_40_sum_stats.rds")
fern_40$taxon<-rep("fern_40", dim(fern_40)[1])
fern_50<-readRDS("self_sim/fern/fern_50/output/fern_50_sum_stats.rds")
fern_50$taxon<-rep("fern_50", dim(fern_50)[1])

agar_5<-readRDS("self_sim/agar/agar_5/output/agar_5_sum_stats.rds")
agar_5$taxon<-rep("agar_5", dim(agar_5)[1])
agar_10<-readRDS("self_sim/agar/agar_10/output/agar_10_sum_stats.rds")
agar_10$taxon<-rep("agar_10", dim(agar_10)[1])
agar_20<-readRDS("self_sim/agar/agar_20/output/agar_20_sum_stats.rds")
agar_20$taxon<-rep("agar_20", dim(agar_20)[1])
agar_30<-readRDS("self_sim/agar/agar_30/output/agar_30_sum_stats.rds")
agar_30$taxon<-rep("agar_30", dim(agar_30)[1])
agar_40<-readRDS("self_sim/agar/agar_40/output/agar_40_sum_stats.rds")
agar_40$taxon<-rep("agar_40", dim(agar_40)[1])
agar_50<-readRDS("self_sim/agar/agar_50/output/agar_50_sum_stats.rds")
agar_50$taxon<-rep("agar_50", dim(agar_50)[1])

sum_stats<-rbind( bird_5, bird_10, bird_20, bird_30, bird_40, bird_50
                 ,amph_5, amph_10, amph_20, amph_30, amph_40, amph_50
                 ,fish_5, fish_10, fish_20, fish_30, fish_40, fish_50
                 ,chon_5, chon_10, chon_20, chon_30, chon_40, chon_50
                 ,squa_5, squa_10, squa_20, squa_30, squa_40, squa_50
                 #,fern_5, fern_10, fern_20, fern_30, fern_40, fern_50
                 #,agar_5, agar_10, agar_20, agar_30, agar_40, agar_50
                )


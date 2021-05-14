###
##### Conscensus tree
###

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

mamm_sl_stat<-readRDS("Slicing/mamm/output/mamm_sum_stats.rds")
mamm_sl_stat$taxon<-rep("Mammalia", dim(mamm_sl_stat)[1])
mamm_sl_stat$rel_age<-mamm_sl_stat$tree.max.age/180.8472
  
seed_sl_stat<-readRDS("Slicing/seed/output/seed_sum_stats.rds")
seed_sl_stat$taxon<-rep("Spermatophyta", dim(seed_sl_stat)[1])
seed_sl_stat$rel_age<-seed_sl_stat$tree.max.age/325.0508

squa_sl_stat<-readRDS("Slicing/squa/output/squa_sum_stats.rds")
squa_sl_stat$taxon<-rep("Squamata", dim(squa_sl_stat)[1])
squa_sl_stat$rel_age<-squa_sl_stat$tree.max.age/189.9633

## ML beta spliting estimates
amph_beta <- readRDS("Slicing/ml_beta/amph/amph_ml_beta.rds")
bird_beta <- readRDS("Slicing/ml_beta/bird/bird_ml_beta.rds")
chon_beta <- readRDS("Slicing/ml_beta/chon/chon_ml_beta.rds")
fish_beta <- readRDS("Slicing/ml_beta/fish/fish_ml_beta.rds")
squa_beta <- readRDS("Slicing/ml_beta/squa/squa_ml_beta.rds")
seed_beta <- data.frame(beta = rep(NA, dim(seed_sl_stat)[1]), 
                        b_low_ci = rep(NA, dim(seed_sl_stat)[1]), 
                        b_up_ci = rep(NA, dim(seed_sl_stat)[1]))

amph_sl_stat <- cbind(amph_sl_stat, amph_beta)
bird_sl_stat <- cbind(bird_sl_stat, bird_beta)
chon_sl_stat <- cbind(chon_sl_stat, chon_beta)
fish_sl_stat <- cbind(fish_sl_stat, fish_beta)
squa_sl_stat <- cbind(squa_sl_stat, squa_beta)
seed_sl_stat <- cbind(seed_sl_stat, seed_beta)

## Add branch length/time means 
amph_branch <- readRDS("Slicing/branch_stats/amph_branch_stats.rds")
bird_branch <- readRDS("Slicing/branch_stats/bird_branch_stats.rds")
chon_branch <- readRDS("Slicing/branch_stats/chon_branch_stats.rds")
fish_branch <- readRDS("Slicing/branch_stats/fish_branch_stats.rds")
squa_branch <- readRDS("Slicing/branch_stats/squa_branch_stats.rds")

amph_sl_stat <- cbind(amph_sl_stat, amph_branch)
bird_sl_stat <- cbind(bird_sl_stat, bird_branch)
chon_sl_stat <- cbind(chon_sl_stat, chon_branch)
fish_sl_stat <- cbind(fish_sl_stat, fish_branch)
squa_sl_stat <- cbind(squa_sl_stat, squa_branch)

sl_sum_stats <- rbind(agar_sl_stat, amph_sl_stat, bird_sl_stat, 
                      chon_sl_stat, fern_sl_stat, fish_sl_stat, 
                      seed_sl_stat, squa_sl_stat, mamm_sl_stat)

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

fern_clas_stats<-readRDS("rank_sampling/fern_clas/output/fern_ords_sum_stats.rds")
fern_clas_stats$rel_age<-fern_clas_stats$tree.max.age/max(fern_clas_stats$tree.max.age)
fern_clas_stats$taxon<-rep("fern_clas", dim(fern_clas_stats)[1])

fish_fams_stats<-readRDS("rank_sampling/fish_fams/output/fish_fams_sum_stats.rds")
fish_fams_stats$rel_age<-fish_fams_stats$tree.max.age/max(fish_fams_stats$tree.max.age)
fish_fams_stats$taxon<-rep("fish_fams", dim(fish_fams_stats)[1])

fish_ords_stats<-readRDS("rank_sampling/fish_ords/output/fish_ords_sum_stats.rds")
fish_ords_stats$rel_age<-fish_ords_stats$tree.max.age/max(fish_ords_stats$tree.max.age)
fish_ords_stats$taxon<-rep("fish_ords", dim(fish_ords_stats)[1])

mamm_fams_stats<-readRDS("rank_sampling/mamm_fams/output/mamm_fams_sum_stats.rds")
mamm_fams_stats$rel_age<-mamm_fams_stats$tree.max.age/max(mamm_fams_stats$tree.max.age)
mamm_fams_stats$taxon<-rep("mamm_fams", dim(mamm_fams_stats)[1])

mamm_ords_stats<-readRDS("rank_sampling/mamm_ords/output/mamm_ords_sum_stats.rds")
mamm_ords_stats$rel_age<-mamm_ords_stats$tree.max.age/max(mamm_ords_stats$tree.max.age)
mamm_ords_stats$taxon<-rep("mamm_ords", dim(mamm_ords_stats)[1])

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

rank_sum_stats<-rbind(agar_fams_stats, agar_ords_stats, 
                      amph_fams_stats, amph_ords_stats, 
                      bird_fams_stats, bird_ords_stats, 
                      chon_fams_stats, chon_ords_stats, 
                      fern_fams_stats, fern_ords_stats, fern_clas_stats, 
                      fish_fams_stats, fish_ords_stats, 
                      mamm_fams_stats, mamm_ords_stats, 
                      squa_fams_stats,
                      seed_fams_stats, seed_ords_stats, seed_clas_stats)
rank_sum_stats$rank <- sapply(strsplit(rank_sum_stats$taxon, "_"), "[", 2)
rank_sum_stats$taxon <- stringr::str_extract(rank_sum_stats$taxon, "[^_]+")

## approximate-age-rank random trees
agar_fams_rdm <- readRDS("rand_age_rank/output/agar_fams_rdm_trs.rds_sum.rds")
agar_ords_rdm <- readRDS("rand_age_rank/output/agar_ords_rdm_trs.rds_sum.rds")
amph_fams_rdm <- readRDS("rand_age_rank/output/amph_fams_rdm_trs.rds_sum.rds")
bird_fams_rdm <- readRDS("rand_age_rank/output/bird_fams_rdm_trs.rds_sum.rds")
bird_ords_rdm <- readRDS("rand_age_rank/output/bird_ords_rdm_trs.rds_sum.rds")
chon_fams_rdm <- readRDS("rand_age_rank/output/chon_fams_rdm_trs.rds_sum.rds")
chon_ords_rdm <- readRDS("rand_age_rank/output/chon_ords_rdm_trs.rds_sum.rds")
fern_fams_rdm <- readRDS("rand_age_rank/output/fern_fams_rdm_trs.rds_sum.rds")
fern_ords_rdm <- readRDS("rand_age_rank/output/fern_ords_rdm_trs.rds_sum.rds")
fish_fams_rdm <- readRDS("rand_age_rank/output/fish_fams_rdm_trs.rds_sum.rds")
fish_ords_rdm <- readRDS("rand_age_rank/output/fish_ords_rdm_trs.rds_sum.rds")
mamm_fams_rdm <- readRDS("rand_age_rank/output/mamm_fams_rdm_trs.rds_sum.rds")
mamm_ords_rdm <- readRDS("rand_age_rank/output/mamm_ords_rdm_trs.rds_sum.rds")
squa_fams_rdm <- readRDS("rand_age_rank/output/squa_fams_rdm_trs.rds_sum.rds")

rank_rdm_sum <- rbind(agar_fams_rdm, agar_ords_rdm, 
                     amph_fams_rdm, bird_fams_rdm, bird_ords_rdm, 
                     chon_fams_rdm, chon_ords_rdm, fern_fams_rdm, 
                     fern_ords_rdm, fish_fams_rdm, fish_ords_rdm, 
                     mamm_fams_rdm, mamm_ords_rdm, squa_fams_rdm)

# SELF-SIMILAR ####
# random age sampling root edge appended
agar_5<-readRDS("self_sim/stem_edge_add/agar/agar_5_sum_stats.rds")
agar_5$taxon<-rep("agar_5", dim(agar_5)[1])
agar_10<-readRDS("self_sim/stem_edge_add/agar/agar_10_sum_stats.rds")
agar_10$taxon<-rep("agar_10", dim(agar_10)[1])
agar_20<-readRDS("self_sim/stem_edge_add/agar/agar_20_sum_stats.rds")
agar_20$taxon<-rep("agar_20", dim(agar_20)[1])
agar_30<-readRDS("self_sim/stem_edge_add/agar/agar_30_sum_stats.rds")
agar_30$taxon<-rep("agar_30", dim(agar_30)[1])
agar_40<-readRDS("self_sim/stem_edge_add/agar/agar_40_sum_stats.rds")
agar_40$taxon<-rep("agar_40", dim(agar_40)[1])
agar_50<-readRDS("self_sim/stem_edge_add/agar/agar_50_sum_stats.rds")
agar_50$taxon<-rep("agar_50", dim(agar_50)[1])

amph_5<-readRDS("self_sim/stem_edge_add/amph/amph_5_sum_stats.rds")
amph_5$taxon<-rep("amph_5", dim(amph_5)[1])
amph_10<-readRDS("self_sim/stem_edge_add/amph/amph_10_sum_stats.rds")
amph_10$taxon<-rep("amph_10", dim(amph_10)[1])
amph_20<-readRDS("self_sim/stem_edge_add/amph/amph_20_sum_stats.rds")
amph_20$taxon<-rep("amph_20", dim(amph_20)[1])
amph_30<-readRDS("self_sim/stem_edge_add/amph/amph_30_sum_stats.rds")
amph_30$taxon<-rep("amph_30", dim(amph_30)[1])
amph_40<-readRDS("self_sim/stem_edge_add/amph/amph_40_sum_stats.rds")
amph_40$taxon<-rep("amph_40", dim(amph_40)[1])
amph_50<-readRDS("self_sim/stem_edge_add/amph/amph_50_sum_stats.rds")
amph_50$taxon<-rep("amph_50", dim(amph_50)[1])

bird_5<-readRDS("self_sim/stem_edge_add/bird/bird_5_sum_stats.rds")
bird_5$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/bird/bird_5_brake_ages.rds")
bird_5$taxon<-rep("bird_5", dim(bird_5)[1])
bird_10<-readRDS("self_sim/stem_edge_add/bird/bird_10_sum_stats.rds")
bird_10$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/bird/bird_10_brake_ages.rds")
bird_10$taxon<-rep("bird_10", dim(bird_10)[1])
bird_20<-readRDS("self_sim/stem_edge_add/bird/bird_20_sum_stats.rds")
bird_20$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/bird/bird_20_brake_ages.rds")
bird_20$taxon<-rep("bird_20", dim(bird_20)[1])
bird_30<-readRDS("self_sim/stem_edge_add/bird/bird_30_sum_stats.rds")
bird_30$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/bird/bird_30_brake_ages.rds")
bird_30$taxon<-rep("bird_30", dim(bird_30)[1])
bird_40<-readRDS("self_sim/stem_edge_add/bird/bird_40_sum_stats.rds")
bird_40$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/bird/bird_40_brake_ages.rds")
bird_40$taxon<-rep("bird_40", dim(bird_40)[1])
bird_50<-readRDS("self_sim/stem_edge_add/bird/bird_50_sum_stats.rds")
bird_50$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/bird/bird_50_brake_ages.rds")
bird_50$taxon<-rep("bird_50", dim(bird_50)[1])

#chon_5<-readRDS("self_sim/stem_edge_add/chon/chon_5_sum_stats.rds")
#chon_5$taxon<-rep("chon_5", dim(chon_5)[1])
chon_10<-readRDS("self_sim/stem_edge_add/chon/chon_10_sum_stats.rds")
chon_10$taxon<-rep("chon_10", dim(chon_10)[1])
chon_20<-readRDS("self_sim/stem_edge_add/chon/chon_20_sum_stats.rds")
chon_20$taxon<-rep("chon_20", dim(chon_20)[1])
chon_30<-readRDS("self_sim/stem_edge_add/chon/chon_30_sum_stats.rds")
chon_30$taxon<-rep("chon_30", dim(chon_30)[1])
chon_40<-readRDS("self_sim/stem_edge_add/chon/chon_40_sum_stats.rds")
chon_40$taxon<-rep("chon_40", dim(chon_40)[1])
chon_50<-readRDS("self_sim/stem_edge_add/chon/chon_50_sum_stats.rds")
chon_50$taxon<-rep("chon_50", dim(chon_50)[1])

fern_5<-readRDS("self_sim/stem_edge_add/fern/fern_5_sum_stats.rds")
fern_5$taxon<-rep("fern_5", dim(fern_5)[1])
fern_10<-readRDS("self_sim/stem_edge_add/fern/fern_10_sum_stats.rds")
fern_10$taxon<-rep("fern_10", dim(fern_10)[1])
fern_20<-readRDS("self_sim/stem_edge_add/fern/fern_20_sum_stats.rds")
fern_20$taxon<-rep("fern_20", dim(fern_20)[1])
fern_30<-readRDS("self_sim/stem_edge_add/fern/fern_30_sum_stats.rds")
fern_30$taxon<-rep("fern_30", dim(fern_30)[1])
fern_40<-readRDS("self_sim/stem_edge_add/fern/fern_40_sum_stats.rds")
fern_40$taxon<-rep("fern_40", dim(fern_40)[1])
fern_50<-readRDS("self_sim/stem_edge_add/fern/fern_50_sum_stats.rds")
fern_50$taxon<-rep("fern_50", dim(fern_50)[1])

fish_5<-readRDS("self_sim/stem_edge_add/fish/fish_5_sum_stats.rds")
fish_5$taxon<-rep("fish_5", dim(fish_5)[1])
fish_10<-readRDS("self_sim/stem_edge_add/fish/fish_10_sum_stats.rds")
fish_10$taxon<-rep("fish_10", dim(fish_10)[1])
fish_20<-readRDS("self_sim/stem_edge_add/fish/fish_20_sum_stats.rds")
fish_20$taxon<-rep("fish_20", dim(fish_20)[1])
fish_30<-readRDS("self_sim/stem_edge_add/fish/fish_30_sum_stats.rds")
fish_30$taxon<-rep("fish_30", dim(fish_30)[1])
fish_40<-readRDS("self_sim/stem_edge_add/fish/fish_40_sum_stats.rds")
fish_40$taxon<-rep("fish_40", dim(fish_40)[1])
fish_50<-readRDS("self_sim/stem_edge_add/fish/fish_50_sum_stats.rds")
fish_50$taxon<-rep("fish_50", dim(fish_50)[1])

mamm_5<-readRDS("self_sim/stem_edge_add/mamm/mamm_5/output/mamm_5_sum_stats.rds")
mamm_5$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/mamm/mamm_5/mamm_5_brake_ages.rds")
mamm_5$taxon<-rep("mamm_5", dim(mamm_5)[1])
mamm_10<-readRDS("self_sim/stem_edge_add/mamm/mamm_10/output/mamm_10_sum_stats.rds")
mamm_10$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/mamm/mamm_10/mamm_10_brake_ages.rds")
mamm_10$taxon<-rep("mamm_10", dim(mamm_10)[1])
mamm_20<-readRDS("self_sim/stem_edge_add/mamm/mamm_20/output/mamm_20_sum_stats.rds")
mamm_20$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/mamm/mamm_20/mamm_20_brake_ages.rds")
mamm_20$taxon<-rep("mamm_20", dim(mamm_20)[1])
mamm_30<-readRDS("self_sim/stem_edge_add/mamm/mamm_30/output/mamm_30_sum_stats.rds")
mamm_30$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/mamm/mamm_30/mamm_30_brake_ages.rds")
mamm_30$taxon<-rep("mamm_30", dim(mamm_30)[1])
mamm_40<-readRDS("self_sim/stem_edge_add/mamm/mamm_40/output/mamm_40_sum_stats.rds")
mamm_40$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/mamm/mamm_40/mamm_40_brake_ages.rds")
mamm_40$taxon<-rep("mamm_40", dim(mamm_40)[1])
mamm_50<-readRDS("self_sim/stem_edge_add/mamm/mamm_50/output/mamm_50_sum_stats.rds")
mamm_50$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/mamm/mamm_50/mamm_50_brake_ages.rds")
mamm_50$taxon<-rep("mamm_50", dim(mamm_50)[1])

seed_10<-readRDS("self_sim/stem_edge_add/seed/seed_10/output/seed_10_sum_stats.rds")
seed_10$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/seed/seed_10/seed_10_brake_ages.rds")
seed_10$taxon<-rep("seed_10", dim(seed_10)[1])
seed_20<-readRDS("self_sim/stem_edge_add/seed/seed_20/output/seed_20_sum_stats.rds")
seed_20$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/seed/seed_20/seed_20_brake_ages.rds")
seed_20$taxon<-rep("seed_20", dim(seed_20)[1])
seed_30<-readRDS("self_sim/stem_edge_add/seed/seed_30/output/seed_30_sum_stats.rds")
seed_30$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/seed/seed_30/seed_30_brake_ages.rds")
seed_30$taxon<-rep("seed_30", dim(seed_30)[1])
seed_40<-readRDS("self_sim/stem_edge_add/seed/seed_40/output/seed_40_sum_stats.rds")
seed_40$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/seed/seed_40/seed_40_brake_ages.rds")
seed_40$taxon<-rep("seed_40", dim(seed_40)[1])
seed_50<-readRDS("self_sim/stem_edge_add/seed/seed_50/output/seed_50_sum_stats.rds")
seed_50$orig_brake_age_f<-readRDS("self_sim/stem_edge_add/seed/seed_50/seed_50_brake_ages.rds")
seed_50$taxon<-rep("seed_50", dim(seed_50)[1])

squa_5<-readRDS("self_sim/stem_edge_add/squa/squa_5_sum_stats.rds")
squa_5$taxon<-rep("squa_5", dim(squa_5)[1])
squa_10<-readRDS("self_sim/stem_edge_add/squa/squa_10_sum_stats.rds")
squa_10$taxon<-rep("squa_10", dim(squa_10)[1])
squa_20<-readRDS("self_sim/stem_edge_add/squa/squa_20_sum_stats.rds")
squa_20$taxon<-rep("squa_20", dim(squa_20)[1])
squa_30<-readRDS("self_sim/stem_edge_add/squa/squa_30_sum_stats.rds")
squa_30$taxon<-rep("squa_30", dim(squa_30)[1])
squa_40<-readRDS("self_sim/stem_edge_add/squa/squa_40_sum_stats.rds")
squa_40$taxon<-rep("squa_40", dim(squa_40)[1])
squa_50<-readRDS("self_sim/stem_edge_add/squa/squa_50_sum_stats.rds")
squa_50$taxon<-rep("squa_50", dim(squa_50)[1])

fract_sum_edg <- rbind(agar_5, agar_10, agar_20, agar_30, agar_40, agar_50 
                       ,amph_5, amph_10, amph_20, amph_30, amph_40, amph_50
                       ,bird_5, bird_10, bird_20, bird_30, bird_40, bird_50
                       ,chon_10, chon_20, chon_30, chon_40, chon_50
                       ,fern_5, fern_10, fern_20, fern_30, fern_40, fern_50
                       ,fish_5, fish_10, fish_20, fish_30, fish_40, fish_50
                       ,mamm_5, mamm_10, mamm_20, mamm_30, mamm_40, mamm_50
                       ,squa_5, squa_10, squa_20, squa_30, squa_40, squa_50
                       ,seed_10, seed_20, seed_30, seed_40, seed_50)

fract_sum_edg <- do.call(data.frame,lapply(fract_sum_edg, function(x) replace(x, is.infinite(x),NA)))
fract_sum_edg$s_age <- paste0("ages_", sapply(strsplit(fract_sum_edg$taxon, "_"), "[", 2))
fract_sum_edg$taxon <- sapply(strsplit(fract_sum_edg$taxon, "_"), "[", 1)

# node extracted trees
agar_5n<-readRDS("self_sim/node_extract/agar/agar_5/output/agar_5_sum_stats.rds")
agar_5n$taxon<-rep("agar_5", dim(agar_5n)[1])
agar_10n<-readRDS("self_sim/node_extract/agar/agar_10/output/agar_10_sum_stats.rds")
agar_10n$taxon<-rep("agar_10", dim(agar_10n)[1])
agar_20n<-readRDS("self_sim/node_extract/agar/agar_20/output/agar_20_sum_stats.rds")
agar_20n$taxon<-rep("agar_20", dim(agar_20n)[1])
agar_30n<-readRDS("self_sim/node_extract/agar/agar_30/output/agar_30_sum_stats.rds")
agar_30n$taxon<-rep("agar_30", dim(agar_30n)[1])
agar_40n<-readRDS("self_sim/node_extract/agar/agar_40/output/agar_40_sum_stats.rds")
agar_40n$taxon<-rep("agar_40", dim(agar_40n)[1])
agar_50n<-readRDS("self_sim/node_extract/agar/agar_50/output/agar_50_sum_stats.rds")
agar_50n$taxon<-rep("agar_50", dim(agar_50n)[1])

amph_5n<-readRDS("self_sim/node_extract/amph/amph_5/output/amph_5_sum_stats.rds")
amph_5n$taxon<-rep("amph_5", dim(amph_5n)[1])
amph_10n<-readRDS("self_sim/node_extract/amph/amph_10/output/amph_10_sum_stats.rds")
amph_10n$taxon<-rep("amph_10", dim(amph_10n)[1])
amph_20n<-readRDS("self_sim/node_extract/amph/amph_20/output/amph_20_sum_stats.rds")
amph_20n$taxon<-rep("amph_20", dim(amph_20n)[1])
amph_30n<-readRDS("self_sim/node_extract/amph/amph_30/output/amph_30_sum_stats.rds")
amph_30n$taxon<-rep("amph_30", dim(amph_30n)[1])
amph_40n<-readRDS("self_sim/node_extract/amph/amph_40/output/amph_40_sum_stats.rds")
amph_40n$taxon<-rep("amph_40", dim(amph_40n)[1])
amph_50n<-readRDS("self_sim/node_extract/amph/amph_50/output/amph_50_sum_stats.rds")
amph_50n$taxon<-rep("amph_50", dim(amph_50n)[1])

bird_5n<-readRDS("self_sim/node_extract/bird/bird_5/output/bird_5_sum_stats.rds")
bird_5n$taxon<-rep("bird_5", dim(bird_5n)[1])
bird_10n<-readRDS("self_sim/node_extract/bird/bird_10/output/bird_10_sum_stats.rds")
bird_10n$taxon<-rep("bird_10", dim(bird_10n)[1])
bird_20n<-readRDS("self_sim/node_extract/bird/bird_20/output/bird_20_sum_stats.rds")
bird_20n$taxon<-rep("bird_20", dim(bird_20n)[1])
bird_30n<-readRDS("self_sim/node_extract/bird/bird_30/output/bird_30_sum_stats.rds")
bird_30n$taxon<-rep("bird_30", dim(bird_30n)[1])
bird_40n<-readRDS("self_sim/node_extract/bird/bird_40/output/bird_40_sum_stats.rds")
bird_40n$taxon<-rep("bird_40", dim(bird_40n)[1])
bird_50n<-readRDS("self_sim/node_extract/bird/bird_50/output/bird_50_sum_stats.rds")
bird_50n$taxon<-rep("bird_50", dim(bird_50n)[1])

chon_5n<-readRDS("self_sim/node_extract/chon/chon_5/output/chon_5_sum_stats.rds")
chon_5n$taxon<-rep("chon_5", dim(chon_5n)[1])
chon_10n<-readRDS("self_sim/node_extract/chon/chon_10/output/chon_10_sum_stats.rds")
chon_10n$taxon<-rep("chon_10", dim(chon_10n)[1])
chon_20n<-readRDS("self_sim/node_extract/chon/chon_20/output/chon_20_sum_stats.rds")
chon_20n$taxon<-rep("chon_20", dim(chon_20n)[1])
chon_30n<-readRDS("self_sim/node_extract/chon/chon_30/output/chon_30_sum_stats.rds")
chon_30n$taxon<-rep("chon_30", dim(chon_30n)[1])
chon_40n<-readRDS("self_sim/node_extract/chon/chon_40/output/chon_40_sum_stats.rds")
chon_40n$taxon<-rep("chon_40", dim(chon_40n)[1])
chon_50n<-readRDS("self_sim/node_extract/chon/chon_50/output/chon_50_sum_stats.rds")
chon_50n$taxon<-rep("chon_50", dim(chon_50n)[1])

fern_5n<-readRDS("self_sim/node_extract/fern/fern_5/output/fern_5_sum_stats.rds")
fern_5n$taxon<-rep("fern_5", dim(fern_5n)[1])
fern_10n<-readRDS("self_sim/node_extract/fern/fern_10/output/fern_10_sum_stats.rds")
fern_10n$taxon<-rep("fern_10", dim(fern_10n)[1])
fern_20n<-readRDS("self_sim/node_extract/fern/fern_20/output/fern_20_sum_stats.rds")
fern_20n$taxon<-rep("fern_20", dim(fern_20n)[1])
fern_30n<-readRDS("self_sim/node_extract/fern/fern_30/output/fern_30_sum_stats.rds")
fern_30n$taxon<-rep("fern_30", dim(fern_30n)[1])
fern_40n<-readRDS("self_sim/node_extract/fern/fern_40/output/fern_40_sum_stats.rds")
fern_40n$taxon<-rep("fern_40", dim(fern_40n)[1])
fern_50n<-readRDS("self_sim/node_extract/fern/fern_50/output/fern_50_sum_stats.rds")
fern_50n$taxon<-rep("fern_50", dim(fern_50n)[1])

fish_5n<-readRDS("self_sim/node_extract/fish/fish_5/output/fish_5_sum_stats.rds")
fish_5n$taxon<-rep("fish_5", dim(fish_5n)[1])
fish_10n<-readRDS("self_sim/node_extract/fish/fish_10/output/fish_10_sum_stats.rds")
fish_10n$taxon<-rep("fish_10", dim(fish_10n)[1])
fish_20n<-readRDS("self_sim/node_extract/fish/fish_20/output/fish_20_sum_stats.rds")
fish_20n$taxon<-rep("fish_20", dim(fish_20n)[1])
fish_30n<-readRDS("self_sim/node_extract/fish/fish_30/output/fish_30_sum_stats.rds")
fish_30n$taxon<-rep("fish_30", dim(fish_30n)[1])
fish_40n<-readRDS("self_sim/node_extract/fish/fish_40/output/fish_40_sum_stats.rds")
fish_40n$taxon<-rep("fish_40", dim(fish_40n)[1])
fish_50n<-readRDS("self_sim/node_extract/fish/fish_50/output/fish_50_sum_stats.rds")
fish_50n$taxon<-rep("fish_50", dim(fish_50n)[1])

mamm_5n<-readRDS("self_sim/node_extract/mamm/mamm_5/output/mamm_5_sum_stats.rds")
mamm_5n$taxon<-rep("mamm_5", dim(mamm_5n)[1])
mamm_10n<-readRDS("self_sim/node_extract/mamm/mamm_10/output/mamm_10_sum_stats.rds")
mamm_10n$taxon<-rep("mamm_10", dim(mamm_10n)[1])
mamm_20n<-readRDS("self_sim/node_extract/mamm/mamm_20/output/mamm_20_sum_stats.rds")
mamm_20n$taxon<-rep("mamm_20", dim(mamm_20n)[1])
mamm_30n<-readRDS("self_sim/node_extract/mamm/mamm_30/output/mamm_30_sum_stats.rds")
mamm_30n$taxon<-rep("mamm_30", dim(mamm_30n)[1])
mamm_40n<-readRDS("self_sim/node_extract/mamm/mamm_40/output/mamm_40_sum_stats.rds")
mamm_40n$taxon<-rep("mamm_40", dim(mamm_40n)[1])
mamm_50n<-readRDS("self_sim/node_extract/mamm/mamm_50/output/mamm_50_sum_stats.rds")
mamm_50n$taxon<-rep("mamm_50", dim(mamm_50n)[1])

seed_10n<-readRDS("self_sim/node_extract/seed/seed_10/output/seed_10_sum_stats.rds")
seed_10n$taxon<-rep("seed_10", dim(seed_10n)[1])
seed_20n<-readRDS("self_sim/node_extract/seed/seed_20/output/seed_20_sum_stats.rds")
seed_20n$taxon<-rep("seed_20", dim(seed_20n)[1])
seed_30n<-readRDS("self_sim/node_extract/seed/seed_30/output/seed_30_sum_stats.rds")
seed_30n$taxon<-rep("seed_30", dim(seed_30n)[1])
seed_40n<-readRDS("self_sim/node_extract/seed/seed_40/output/seed_40_sum_stats.rds")
seed_40n$taxon<-rep("seed_40", dim(seed_40n)[1])
seed_50n<-readRDS("self_sim/node_extract/seed/seed_50/output/seed_50_sum_stats.rds")
seed_50n$taxon<-rep("seed_50", dim(seed_50n)[1])

squa_5n<-readRDS("self_sim/node_extract/squa/squa_5/output/squa_5_sum_stats.rds")
squa_5n$taxon<-rep("squa_5", dim(squa_5n)[1])
squa_10n<-readRDS("self_sim/node_extract/squa/squa_10/output/squa_10_sum_stats.rds")
squa_10n$taxon<-rep("squa_10", dim(squa_10n)[1])
squa_20n<-readRDS("self_sim/node_extract/squa/squa_20/output/squa_20_sum_stats.rds")
squa_20n$taxon<-rep("squa_20", dim(squa_20n)[1])
squa_30n<-readRDS("self_sim/node_extract/squa/squa_30/output/squa_30_sum_stats.rds")
squa_30n$taxon<-rep("squa_30", dim(squa_30n)[1])
squa_40n<-readRDS("self_sim/node_extract/squa/squa_40/output/squa_40_sum_stats.rds")
squa_40n$taxon<-rep("squa_40", dim(squa_40n)[1])
squa_50n<-readRDS("self_sim/node_extract/squa/squa_50/output/squa_50_sum_stats.rds")
squa_50n$taxon<-rep("squa_50", dim(squa_50n)[1])

fract_sum_node <- rbind(agar_5n, agar_10n, agar_20n, agar_30n, agar_40n, agar_50n,
                        amph_5n, amph_10n, amph_20n, amph_30n, amph_40n, amph_50n,
                        bird_5n, bird_10n, bird_20n, bird_30n, bird_40n, bird_50n,
                        chon_5n, chon_10n, chon_20n, chon_30n, chon_40n, chon_50n,
                        fern_5n, fern_10n, fern_20n, fern_30n, fern_40n, fern_50n,
                        fish_5n, fish_10n, fish_20n, fish_30n, fish_40n, fish_50n,
                        seed_10n, seed_20n, seed_30n, seed_40n, seed_50n,
                        mamm_10n, mamm_20n, mamm_30n, mamm_40n, mamm_50n,
                        squa_5n, squa_10n, squa_20n, squa_30n, squa_40n, squa_50n)
fract_sum_node <- do.call(data.frame,lapply(fract_sum_node, function(x) replace(x, is.infinite(x),NA)))
fract_sum_node$s_age <- paste0("ages_", sapply(strsplit(fract_sum_node$taxon, "_"), "[", 2))
fract_sum_node$taxon <- sapply(strsplit(fract_sum_node$taxon, "_"), "[", 1)

# Simulated trees from empirican bird parameters ####
sim_bird_5<-readRDS("self_sim/sim_bird/sim_bird_5_sum_stats.rds")
sim_bird_5$taxon<-rep("sim_bird_5", dim(sim_bird_5)[1])
sim_bird_10<-readRDS("self_sim/sim_bird/sim_bird_10_sum_stats.rds")
sim_bird_10$taxon<-rep("sim_bird_10", dim(sim_bird_10)[1])
sim_bird_20<-readRDS("self_sim/sim_bird/sim_bird_20_sum_stats.rds")
sim_bird_20$taxon<-rep("sim_bird_20", dim(sim_bird_20)[1])
sim_bird_30<-readRDS("self_sim/sim_bird/sim_bird_30_sum_stats.rds")
sim_bird_30$taxon<-rep("sim_bird_30", dim(sim_bird_30)[1])
sim_bird_40<-readRDS("self_sim/sim_bird/sim_bird_40_sum_stats.rds")
sim_bird_40$taxon<-rep("sim_bird_40", dim(sim_bird_40)[1])
sim_bird_50<-readRDS("self_sim/sim_bird/sim_bird_50_sum_stats.rds")
sim_bird_50$taxon<-rep("sim_bird_50", dim(sim_bird_50)[1])

sim_bird_st <- rbind(sim_bird_5, sim_bird_10, sim_bird_20, sim_bird_30, sim_bird_40, sim_bird_50)

###
##### POSTERIOR TREES
###

# 1K TREES ####

amph_1k_sum <- readRDS("1k_trees/output/amph_sum_stats.rds")
amph_1k_sum$taxon <- "amph"
bird_1k_sum <- readRDS("1k_trees/output/bird_hack_sum_stats.rds")
bird_1k_sum$taxon <- "bird"
mamm_1k_sum <- readRDS("1k_trees/output/mamm_sum_stats.rds")
mamm_1k_sum$taxon <- "mamm"
shar_1k_sum <- readRDS("1k_trees/output/shark_sum_stats.rds")
shar_1k_sum$taxon <- "shar"
#squa_1k_sum <- readRDS("1k_trees/output/squa_sum_stats.rds")
#squa_1k_sum$taxon <- "squa"

all_1k_sum <- rbind(amph_1k_sum, bird_1k_sum, mamm_1k_sum, shar_1k_sum)

# slicing on 10 random trees from posterior
all_10_slic  <- readRDS("rand_post_tree/slicing_samp/output/all_taxa_res.rds")
squa_10_slic <- readRDS("rand_post_tree/slicing_samp/output/squa_all_sum.rds")
all_10_slic  <- rbind(all_10_slic, squa_10_slic)
all_10_slic$taxon <- stringr::str_extract(all_10_slic$n_tree, "[^_]+")

# rank sampling on 10 random trees from posterior
load_res_rds=function(path, clade, file_2l){
  final_tab <- c()
  for(i in 1:length(dir(path)[grep(clade, dir(path))])){
    temp_tab <- readRDS(paste0(path, clade, i, file_2l))
    final_tab <- rbind(final_tab, temp_tab)
  }
  final_tab
}

amph_10_rank <- load_res_rds("rand_post_tree/rank_samp/output/", "amph", "_sum_stats.rds")
#bird_10_rank <- readRDS("rand_post_tree/rank_samp/output/")
mamm_10_rank <- load_res_rds("rand_post_tree/rank_samp/output/", "mamm", "_sum_stats.rds")
shar_10_rank <- load_res_rds("rand_post_tree/rank_samp/output/", "shark", "_sum_stats.rds")
squa_10_rank <- load_res_rds("rand_post_tree/rank_samp/output/", "squa", "_sum_stats.rds")

all_10_rank <- rbind(amph_10_rank, 
                     #bird_10_rank, 
                     mamm_10_rank, shar_10_rank, squa_10_rank)
all_10_rank$taxon <- stringr::str_extract(all_10_rank$n_tree, "[^_]+")

# edge sampling on 10 random trees from posterior
amph_10_rando <- readRDS("rand_post_tree/fract_samp/output/amph_sum.rds")
bird_10_rando <- readRDS("rand_post_tree/fract_samp/output/bird_erick_sum.rds")
mamm_10_rando <- readRDS("rand_post_tree/fract_samp/output/mamm_sum.rds")
shar_10_rando <- readRDS("rand_post_tree/fract_samp/output/shark_sum.rds")
squa_10_rando <- readRDS("rand_post_tree/fract_samp/output/squa_sum.rds")
all_10_rando <- rbind(amph_10_rando, bird_10_rando, mamm_10_rando, shar_10_rando, squa_10_rando)
all_10_rando$taxon <- stringr::str_extract(all_10_rando$n_tree, "[^_]+")
all_10_rando$s_age <- sapply(strsplit(all_10_rando$ht_smp, "_"), "[", 2)

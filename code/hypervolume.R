library(hypervolume)
library(tidyverse)

# Slicing ####
fish_vol<-filter(sum_stats, taxon == "Actinopterygii") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_vol<-filter(sum_stats, taxon == "Agaricomycetes") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_vol<-filter(sum_stats, taxon == "Amphibia") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_vol<-filter(sum_stats, taxon == "Aves") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_vol<-filter(sum_stats, taxon == "Chondrichthyes") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_vol<-filter(sum_stats, taxon == "Polypodiopsida") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

squa_vol<-filter(sum_stats, taxon == "Squamata") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

all_clades<-select(sum_stats, principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

slice_vol<-data.frame(fish_vol@Volume, agar_vol@Volume, amph_vol@Volume, bird_vol@Volume, chon_vol@Volume, fern_vol@Volume, squa_vol@Volume, all_clades@Volume)

# Rank sampling ####
agar_fams_vol<-filter(sum_stats, taxon == "agar_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_ords_vol<-filter(sum_stats, taxon == "agar_ords") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_fams_vol<-filter(sum_stats, taxon == "amph_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_ords_vol<-filter(sum_stats, taxon == "amph_ords") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_fams_vol<-filter(sum_stats, taxon == "bird_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_ords_vol<-filter(sum_stats, taxon == "bird_ords") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  drop_na() %>% expectation_convex(check.memory = F)

chon_fams_vol<-filter(sum_stats, taxon == "chon_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_ords_vol<-filter(sum_stats, taxon == "chon_ords") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_fams_vol<-filter(sum_stats, taxon == "fern_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_ords_vol<-filter(sum_stats, taxon == "fern_ords") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_fams_vol<-filter(sum_stats, taxon == "fish_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_ords_vol<-filter(sum_stats, taxon == "fish_ords") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  drop_na() %>% expectation_convex(check.memory = F)

squa_fams_vol<-filter(sum_stats, taxon == "squa_fams") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

ranks_vol<-data.frame(agar_fams_vol@Volume, agar_ords_vol@Volume, 
                      amph_fams_vol@Volume, #amph_ords_vol@Volume, 
                      bird_fams_vol@Volume, bird_ords_vol@Volume, 
                      chon_fams_vol@Volume, chon_ords_vol@Volume, 
                      fern_fams_vol@Volume, fern_ords_vol@Volume, 
                      fish_fams_vol@Volume ,fish_ords_vol@Volume, 
                      squa_fams_vol@Volume)

all_ranks<-select(sum_stats,principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  drop_na() %>% expectation_convex(check.memory = F)
  
plot(all_ranks)
plot(all_ranks, show.3d = T)

# Self-similar ####
bird_5_vol<-filter(sum_stats, taxon == "bird_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_10_vol<-filter(sum_stats, taxon == "bird_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_20_vol<-filter(sum_stats, taxon == "bird_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_30_vol<-filter(sum_stats, taxon == "bird_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

bird_40_vol<-filter(sum_stats, taxon == "bird_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

bird_50_vol<-filter(sum_stats, taxon == "bird_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_5_vol<-filter(sum_stats, taxon == "agar_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_10_vol<-filter(sum_stats, taxon == "agar_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_20_vol<-filter(sum_stats, taxon == "agar_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_30_vol<-filter(sum_stats, taxon == "agar_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

agar_40_vol<-filter(sum_stats, taxon == "agar_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

agar_50_vol<-filter(sum_stats, taxon == "agar_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_5_vol<-filter(sum_stats, taxon == "amph_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_10_vol<-filter(sum_stats, taxon == "amph_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_20_vol<-filter(sum_stats, taxon == "amph_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_30_vol<-filter(sum_stats, taxon == "amph_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

amph_40_vol<-filter(sum_stats, taxon == "amph_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

amph_50_vol<-filter(sum_stats, taxon == "amph_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_5_vol<-filter(sum_stats, taxon == "chon_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_10_vol<-filter(sum_stats, taxon == "chon_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_20_vol<-filter(sum_stats, taxon == "chon_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_30_vol<-filter(sum_stats, taxon == "chon_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

chon_40_vol<-filter(sum_stats, taxon == "chon_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

chon_50_vol<-filter(sum_stats, taxon == "chon_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_5_vol<-filter(sum_stats, taxon == "fern_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_10_vol<-filter(sum_stats, taxon == "fern_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_20_vol<-filter(sum_stats, taxon == "fern_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_30_vol<-filter(sum_stats, taxon == "fern_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fern_40_vol<-filter(sum_stats, taxon == "fern_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

fern_50_vol<-filter(sum_stats, taxon == "fern_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_5_vol<-filter(sum_stats, taxon == "fish_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_10_vol<-filter(sum_stats, taxon == "fish_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_20_vol<-filter(sum_stats, taxon == "fish_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_30_vol<-filter(sum_stats, taxon == "fish_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

fish_40_vol<-filter(sum_stats, taxon == "fish_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

fish_50_vol<-filter(sum_stats, taxon == "fish_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

squa_5_vol<-filter(sum_stats, taxon == "squa_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

squa_10_vol<-filter(sum_stats, taxon == "squa_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

squa_20_vol<-filter(sum_stats, taxon == "squa_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

squa_30_vol<-filter(sum_stats, taxon == "squa_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

squa_40_vol<-filter(sum_stats, taxon == "squa_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

squa_50_vol<-filter(sum_stats, taxon == "squa_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

all_samp_vol<-select(sum_stats, principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

rand_samp_vol<-data.frame( bird_5_vol@Volume, bird_10_vol@Volume, bird_20_vol@Volume, bird_30_vol@Volume, bird_40_vol@Volume, bird_50_vol@Volume
                          ,agar_5_vol@Volume, agar_10_vol@Volume, agar_20_vol@Volume, agar_30_vol@Volume, agar_40_vol@Volume, agar_50_vol@Volume
                          ,amph_5_vol@Volume, amph_10_vol@Volume, amph_20_vol@Volume, amph_30_vol@Volume, amph_40_vol@Volume, amph_50_vol@Volume
                          #,chon_5_vol@Volume
                          , chon_10_vol@Volume, chon_20_vol@Volume, chon_30_vol@Volume, chon_40_vol@Volume, chon_50_vol@Volume
                          ,fern_5_vol@Volume, fern_10_vol@Volume, fern_20_vol@Volume, fern_30_vol@Volume, fern_40_vol@Volume, fern_50_vol@Volume
                          ,fish_5_vol@Volume, fish_10_vol@Volume, fish_20_vol@Volume, fish_30_vol@Volume, fish_40_vol@Volume, fish_50_vol@Volume
                          ,squa_5_vol@Volume, squa_10_vol@Volume, squa_20_vol@Volume, squa_30_vol@Volume, squa_40_vol@Volume, squa_50_vol@Volume
                          )

plot(all_samp_vol)
plot(all_samp_vol, show.3d = T)

# simulated trees usign empirical parameters from birds
sim_bird_5_vol<-filter(sum_stats, taxon == "sim_bird_5") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

sim_bird_10_vol<-filter(sum_stats, taxon == "sim_bird_10") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

sim_bird_20_vol<-filter(sum_stats, taxon == "sim_bird_20") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

sim_bird_30_vol<-filter(sum_stats, taxon == "sim_bird_30") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

sim_bird_40_vol<-filter(sum_stats, taxon == "sim_bird_40") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)  

sim_bird_50_vol<-filter(sum_stats, taxon == "sim_bird_50") %>% 
  select(principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

sim_bird_vol<-data.frame(sim_bird_5_vol@Volume, sim_bird_10_vol@Volume, sim_bird_20_vol@Volume, sim_bird_30_vol@Volume, sim_bird_40_vol@Volume, sim_bird_50_vol@Volume)



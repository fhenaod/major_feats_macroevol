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

slice_vol<-data.frame(fish=fish_vol@Volume, agar=agar_vol@Volume, amph=amph_vol@Volume, bird=bird_vol@Volume, chon=chon_vol@Volume, fern=fern_vol@Volume, squa=squa_vol@Volume)

all_clades<-select(sum_stats, principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)
all_clades@Volume

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

ranks_vol<-data.frame(agar_fams = agar_fams_vol@Volume, agar_ords = agar_ords_vol@Volume, 
                      amph_fams = amph_fams_vol@Volume, #amph_ords = amph_ords_vol@Volume, 
                      bird_fams = bird_fams_vol@Volume, bird_orders = bird_ords_vol@Volume, 
                      chon_fams = chon_fams_vol@Volume, chon_ords = chon_ords_vol@Volume, 
                      fern_fams = fern_fams_vol@Volume, fern_ords = fern_ords_vol@Volume, 
                      fish_fams = fish_fams_vol@Volume, fish_ords = fish_ords_vol@Volume, 
                      squa_fams = squa_fams_vol@Volume)

all_ranks<-select(sum_stats,principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  drop_na() %>% expectation_convex(check.memory = F)

all_ranks@Volume
  
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

rand_samp_vol<-rbind( bird_5 = bird_5_vol@Volume, bird_10 = bird_10_vol@Volume, bird_20 = bird_20_vol@Volume, bird_30 = bird_30_vol@Volume, bird_40 = bird_40_vol@Volume, bird_50 = bird_50_vol@Volume
                          ,agar_5 = agar_5_vol@Volume, agar_10 = agar_10_vol@Volume, agar_20 = agar_20_vol@Volume, agar_30 = agar_30_vol@Volume, agar_40 = agar_40_vol@Volume, agar_50 = agar_50_vol@Volume
                          ,amph_5 = amph_5_vol@Volume, amph_10 = amph_10_vol@Volume, amph_20 = amph_20_vol@Volume, amph_30 = amph_30_vol@Volume, amph_40 = amph_40_vol@Volume, amph_50 = amph_50_vol@Volume
                          #, chon_5 = chon_5_vol@Volume
                          ,chon_5 = NA ,chon_10 = chon_10_vol@Volume, chon_20 = chon_20_vol@Volume, chon_30 = chon_30_vol@Volume, chon_40 = chon_40_vol@Volume, chon_50 = chon_50_vol@Volume
                          ,fern_5 = fern_5_vol@Volume, fern_10 = fern_10_vol@Volume, fern_20 = fern_20_vol@Volume, fern_30 = fern_30_vol@Volume, fern_40 = fern_40_vol@Volume, fern_50 = fern_50_vol@Volume
                          ,fish_5 = fish_5_vol@Volume, fish_10 = fish_10_vol@Volume, fish_20 = fish_20_vol@Volume, fish_30 = fish_30_vol@Volume, fish_40 = fish_40_vol@Volume, fish_50 = fish_50_vol@Volume
                          ,squa_5 = squa_5_vol@Volume, squa_10 = squa_10_vol@Volume, squa_20 = squa_20_vol@Volume, squa_30 = squa_30_vol@Volume, squa_40 = squa_40_vol@Volume, squa_50 = squa_50_vol@Volume
                          )

all_samp_vol<-select(sum_stats, principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

all_samp_vol@Volume

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

sim_bird_vol<-data.frame(sim_bird_5 = sim_bird_5_vol@Volume, 
                         sim_bird_10 = sim_bird_10_vol@Volume, 
                         sim_bird_20 = sim_bird_20_vol@Volume, 
                         sim_bird_30 = sim_bird_30_vol@Volume, 
                         sim_bird_40 = sim_bird_40_vol@Volume, 
                         sim_bird_50 = sim_bird_50_vol@Volume)

all_sim_vol<-select(sum_stats, principal_eigenvalue, asymmetry, peakedness) %>% 
  mutate(principal_eigenvalue = log(principal_eigenvalue),asymmetry = asymmetry, peakedness = log(peakedness)) %>% 
  expectation_convex(check.memory = F)

all_sim_vol@Volume

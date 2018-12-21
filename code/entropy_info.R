library(infotheo)
disc_sum<-discretize(sum_stats_f)

# Dataset entropy 
H<-entropy(disc_sum,method="emp")
natstobits(H) # Transformation to bits

# Interaction info (synergy or complementarity)
Hi<-interinformation(disc_sum,method="emp")
natstobits(Hi) # Transformation to bits

# Multi-info (total correlation)
Hm<-multiinformation(disc_sum,method="emp")
natstobits(Hm) # Transformation to bits

# Conditional mutual info 
Hu<-condinformation(disc_sum$ln_dr,disc_sum$asymmetry, disc_sum$tree.max.age, method="emp")
natstobits(Hu) # Transformation to bits

# Conditional entropy
Hc<-condentropy(disc_sum$ln_dr, disc_sum$tree.max.age, method = "emp")
natstobits(Hc)


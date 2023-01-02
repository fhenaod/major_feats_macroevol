library(tidyverse)
library(base)
library(plot3D)
library(animation)
library(RColorBrewer)

# dataframes ####
               # consensus trees
sl_sum_stats   # time-sampling consensus trees
jnk_str_stats  # time-sampling consensus trees Sturges-Jenks intervals
rank_sum_stats # rank sampling consensus trees
fract_sum_node # random nodes consensus trees
fract_sum_edg  # random edges consensus trees
rank_rdm_sum %>% group_by(taxon) %>% 
  sample_n(1000, replace = F)  # approximate-age-rank random trees

all_1k_sum    # 1k posterior trees
all_10_slic   # sliced 10 posterior trees
sim_sl_stats  # sliced simulated trees
all_10_rank   # rank sampled 10 posterior trees
all_10_rando %>% 
  group_by(s_age) %>% 
  sample_n(200, replace = F) # self-similar sampled 10 posterior trees

# data frame to plot
df2plot <- sim_sl_stats %>% filter(ntips > 20) #%>% filter(taxon != 'Spermatophyta')
sufx <- "sim_slic" 

# plot pars ####
# addalpha()
addalpha <- function(colors, alpha = 1.0) {
  r <- col2rgb(colors, alpha = T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# colorRampPaletteAlpha()
colorRampPaletteAlpha <- function(colors, n = 32, interpolate = 'linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha = T)[4,]
  # Interpolate
  if (interpolate == 'linear') {
    l <- approx(a, n = n)
  } else {
    l <- spline(a, n = n)
  }
  l$y[l$y > 255] <- 255 # Clamp if spline is > 255
  cr <- addalpha(cr, l$y/255.0)
  return(cr)
}

par(xpd = TRUE)
cols <- colorRampPalette(brewer.pal(11,"Spectral"))(100)
cols <- c("#003f5c", "#2f4b7c", "#665191", 
         "#a05195", "#d45087", "#f95d6a", 
         "#ff7c43", "#ffa600", "#e3e418ff")
#cols <- wesanderson::wes_palette("FantasticFox1", n = 9, type = "continuous")
pal <- addalpha(cols, .75)

scales::show_col(pal)

# rpanda ####
rotate <- 360
saveGIF({
  while (rotate>0)
  {
    rotate <- rotate-2 # change to 1 and run locally for rotation animation 
    
    scatter3D(log(df2plot$principal_eigenvalue), 
              df2plot$asymmetry, 
              log(df2plot$peakedness),  
              axes = T,
              #nticks = 5, ticktype = "detailed", 
              colvar = df2plot$tree.max.age,
              #colvar = df2plot$rel_age,
              #colvar = df2plot$or_brake_age_f,
              #colvar = df2plot$orig_brake_age_f,
              col = pal,
              xlab = "Expansion", 
              ylab = "Tippiness",
              zlab = "Branch length heterogeneity",
              clab = "Clade age (Myr)",
              #clab = "Relative age",
              #clab = "Breaking age",
              theta = rotate, phi = 30,
              #pch = as.numeric(factor(df2plot$taxon))+10, # slice, rank, node 
              pch = as.numeric(factor(df2plot$n_tree)), # sim_slic
              #pch = as.numeric(factor(df2plot$taxon))+12, #rdm_rank
              #pch = as.numeric(factor(df2plot$taxon))+14, #1k, 10_slic
              #cex = log(df2plot$ntips)/3, # slice, rank, node
              #cex = log(df2plot$ntips)/4, # rdm_rank, 1k, 10_slic
              cex = log(df2plot$ntips)/5, # sim_slic
              #bty = "b2",
              #type = "h",
              #ticktype = "detailed", tick numbers
              add = FALSE)
  }
}, interval = 0.2, movie.name = paste0("res_rpanda_",sufx,".gif"), ani.width = 800, ani.height = 600)

# Save ggplot at high resolution
ggsave("Rplot.png", width = 20, height = 20, units = "cm", dpi = 320)

# shape stats ####
# Yule
rotate <- 360
saveGIF({
  
  while (rotate>0)
  {
    rotate <- rotate-2 # change to 1 and run locally for rotation animation 
    
    scatter3D((df2plot$shape.yule), 
              df2plot$colles.yule, 
              (df2plot$sackin.yule),  
              colvar = df2plot$tree.max.age,
              #colvar = df2plot$rel_age,
              col = pal,
              xlab = "Shape (Yule)", 
              ylab = "Colless (Yule)",
              zlab = "Sackin (Yule)",
              clab = "Clade age (Myr)",
              #clab = "Relative age",
              theta = rotate,
              phi = 30,
              #pch = as.numeric(factor(df2plot$taxon))+10, # slice, rank, node
              #pch = as.numeric(factor(df2plot$taxon))+12, # rdm_rank
              #pch = as.numeric(factor(df2plot$taxon))+14, #1k,  10_slic
              pch = as.numeric(factor(df2plot$n_tree)), # sim_slic
              #cex = log(df2plot$ntips)/3, # slice, rank, node
              cex = log(df2plot$ntips)/4, # rdm_rank,  10_slic
              bty = "b2",
              add = FALSE)
  }
}, interval = 0.2, movie.name = paste0("res_yule_", sufx,".gif"), ani.width = 800, ani.height = 600)

# PDA
rotate <- 360
saveGIF({
  
  while (rotate>0)
  {
    rotate <- rotate-2 # change to 1 and run locally for rotation animation 
    
    scatter3D((df2plot$shape.pda), 
              df2plot$colles.pda, 
              (df2plot$sackin.pda),  
              colvar = df2plot$tree.max.age,
              #colvar = df2plot$rel_age,
              col = pal,
              xlab = "Shape (PDA)", 
              ylab = "Colless (PDA)",
              zlab = "Sackin (PDA)",
              clab = "Clade age (Myr)",
              #clab = "Relative age",
              theta = rotate,
              phi = 30,
              #pch = as.numeric(factor(df2plot$taxon))+10, # slice, rank, node
              #pch = as.numeric(factor(df2plot$taxon))+12, # rdm_rank
              #pch = as.numeric(factor(df2plot$taxon))+14, # 1k, 10_slic
              pch = as.numeric(factor(df2plot$n_tree)), # sim_slic
              #cex = log(df2plot$ntips)/3, # slice, rank, node
              cex = log(df2plot$ntips)/4, # rdm_rank, 1k, 10_slic
              bty = "b2",
              add = FALSE)
  }
}, interval = 0.2, movie.name = paste0("res_pda_",sufx,".gif"), ani.width = 800, ani.height = 600)

library(ggplot2)
library(base)
library(plot3D)
library(animation)
library(RColorBrewer)

# dataframes ####
sl_sum_stats   # sliced concensus trees
rank_sum_stats # rank sampling consensus trees
fract_sum_node # self-similar sampling by nodes consensus trees
fract_sum_edg %>% group_by(s_age) %>% sample_n(200, replace = F) # self-similar sampling by edges consensus trees

all_1k_sum  # 1k posterior trees
all_10_slic # sliced 10 posterior trees
all_10_rank # rank sampled 10 posterior trees
all_10_rando %>% group_by(s_age) %>% sample_n(200, replace = F) # self-similar sampled 10 posterior trees

# data frame to plot
df2plot <- all_10_rando %>% group_by(s_age) %>% sample_n(200, replace = F)

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
colorRampPaletteAlpha <- function(colors, n = 32, interpolate='linear') {
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
pal <- addalpha(cols, .6)

# rpanda ####
rotate <-360
saveGIF({
  while (rotate>0)
  {
    rotate<-rotate-2 # change to 1 and run locally for rotation animation 
    
    scatter3D(log(df2plot$principal_eigenvalue), 
              df2plot$asymmetry, 
              log(df2plot$peakedness),  
              colvar = df2plot$tree.max.age,
              #colvar = df2plot$rel_age,
              #colvar = df2plot$orig_brake_age_f,
              col = pal,
              xlab = "Expansion", 
              ylab = "Tippiness",
              zlab = "Branch length heterogeneity",
              clab = "Clade age (Myr)",
              #clab = "Relative age",
              #clab = "Breaking age",
              theta = rotate,
              phi = 30,
              #pch = as.numeric(factor(df2plot$taxon))+10,
              pch = as.numeric(factor(df2plot$taxon))+12,
              #pch = as.numeric(factor(df2plot$taxon))+14,
              cex = log(df2plot$ntips)/2,
              bty = "b2",
              add = FALSE)
  }
}, interval = 0.2, movie.name = "all_10post_edge_rpanda.gif", ani.width = 800, ani.height = 600)

# Save ggplot at high resolution
ggsave("Rplot.png", width = 20, height = 20, units = "cm", dpi = 320)

# shape stats ####
# Yule
rotate <-360
saveGIF({
  
  while (rotate>0)
  {
    rotate<-rotate-2 # change to 1 and run locally for rotation animation 
    
    scatter3D(log(df2plot$shape.yule), 
              df2plot$colles.yule, 
              log(df2plot$sackin.yule),  
              #colvar = df2plot$tree.max.age,
              colvar = df2plot$rel_age,
              col = pal,
              xlab = "Shape (Yule)", 
              ylab = "Colless (Yule)",
              zlab = "Sackin (Yule)",
              clab = "Clade age (Myr)",
              #clab = "Relative age",
              theta = rotate,
              phi = 30,
              pch = as.numeric(factor(df2plot$taxon))+10,
              #pch = as.numeric(factor(df2plot$taxon))+14,
              cex = log(df2plot$ntips)/2,
              bty = "b2",
              add = FALSE)
  }
}, interval = 0.2, movie.name = "all_10post_rank_yule.gif", ani.width = 800, ani.height = 600)

# PDA
rotate <-360
saveGIF({
  
  while (rotate>0)
  {
    rotate<-rotate-2 # change to 1 and run locally for rotation animation 
    
    scatter3D(log(df2plot$shape.pda), 
              df2plot$colles.pda, 
              log(df2plot$sackin.pda),  
              #colvar = df2plot$tree.max.age,
              colvar = df2plot$rel_age,
              col = pal,
              xlab = "Shape (PDA)", 
              ylab = "Colless (PDA)",
              zlab = "Sackin (PDA)",
              clab = "Clade age (Myr)",
              #clab = "Relative age",
              theta = rotate,
              phi = 30,
              pch = as.numeric(factor(df2plot$taxon))+10,
              #pch = as.numeric(factor(df2plot$taxon))+14,
              cex = log(df2plot$ntips)/2,
              bty = "b2",
              add = FALSE)
  }
}, interval = 0.2, movie.name = "all_10post_rank_pda.gif", ani.width = 800, ani.height = 600)

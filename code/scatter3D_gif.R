library(ggplot2)
library(base)
library(plot3D)
library(animation)
library(RColorBrewer)

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

rotate <-360
par(xpd = TRUE)
cols<-colorRampPalette(brewer.pal(11,"Spectral"))(100)
pal<-addalpha(cols,.5)

saveGIF({
  
  while (rotate>0)
  {
    rotate<-rotate-5 # change to 1 and run locally for rotation animation 
    
    scatter3D(log(sum_stats$principal_eigenvalue), 
              sum_stats$asymmetry, 
              log(sum_stats$peakedness),  
              colvar = sum_stats$tree.max.age,
              col = pal,
              xlab = "Expansion", 
              ylab = "Tippiness",
              zlab = "Branch length heterogeneity",
              clab = "Clade age (Myr)",
              theta = rotate,
              phi = 30,
              pch = 16,
              cex = log(sum_stats$ntips),
              bty = "b2",
              add = FALSE)
  }
}, interval = 0.1, movie.name = "slicing_3d.gif", ani.width = 800, ani.height = 600)

# Save ggplot at high resolution
ggsave("Rplot.png", width = 20, height = 20, units = "cm", dpi = 320)

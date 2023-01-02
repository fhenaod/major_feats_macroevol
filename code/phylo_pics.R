library(tidyverse)
library(ggimage)
library(ggthemes)
library(png)
library(grid)
library(grImport2)
library(rsvg)

# load phylopics ####
path <- "images/"
files <- dir(path)
tax_pics <- list()
for(i in 1:length(files)){
  if(fl_nam[[i]][2]=="png"){
    img <- readPNG(paste0(path, files[i]))
    g_pic <- rasterGrob(img, interpolate = TRUE)
    tax_pics[[i]] <- g_pic
  } else {
    img <- image_read2(paste0(path, files[i]))
    g_pic <- rasterGrob(img, interpolate = TRUE)
    tax_pics[[i]] <- g_pic
  }
}
names(tax_pics) <- c("Amphibia", "Aves", "Chondrichthyes", 
                     "Polypodiopsida", "Actinopterygii", "Agaricomycetes", 
                      "Mammalia", "Spermatophyta" ,"Squamata")

# check images and dimensions 
data.frame(x = 1:100, y = 1:100) %>% ggplot(aes(x, y)) + 
  geom_rangeframe() + theme_classic() +
  annotation_custom(tax_pics[[1]], xmin = 1,  xmax = 20, ymin = 75, ymax = 100) + 
  annotation_custom(tax_pics[[2]], xmin = 30, xmax = 50, ymin = 75, ymax = 100) + 
  annotation_custom(tax_pics[[3]], xmin = 60, xmax = 85, ymin = 75, ymax = 100) + 
  
  annotation_custom(tax_pics[[4]], xmin = 1,  xmax = 20, ymin = 45, ymax = 70) +
  annotation_custom(tax_pics[[5]], xmin = 30, xmax = 50, ymin = 45, ymax = 70) + 
  annotation_custom(tax_pics[[6]], xmin = 60, xmax = 85, ymin = 45, ymax = 70) +
  
  annotation_custom(tax_pics[[7]], xmin = 1,  xmax = 20, ymin = 10, ymax = 35) +
  annotation_custom(tax_pics[[8]], xmin = 30, xmax = 50, ymin = 10, ymax = 35) + 
  annotation_custom(tax_pics[[9]], xmin = 60, xmax = 85, ymin = 10, ymax = 35)

# load icons ####
path <- "iconos_low_white/"
files <- dir(path)
tax_pics <- list()
for(i in 1:length(files)){
  if(fl_nam[[i]][2]=="png"){
    img <- readPNG(paste0(path, files[i]))
    g_pic <- rasterGrob(img, interpolate = TRUE)
    tax_pics[[i]] <- g_pic
  } else {
    img <- image_read2(paste0(path, files[i]))
    g_pic <- rasterGrob(img, interpolate = TRUE)
    tax_pics[[i]] <- g_pic
  }
}
names(tax_pics) <- c("Actinopterygii", "Agaricomycetes", "Amphibia", "Aves", 
                     "Chondrichthyes", "Mammalia", "Polypodiopsida", 
                      "Spermatophyta" ,"Squamata")

# check images and dimensions 
data.frame(x = 1:100, y = 1:100) %>% 
  ggplot(aes(x, y)) + 
  geom_rangeframe() + theme_classic() +
  annotation_custom(tax_pics[[1]], xmin = 1,  xmax = 20, ymin = 75, ymax = 100) + 
  annotation_custom(tax_pics[[2]], xmin = 30, xmax = 50, ymin = 75, ymax = 100) + 
  annotation_custom(tax_pics[[3]], xmin = 60, xmax = 85, ymin = 75, ymax = 100) + 
  
  annotation_custom(tax_pics[[4]], xmin = 1,  xmax = 20, ymin = 45, ymax = 70) +
  annotation_custom(tax_pics[[5]], xmin = 30, xmax = 50, ymin = 45, ymax = 70) + 
  annotation_custom(tax_pics[[6]], xmin = 60, xmax = 85, ymin = 45, ymax = 70) +
  
  annotation_custom(tax_pics[[7]], xmin = 1,  xmax = 20, ymin = 10, ymax = 35) +
  annotation_custom(tax_pics[[8]], xmin = 30, xmax = 50, ymin = 10, ymax = 35) + 
  annotation_custom(tax_pics[[9]], xmin = 60, xmax = 85, ymin = 10, ymax = 35)


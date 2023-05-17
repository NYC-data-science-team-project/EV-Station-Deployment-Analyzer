library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(dplyr)
library(rayrender)

#installing rayshader direct from github as CRAN was outdated.
#install.packages("devtools")
#devtools::install_github("tylermorganwall/rayshader")

# load geopackage data

data <- st_read("C:/Users/kanem/Documents/Pop_Mapping/data/kontur_population_US_20220630.gpkg")

# load states 

st <- states()

# filter for CO

colorado <- st %>% 
  filter(NAME == "Colorado") %>% 
  st_transform(crs = st_crs(data))

#plot test

colorado %>% 
  ggplot()+
  geom_sf()

#filter and limit data for points covered in OR

st_colorado <- st_intersection(data,colorado)

#create aspect ratio with help from bb

bb <- st_bbox(st_colorado)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>% 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]],bb[["ymin"]])) %>% 
  st_sfc(crs = st_crs(data))

#Testing Plot
colorado %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data = bottom_left)+
  geom_sf(data = bottom_right, color = "red" )

#Determing height and width
width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>% 
  st_sfc(crs = st_crs(data))
height <- st_distance(bottom_left, top_left)

if (width > height) {
  w_ratio <- 1
  h_ratio <- height/width
} else {
  h_ratio <- 1
  w_ratio <-width/height
}
#convert to raster then matrix
size = 1000
colorado_rast <- st_rasterize(st_colorado,
                            nx = floor(size * w_ratio),
                            ny = floor(size * h_ratio)) 
mat <- matrix(colorado_rast$population,
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))


# Color Palette

hg <- met.brewer("Pillement")
swatchplot(hg)

texture <- grDevices::colorRampPalette(hg, bias = 1)(256)
swatchplot(texture)

#Plot 3d render

mat %>% 
  height_shade(texture = texture) %>% 
  plot_3d(heightmap = mat,
          zscale = 40,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = 20, phi = 22, zoom = .8)

#Output new high res png
render_highquality(
  filename = "images/test_plot2.png",
  interactive = FALSE,
  lightdirection = 300,
  lightaltitude = c(20,80),
  lightcolor = c(hg[2],"black"),
  lightintensity = c(1200,200),
  
  
)

#Close rgl
rgl::rgl.close()
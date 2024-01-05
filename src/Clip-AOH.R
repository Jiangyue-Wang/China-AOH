### load library----
library(tidyverse)
library(sf)
library(terra)
library(raster)
### read in files----
# China boundary &  polygonize
Behrmann <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
China <- st_read("E:/Jiangyue/SCP/layer/国界线.shp") %>% st_transform(Behrmann) %>% st_union() %>%st_polygonize() %>% st_collection_extract(
  type = c("POLYGON")
)

# Species data table and raster layers
amphibian <- "E:/Priority program/WJY/AOH/amphibian/"
amphi <- readRDS("E:/Priority program/WJY/AOH/amphibian.rds")
reptile <- "E:/Priority program/WJY/AOH/reptile/"
reptl <- readRDS("E:/Priority program/WJY/AOH/reptile.rds")
bird <- "E:/Priority program/WJY/AOH/bird_merge/"
br <- bind_rows(readRDS("E:/Priority program/WJY/AOH/bird_p1.rds"),readRDS("E:/Priority program/WJY/AOH/bird_p2.rds"))
mammal <- "E:/Priority program/WJY/AOH/mammal/"
mamal <- readRDS("E:/Priority program/WJY/AOH/mammal.rds")

### clip by China boundary, one by one----
# amphibian
for(i in 1: length(dir(amphibian))){
  path <- amphibian
  file <- dir(amphibian)[i]
  layer <- rast(paste0(path, file))
  polygon <- amphi %>% filter(path == paste0("/home/jhanson/aoh-data/AMPHIBIANS/", file)) %>% st_geometry()
  layer_c <- try(layer %>% terra::crop(China) %>% terra::mask(vect(China)))
  if(inherits(layer_c, "try-error")){next}
  writeRaster(layer_c, paste0("AOH-layers/Amphibians/",file))
  rm(path, file, layer, polygon, layer_c)
  gc()
  print(i)
}

# reptile
for(i in 1: length(dir(reptile))){
  path <- reptile
  file <- dir(reptile)[i]
  layer <- rast(paste0(path, file))
  polygon <- reptl %>% filter(path == paste0("/home/jhanson/aoh-data/REPTILES/", file)) %>% st_geometry()
  layer_c <- try(layer %>% terra::crop(China) %>% terra::mask(vect(China)))
  if(inherits(layer_c, "try-error")){next}
  writeRaster(layer_c, paste0("AOH-layers/Reptiles/",file))
  rm(path, file, layer, polygon, layer_c)
  gc()
  print(i)
}

# bird
for(i in 1: length(dir(bird))){
  path <- bird
  file <- dir(bird)[i]
  layer <- rast(paste0(path, file))
  polygon <- br %>% filter(path == paste0("/home/jhanson/frc-data/BOTW/", file)) %>% st_geometry()
  layer_c <- try(layer %>% terra::crop(China) %>% terra::mask(vect(China)))
  if(inherits(layer_c, "try-error")){next}
  writeRaster(layer_c, paste0("AOH-layers/Birds/",file))
  rm(path, file, layer, polygon, layer_c)
  gc()
  print(i)
}

# mammal
for(i in 1: length(dir(mammal))){
  path <- mammal
  file <- dir(mammal)[i]
  layer <- rast(paste0(path, file))
  polygon <- mamal %>% filter(path == paste0("/home/jhanson/aoh-data/MAMMALS_TERRESTRIAL_ONLY/", file)) %>% st_geometry()
  layer_c <- try(layer %>% terra::crop(China) %>% terra::mask(vect(China)))
  if(inherits(layer_c, "try-error")){next}
  writeRaster(layer_c, paste0("AOH-layers/Mammals/",file))
  rm(path, file, layer, polygon, layer_c)
  gc()
  print(i)
}

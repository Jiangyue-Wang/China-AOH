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
China_1km <- rast(vect(China),res=1000)
China1km <- rasterize(vect(China), China_1km)
China_5km <- rast(vect(China),res=5000)
China5km <- rasterize(vect(China), China_5km)
China_10km <- rast(vect(China),res=10000)
China10km <- rasterize(vect(China), China_10km)
CN_grid_1km <- terra::xyFromCell(China1km,1:ncell(China1km)) %>% as_tibble()
CN_grid_5km <- terra::xyFromCell(China5km,1:ncell(China5km))%>% as_tibble()
CN_grid_10km <- terra::xyFromCell(China10km,1:ncell(China10km))%>% as_tibble()

# read in rasters and make richness map

amphibian <- dir("AOH-layers/Amphibians/")
reptile <- dir("AOH-layers/Reptiles/")
bird <- dir("AOH-layers/Birds/")
mammal <- dir("AOH-layers/Mammals/")

# amphibian
amphi_1km <- rep(0,nrow(CN_grid_1km))
amphi_5km <- rep(0,nrow(CN_grid_5km))
amphi_10km <- rep(0,nrow(CN_grid_10km))
for(i in 1:length(amphibian)){
  tmp <- rast(paste0("AOH-layers/Amphibians/", amphibian[i]))
  values(tmp)[values(tmp)>0] <- 1
  values(tmp)[is.na(values(tmp))] <- 0
  if(sum(values(tmp))>0){
    # writeRaster(tmp, paste0("AOH/Amphibians/", amphibian[i]))
    tmp_1km <- resample(tmp, China1km)
    tmp_5km <- resample(tmp, China5km)
    tmp_10km <- resample(tmp, China10km)
    values(tmp_1km)[is.na(values(tmp_1km))] <- 0
    values(tmp_5km)[is.na(values(tmp_5km))] <- 0
    values(tmp_10km)[is.na(values(tmp_10km))] <- 0
    values(tmp_1km)[(values(tmp_1km))>0] <- 1
    values(tmp_5km)[(values(tmp_5km))>0] <- 1
    values(tmp_10km)[(values(tmp_10km))>0] <- 1
    amphi_1km <- amphi_1km + values(tmp_1km)
    amphi_5km <- amphi_5km + values(tmp_5km)
    amphi_10km <- amphi_10km + values(tmp_10km)
  }
  print(i)
  rm(tmp,tmp_1km, tmp_5km, tmp_10km)
}

ras <- rasterFromXYZ(cbind(CN_grid_1km, amphi_1km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_amphibians_1km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_5km, amphi_5km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_amphibians_5km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_10km, amphi_10km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_amphibians_10km.tif")

amphi <- read.csv("data-tables/Amphibians.csv")
amphi <- amphi %>% filter(path %in% paste0("/home/jhanson/aoh-data/AMPHIBIANS/",dir("AOH/Amphibians/")))
write.csv(amphi, "data-tables/Amphibians.csv", row.names=F)

# reptile
reptl_1km <- rep(0,nrow(CN_grid_1km))
reptl_5km <- rep(0,nrow(CN_grid_5km))
reptl_10km <- rep(0,nrow(CN_grid_10km))
for(i in 1:length(reptile)){
  tmp <- rast(paste0("AOH-layers/Reptiles/", reptile[i]))
  values(tmp)[values(tmp)>0] <- 1
  values(tmp)[is.na(values(tmp))] <- 0
  if(sum(values(tmp))>0){
    writeRaster(tmp, paste0("AOH/Reptiles/", reptile[i]))
    tmp_1km <- resample(tmp, China1km)
    tmp_5km <- resample(tmp, China5km)
    tmp_10km <- resample(tmp, China10km)
    values(tmp_1km)[is.na(values(tmp_1km))] <- 0
    values(tmp_5km)[is.na(values(tmp_5km))] <- 0
    values(tmp_10km)[is.na(values(tmp_10km))] <- 0
    values(tmp_1km)[(values(tmp_1km))>0] <- 1
    values(tmp_5km)[(values(tmp_5km))>0] <- 1
    values(tmp_10km)[(values(tmp_10km))>0] <- 1
    reptl_1km <- reptl_1km + values(tmp_1km)
    reptl_5km <- reptl_5km + values(tmp_5km)
    reptl_10km <- reptl_10km + values(tmp_10km)
  }
  print(i)
  rm(tmp,tmp_1km, tmp_5km, tmp_10km)
}

ras <- rasterFromXYZ(cbind(CN_grid_1km, reptl_1km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_reptiles_1km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_5km, reptl_5km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_reptiles_5km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_10km, reptl_10km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_reptiles_10km.tif")

reptl <- read.csv("data-tables/Reptiles.csv")
reptl <- reptl %>% filter(path %in% paste0("/home/jhanson/aoh-data/Reptiles/",dir("AOH/Reptiles/")))
write.csv(reptl, "data-tables/Reptiles.csv", row.names=F)


# bird
bird_1km <- rep(0,nrow(CN_grid_1km))
bird_5km <- rep(0,nrow(CN_grid_5km))
bird_10km <- rep(0,nrow(CN_grid_10km))
for(i in 1:length(bird)){
  tmp <- rast(paste0("AOH-layers/Birds/", bird[i]))
  values(tmp)[values(tmp)>0] <- 1
  values(tmp)[is.na(values(tmp))] <- 0
  if(sum(values(tmp))>0){
    writeRaster(tmp, paste0("AOH/Birds/", bird[i]))
    tmp_1km <- resample(tmp, China1km)
    tmp_5km <- resample(tmp, China5km)
    tmp_10km <- resample(tmp, China10km)
    values(tmp_1km)[is.na(values(tmp_1km))] <- 0
    values(tmp_5km)[is.na(values(tmp_5km))] <- 0
    values(tmp_10km)[is.na(values(tmp_10km))] <- 0
    values(tmp_1km)[(values(tmp_1km))>0] <- 1
    values(tmp_5km)[(values(tmp_5km))>0] <- 1
    values(tmp_10km)[(values(tmp_10km))>0] <- 1
    bird_1km <- bird_1km + values(tmp_1km)
    bird_5km <- bird_5km + values(tmp_5km)
    bird_10km <- bird_10km + values(tmp_10km)
  }
  print(i)
  rm(tmp,tmp_1km, tmp_5km, tmp_10km)
}

ras <- rasterFromXYZ(cbind(CN_grid_1km, bird_1km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_birds_1km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_5km, bird_5km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_birds_5km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_10km, bird_10km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_birds_10km.tif")

bird <- read.csv("data-tables/Birds.csv")
bird <- bird %>% filter(path %in% paste0("/home/jhanson/frc-data/BOTW/",dir("AOH/Birds/")))
write.csv(bird, "data-tables/Birds.csv", row.names=F)

# mammal
mamal_1km <- rep(0,nrow(CN_grid_1km))
mamal_5km <- rep(0,nrow(CN_grid_5km))
mamal_10km <- rep(0,nrow(CN_grid_10km))
for(i in 1:length(mammal)){
  tmp <- rast(paste0("AOH-layers/Mammals/", mammal[i]))
  values(tmp)[values(tmp)>0] <- 1
  values(tmp)[is.na(values(tmp))] <- 0
  if(sum(values(tmp))>0){
    writeRaster(tmp, paste0("AOH/Mammals/", mammal[i]))
    tmp_1km <- resample(tmp, China1km)
    tmp_5km <- resample(tmp, China5km)
    tmp_10km <- resample(tmp, China10km)
    values(tmp_1km)[is.na(values(tmp_1km))] <- 0
    values(tmp_5km)[is.na(values(tmp_5km))] <- 0
    values(tmp_10km)[is.na(values(tmp_10km))] <- 0
    values(tmp_1km)[(values(tmp_1km))>0] <- 1
    values(tmp_5km)[(values(tmp_5km))>0] <- 1
    values(tmp_10km)[(values(tmp_10km))>0] <- 1
    mamal_1km <- mamal_1km + values(tmp_1km)
    mamal_5km <- mamal_5km + values(tmp_5km)
    mamal_10km <- mamal_10km + values(tmp_10km)
  }
  print(i)
  rm(tmp,tmp_1km, tmp_5km, tmp_10km)
}

ras <- rasterFromXYZ(cbind(CN_grid_1km, mamal_1km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_mammals_1km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_5km, mamal_5km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_mammals_5km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_10km, mamal_10km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_mammals_10km.tif")

mamal <- read.csv("data-tables/Mammals.csv")
mamal <- mamal %>% filter(path %in% paste0("/home/jhanson/aoh-data/MAMMALS_TERRESTRIAL_ONLY/",dir("AOH/Mammals/")))
write.csv(mamal, "data-tables/Mammals.csv", row.names=F)


ras <- rasterFromXYZ(cbind(CN_grid_1km, amphi_1km+reptl_1km+bird_1km+mamal_1km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_vertebrates_1km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_5km, amphi_5km+reptl_5km+bird_5km+mamal_5km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_vertebrates_5km.tif")

ras <- rasterFromXYZ(cbind(CN_grid_10km, amphi_10km+reptl_10km+bird_10km+mamal_10km))
crs(ras) <- Behrmann
ras <- mask(rast(ras), vect(China))
writeRaster(ras, "Species-richness/SR_vertebrates_10km.tif")


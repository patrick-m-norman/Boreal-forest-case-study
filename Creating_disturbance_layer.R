##install and import required packages
required_packages <- c(
  "tidyverse",
  "rgdal",
  "raster",
  "sf",
  "sp",
  "rgbif",
  "dismo",
  "rasterVis",
  "rgeos",
  "lwgeom",
  "stringr",
  "terra",
  "fasterize"
)

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}
lapply(required_packages, library, character.only = TRUE)

#set working directory
setwd(".")


base_forest <- rast("./caribou_disturbance_components/caribou_habitat_land_cover.tif")
crs(base_forest)

main_roads_ont <- vect("./caribou_disturbance_components/mainroads_3978_500m_buffer_dissolved.shp")
gravel_roads_ont <- vect("./caribou_disturbance_components/forest_road_gravel_3978_500m_buffer_dissolved.shp")
unsurface_roads_ont <-vect("./caribou_disturbance_components/ontario_buffered_unsurfaced_500m.shp")
quebec_roads <- vect("./caribou_disturbance_components/quebec_buffered_roads.shp")
ont_harvest <- rast("./caribou_disturbance_components/ont_harvest_post_1980_no_data_removed_buffered_ext_matched.tif")
que_harvest <- rast("./caribou_disturbance_components/quebec_harvest_buffered_500m.tif")
ont_unclassified <- rast("./caribou_disturbance_components/ont_unclassified_buffered.tif")
fire_quebec <- vect("./caribou_disturbance_components/fire_quebec_1980.shp")
fire_ontario <- vect("./caribou_disturbance_components/fire_ontario_1980.shp")
ont_dist <- rast("./caribou_disturbance_components/ont_lc_dist_500m_buffered.tif")
que_dist <- rast("./caribou_disturbance_components/que_lc_dist_500m_buffered.tif")

fire_quebec <- st_read("./caribou_disturbance_components/fire_quebec_1980.shp")
fire_quebec_fixed <- st_make_valid(fire_quebec)
fire_quebec <- vect(fire_quebec_fixed)

fire_ontario <- st_read("./caribou_disturbance_components/fire_ontario_1980.shp")
fire_ontario_fixed <- st_make_valid(fire_ontario)
fire_ontario <- vect(fire_ontario_fixed)

crs(main_roads_ont) == crs(base_forest)
crs(gravel_roads_ont) == crs(base_forest)
crs(unsurface_roads_ont) == crs(base_forest)
crs(quebec_roads) == crs(base_forest)

quebec_roads <- project(quebec_roads, crs(base_forest))

crs(quebec_roads) == crs(base_forest)

crs(ont_harvest) == crs(base_forest)
crs(que_harvest) == crs(base_forest)
crs(ont_unclassified) == crs(base_forest)
crs(fire_quebec) == crs(base_forest)
crs(fire_ontario) == crs(base_forest)
crs(ont_dist) == crs(base_forest)
crs(que_dist) == crs(base_forest)


ext(main_roads_ont) == ext(base_forest)
main_roads_ont <- terra::crop(main_roads_ont,base_forest)

ext(gravel_roads_ont) == ext(base_forest)
gravel_roads<- terra::crop(gravel_roads_ont,base_forest)

ext(unsurface_roads_ont) == ext(base_forest)
unsurface_roads_ont <- terra::crop(unsurface_roads_ont,base_forest)

ext(quebec_roads) == ext(base_forest)
quebec_roads <- terra::crop(quebec_roads,base_forest)

ext(ont_harvest) == ext(base_forest)
ont_harvest <- terra::crop(ont_harvest,base_forest)
ext(ont_harvest) == ext(base_forest)
ont_harvest <- resample(ont_harvest, base_forest)

ext(que_harvest) == ext(base_forest)
que_harvest <- terra::crop(que_harvest,base_forest)
ext(que_harvest) == ext(base_forest)
que_harvest <- resample(que_harvest, base_forest)
ext(que_harvest) == ext(base_forest)

ext(ont_unclassified) == ext(base_forest)
ont_unclassified <- terra::crop(ont_unclassified,base_forest)
ext(ont_unclassified) == ext(base_forest)
ont_unclassified <- resample(ont_unclassified, base_forest)
ext(ont_unclassified) == ext(base_forest)


ext(fire_quebec) == ext(base_forest)
fire_quebec <- terra::crop(fire_quebec,base_forest)

ext(fire_ontario) == ext(base_forest)
fire_ontario <- terra::crop(fire_ontario,base_forest)

ext(ont_dist) == ext(base_forest)
ont_dist <- terra::crop(ont_dist,base_forest)
ext(ont_dist) == ext(base_forest)
ont_dist <- resample(ont_dist, base_forest)
ext(ont_dist) == ext(base_forest)

ext(que_dist) == ext(base_forest)
que_dist <- terra::crop(que_dist,base_forest)
ext(que_dist) == ext(base_forest)
que_dist <- resample(que_dist, base_forest)

#polygon_disturbance <- rbind(main_roads_ont,unsurface_roads_ont, quebec_roads, fire_ontario, fire_quebec)
polygon_disturbance <- rbind(main_roads_ont, unsurface_roads_ont)
polygon_disturbance <- rbind(polygon_disturbance, quebec_roads)

raster_polygon_dist <- fasterize(polygon_disturbance, base_forest)

#writeRaster(raster_polygon_dist, "C:/Users/Carly/rasterized_polygonal_disturbance.tif")
raster_polygon_dist <- rast("C:/Users/Carly/rasterized_polygonal_disturbance.tif")
plot(raster_polygon_dist)

all_disturbance <- merge(raster_polygon_dist,que_dist)
all_disturbance <- merge(all_disturbance, ont_dist)
all_disturbance <- merge(all_disturbance, ont_unclassified)
all_disturbance <- merge(all_disturbance, que_harvest)
all_disturbance <- merge(all_disturbance, ont_harvest)


all_disturbance <- c(raster_polygon_dist,que_dist,ont_dist,ont_unclassified, que_harvest, ont_harvest)

m <- c(1,2,1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rclmat



all_disturbance_reclass <- terra::classify(all_disturbance, rclmat)

writeRaster(all_disturbance_reclass,"all_disturbance_for_caribou.tif", overwrite=TRUE)

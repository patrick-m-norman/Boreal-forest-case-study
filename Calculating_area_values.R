#Looping multiple zonal statistics at once
setwd(".")

library(raster)
library(rgeos)
library(sf)
library(exactextractr)
library(dplyr)
library(plyr)
library(data.table)
library(parallel)

options(scipen=999)

#reading in the raster layers
Old_growth <- ("Old_growth_cleaned_to_forest.tif")
Logged_planted <- ("All_plantations_and_harvests_post1920_within_AUO.tif")
Disturbed_1920_1975 <- ("Pre_1975_post1920_known_disturbed_AOU.tif")
Total_forest_area <- ("CA_forest_ecosystems_AOU_warped_w_nodata.tif")

#Creating a raster list
raster_list <- as.list(c(Old_growth, Logged_planted, Disturbed_1920_1975, Total_forest_area))

#reading in the polygon layer
poly_full <- st_read(dsn = '.', layer = "Area_polygon")
poly_read_full <- st_as_sf(poly_full)

#creating a polygon data frame to join later on
poly_data_frame <- as.data.frame(poly_full)
poly_data_frame$geometry <- NULL
poly_data_frame$index <- 1:nrow(poly_data_frame)

#The extraction function. Note that the input rasters are all the same resolution (30 x 30)
my_function <- function(x){
  raster_i <- raster(x)
  zonal_list <- exact_extract(raster_i, poly_full, fun ='sum',include_cell=FALSE,include_xy=FALSE)
  zonal_ha <- zonal_list*0.09
  return(data.frame(zonal_ha))
}

#Detecting cores for parallel processing
cores <- detectCores() - 25

#Running the function and binding columns. Note that the mclapply does not work on windows. Use the lapply line or incorporate the 'future' package
#to run in parallel
results_frame <- mclapply(raster_list, my_function, mc.cores=cores) %>% bind_cols()
#results_frame <- lapply(raster_list, my_function) %>% bind_cols()

#renaming the columns
trial_frame <- results_frame
names(trial_frame) <- c('Old_growth', 'Logged_planted', 'Disturbed_1920_1975', 'Total_forest_area')
trial_frame$index <- 1:nrow(trial_frame)


#joining the dataframe back together
export_data <-  merge(x=poly_data_frame,y= trial_frame, by= "index", all.x = TRUE)

#writing the data frame to a csv file
fwrite(export_data,'Total_area_within_study.csv')

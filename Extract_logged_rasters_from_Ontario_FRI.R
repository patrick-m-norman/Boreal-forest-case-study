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
  "terra"
)

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}
lapply(required_packages, library, character.only = TRUE)

#set working directory
setwd("//researchdata.student.ad.griffith.edu.au/data/ClimateB/Canadian Mature Forest Mapping/Spatial Data/")

##importing height layer as a base for rasterisation
ont_harvest<- rast("./combined_harvest_map_ontario/CA_harvest_ont_clipped.tif")

##get list of regions by extracting filenames,and retaining only those in .gdb format
file_list <- list.files("./Ontario FRI/") 
file_list_keep <- file_list %>% 
  str_detect('gdb') %>%
  keep(file_list, .)

#different areas have difference crs, this file has the associations
crs_list <- read.csv("./Ontario FRI/crs_summary.csv")

#loop through each management unit gdb
for (j in 1:length(file_list_keep)) {
  
  #getting the region name for filing and organisation
  current_region <- file_list_keep[j]
  current_region_split <- as.list(strsplit(current_region, "_")[[1]])
  trimmer <- c(4:(length(current_region_split)-1))
  current_region_split <- current_region_split[trimmer]
  current_region_name <- paste(current_region_split, collapse='_')
  print(current_region_name)
  
  ###import gdb by region, specifically calling "Polygon Forest" layer
  
  #currently using a work-around to get layer names 
  #(there doesn't seem to be a way to get them before importing the file)
  #so, when the file is imported, a warning is generated that lists all of the layers
  #the warning is then stored, converted to a list
  #then the layer name with "Polygon_Forest" is selected, and the data is reimported
  
  ####Update: many of the shapefiles do not follow the naming convention
  ##so some layer names will have to be manually imported
  ##alternatively keep adding to the str_detect() options
  #even doing so, probably still will break in cases where there are similar names, or strsplit fails
  ###this could be fixed by running through and exporting just the polygon file as a shapefile for each MU
  
  ##Update:there are a few MU that will have to manually processed
  ##---Trout Lake - all layers in the gdb are fragments of the polygon layer (why..)
  #--------had to merge them separately, so trout lake is now in a separate polygon layer
  ##---also issues with Wabigoon
  
  ##MANUALLY PROCESS TROUT LAKE, WABIGOON, ROMEO MALETTE - make sure that the CRS is correct!!!!
  
  ##get list of layers via warning message
  withCallingHandlers({
    dat <- vect(paste0("./Ontario FRI/",file_list_keep[j]))
  }, warning = function(w){
    warnMsg <<- w$message
    invokeRestart("muffleWarning")
  })
  
  #split them into a list
  layer_list <- as.list(strsplit(warnMsg, ",")[[1]])
  
  #attempt to pick the relevant layer
  layer_list_keep <-layer_list %>% 
    str_detect('Forest|FinalLayer|FOREST_|_FOREST|forest_|FRI|Inv_|_2D|Redlake|Temagami') %>%
    keep(layer_list, .)
  
  #store as character
  ###NOTE: if the script fails because of layer issues
  ###manually enter the polygon layer name into this variable
  # layer_name <- ""
  layer_name <- trimws(as.character(layer_list_keep))
  
  #in cases where there are multiple layers highlighted with the desired name
  ##this takes the first (which often results in failure)
  if(length(layer_name)>1) {
    layer_name <- layer_name[length(layer_name)]
  }
  
  #import the polygon shapefile
  print("getting vectors...")
  dat <- vect(paste0("./Ontario FRI/",file_list_keep[j]), layer=layer_name)
  print("vectors loaded!")
  
  #select only forest polygons
  dat <- dat[which(dat$POLYTYPE == "FOR"),]
  
  #get crs from the .csv
  crs(dat) <- paste0("epsg:",crs_list$EPSG[which(crs_list$File == file_list_keep[j])])
  #project from the native crs to WGS84
  dat <- project(dat, crs(ont_harvest))
  
  ###This selects only logged polygons
  dat_logged<- dat[which(dat$DEPTYPE =="HARVEST"),]
  
  #optional script for if exporting the shapefile as well as the raster
  #writeVector(dat_logged, paste0("Output_location", current_region_name, ".shp"))
  
  
  #rasterisation - using height as the raster template
  #raster value set by "field"
  dat_rast <-rasterize(dat_logged, ont_harvest, field="YRDEP")
  
  #write raster locally
  writeRaster(dat_rast, paste0("Output_location", current_region_name, ".tif"))
 }

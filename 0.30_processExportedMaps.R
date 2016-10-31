
# ArcGIS can't read the projection info in GEE outputs for some reason
# Also, full HPA 30 m exports come out as two tiles

# This script reads in the GEE output rasters, tweaks the datum in the 
# proj4 projection specification, merges tiles, and writes back out

# This assumes rasters are written out with the year as the first part of the name
# also currently assumed output is split into TWO rasters

# Jill Deines

# load packages
library(raster)
library(rgdal)
library(gdalUtils)

## NOTE: FUTURE VERSIONS SHOULD CHANGE THE POLYGON MASK TO USE THE EXPORTED
## NO DATA VALUE OF -9999

# directory containing rasters
rasDir <- 'C:/Users/deinesji/Google Drive/GEE_classification/HPA_annual_tests'

# director to write out to
outDir <- 'S:/Users/rappjer1/Jill/HPA_test_maps'

# specified projection
# add the datum specification (NAD83) for EPSG 5070 (Albers Equal Area US48)
newProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# shapefile to mask by (directory, filename without extension)
hparrb <- readOGR('S:/Users/deinesji/HPA/gis','HPA_30km_RRB_buff')
hparrb2 <- spTransform(hparrb, CRS(newProj))

# end user specifications -------------------------------------------

# get files
rasFiles <- list.files(rasDir)

# get years involved
years <- as.numeric(unique(substr(rasFiles, 1, 4)))


# function to merge/define projection of rasters by year
rasMerge <- function(year, outDir, newProj, maskShape){
  # load pair of rasters for that year
  yearFiles <- grep(year, rasFiles, value=T) 
  ras1 <- raster(paste(rasDir,yearFiles[1],sep='/'))
  ras2 <- raster(paste(rasDir,yearFiles[2],sep='/'))
  
  # overwrite projection to include datum
  proj4string(ras1) <- newProj
  proj4string(ras2) <- newProj
  
  # get output filename
  outRasName <- gsub("-.*$", "", yearFiles[1])
  outFileName <- paste0(outDir, '/', outRasName, '.tif')
  
  # merge 
  merged <- merge(ras1, ras2)
  masked <- mask(merged, maskShape, filename=OutFileName)
  return(merged)
}

# apply function to each year and store in a list (function also writes out)
newRasters <- list()

for(i in 1:length(years)){
  ptm <- proc.time()
  newRasters[[i]] <- rasMerge(years[i], outDir, newProj, hparrb2)
  proc.time() - ptm
}




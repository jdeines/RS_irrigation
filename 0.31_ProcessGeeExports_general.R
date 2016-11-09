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


# directory containing rasters
rasDir <- 'C:/Users/deinesji/Google Drive/GEE_tsExports'
fName <- 'KS_2001_GI.tif'

# director to write out to
outDir <- rasDir
outName <- 'KS_2001_GI_projFixed.tif'

# specified projection
# add the datum specification (NAD83) for EPSG 5070 (Albers Equal Area US48)
newProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# end user specifications -------------------------------------------

# load
ras1 <- raster(paste(rasDir,fName,sep='/'))
  
# overwrite projection to include datum
proj4string(ras1) <- newProj

# write out
writeRaster(ras1, paste(rasDir,outName,sep='/'))


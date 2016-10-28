
# ArcGIS can't read the projection info in GEE outputs for some reason

# This script reads in the GEE output rasters, tweaks the datum in the 
# proj4 projection specification, and writes the rasters back out

# Jill Deines

# load packages
library(raster)

# path to directory
fileDir <- 'S:/Data/GIS_Data/Downloaded/Irrigation_Datasets/GCE_1km_2000'

# load rasters
v0 <- raster(paste0(fileDir, '/GCE_1km_V0_hpaClip.tif'))
v1 <- raster(paste0(fileDir, '/GCE_1km_V1_hpaClip.tif'))

# see projection info
proj4string(v0)

# add the datum specification (NAD83) for EPSG 5070 (Albers
newProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# replace the proj4 string of the two rasters
proj4string(v0) <- newProj
proj4string(v1) <- newProj

# write out rasters
writeRaster(v0, paste0(fileDir, '/GCE_1km_V0_hpaClip_withProjection.tif'))
writeRaster(v1, paste0(fileDir, '/GCE_1km_V1_hpaClip_withProjection.tif'))

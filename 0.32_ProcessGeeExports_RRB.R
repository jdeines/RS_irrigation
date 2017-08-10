# ArcGIS can't read the projection info in GEE outputs for some reason

# This script reads in the GEE output rasters, tweaks the datum in the 
# proj4 projection specification, and writes back out to a new directory
# using the same filename

# it's set up for the annual maps from AIM-RRB, or the greater RRB region.
# no tile merging necessary.


# Jill Deines

# load packages
library(raster)
library(rgdal)



# directory containing rasters
rasDir <- 'C:/Users/deinesji/Google Drive/GEE_classification/AIM_RRB_finalMaps'

# director to write out to
outDir <- 'C:/Users/deinesji/Google Drive/GEE_classification/AIM_RRB_finalMaps/withProjectionDefined'

# specified projection
# add the datum specification (NAD83) for EPSG 5070 (Albers Equal Area US48)
newProj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# end user specifications -------------------------------------------

# get filenames
files <- list.files(rasDir, pattern='*tif$')

# loop over files to load, define projection, write back out
for (file in files) {
  ras1 <- raster(paste(rasDir,file,sep='/'))
  proj4string(ras1) <- newProj
  writeRaster(ras1, paste(outDir,file,sep='/'), datatype='INT1U', NAflag=999)
}


# test hydroshare downloads
#t1999 <- raster('C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/manuscript/data/testMapDownloads/1999_AIM-RRB_20170721.tif')
#t1999[t1999 == 254] <- NA
#plot(t1999)



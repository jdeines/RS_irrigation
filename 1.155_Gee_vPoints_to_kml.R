# Convert Gee vPoints to KML 
# Feb 3, 2017

# Randomly generated points in GEE for masked classification area
# (Validation/2.0_Gen_Rand_vPoints)

# this converts to a KML for uploading as a fusion table

library(rgdal)

filedir <- 'C:/Users/deinesji/Google Drive/GEE_validation/validationSets'
fileName <- '2002_data_NE_randAll_1050_jmd_annual' # no ext

outDir <- 'C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/data/GIS/validation/generatedPointsForUpload'
outName <- '2002_data__NE_randAll_1050_jmd_annual1036.kml'

latcolumn <- 'lat'
longcolumn <- 'long'

#columnsToKeep <- c('GI_max_14') # for location only datasets
columnsToKeep <- c('class','certainty','classNum')

# -------------------------------------------------------------------

# load data
vpoints <- read.csv(paste0(filedir, '/', fileName, '.csv'))
vpoints <- vpoints[,c(columnsToKeep,latcolumn,longcolumn)]

# spatialize
coordinates(vpoints) <- cbind(longcolumn, latcolumn)
proj4string(vpoints) <- CRS("+proj=longlat +datum=WGS84")

# export
writeOGR(vpoints, paste0(outDir, '/', outName), layer = 'layer', driver = 'KML')

# Convert Gee vPoints to KML 
# Feb 3, 2017

# Randomly generated points in GEE for masked classification area
# (Validation/2.0_Gen_Rand_vPoints)

# this converts to a KML for uploading as a fusion table

library(rgdal)

filedir <- 'C:/Users/deinesji/Google Drive/GEE_validation/validationSets'
fileName <- '2015_locations_randAll_1250'

outDir <- 'C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/data/GIS/validation/generatedPointsForUpload'
outName <- '2015_locations_randAll_1250.kml'

# -------------------------------------------------------------------

# load data
vpoints <- read.csv(paste0(filedir, '/', fileName, '.csv'))
vpoints <- vpoints[,c('GI_max_14','latitude','longitude')]

# spatialize
coordinates(vpoints) <- ~ longitude + latitude
proj4string(vpoints) <- CRS("+proj=longlat +datum=WGS84")

# export
writeOGR(vpoints, paste0(outDir, '/', outName), layer = 'layer', driver = 'KML')

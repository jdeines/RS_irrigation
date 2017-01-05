# Get study area counties
# 2017-01-04

# Goal: produce a shapefile/KML of counties intersecting the
# study area boundaries, with columns for 5 digit fips and 
# the percent of the county within the basin


# packages required
library(rgdal)
library(rgeos)
library(raster)
library(maps)

#polygon denoting area of interest
gisDir <- 'S:/Users/deinesji/HPA/gis'
aoi0 <- readOGR(gisDir,'BigExtent_RRB_RRCA_buff', verbose=F) 

# out directory
outDir <- 'C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/data/GIS/kml'
fname <- 'Counties_RRCAextent_Clip'

# get counties --------------------------------------------------------
# load TIGER counties
countyAll <- readOGR('S:/Data/GIS_Data/Downloaded/TIGER/TIGER2012/COUNTY',
                     'tl_2012_us_county', verbose=F)
# subset columns and add 5 digit fips
columnsToKeep <- c('STATEFP','COUNTYFP','COUNTYNS','NAME')
countyAll <- countyAll[,columnsToKeep]
countyAll$fips5 <- paste0(countyAll$STATEFP, countyAll$COUNTYFP)

# do a slight negative buffer to avoid line mismatches
aoi <- gBuffer(aoi0, width=-100)
  
# match projections
countyAll <- spTransform(countyAll, CRS(proj4string(aoi)))

# subset counties by aoi (all counties with any overlap)
counties <- countyAll[aoi,]

# crop counties by aoi
countyCrop  <- raster::intersect(countyAll, aoi)

# find the proportion of each county within the aoi by getting the area of
# both and dividing
counties$area_km2 <- round(gArea(counties, byid=T)/1000000)
countyCrop$area_km2 <- round(gArea(countyCrop, byid=T)/1000000)
countyCrop$proportion <- countyCrop$area_km2/counties$area_km2
countyCrop$ninety <- countyCrop$proportion > 0.9

# visualize
plot(counties)
plot(aoi, add=T)
plot(countyCrop[countyCrop$ninety == TRUE,], add=T, col='red')

# how many counties are 90% within the border?
sum(countyCrop$ninety == TRUE)

# id counties inside aoi with a 1. Note `rgeos` must be loaded
overlap <- gIntersects(countyAll, aoi, byid=T)  # 1's indicate overlap

# fill in state abbrevs column based on FIPS
data(state.fips)  # from maps package
state.fips$STATEFP <- sprintf("%02d",state.fips$fips) # add leading zero
countiesOut <- merge(countyCrop, state.fips[,c('abb','STATEFP')], all.y=F)

# add a 'masterID' field for tracking stats in GEE
countiesOut$masterID <- paste0('county_', countiesOut$abb, '-', countiesOut$fips5)

# write out as a KML and a shapefile
spdf.wgs84 <- spTransform(countiesOut, CRS("+proj=longlat +datum=WGS84"))
writeOGR(spdf.wgs84, paste0(outDir, '/', fname, '.kml'),
          layer = 'layer', driver = 'KML')
writeOGR(spdf.wgs84, paste0(outDir, '/shapefile'), fname, 
         driver = 'ESRI Shapefile')



# random stuff


# get total study area
library(rgdal)
library(rgeos)

studyArea <- readOGR('C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/data/GIS/kml/shapefile',
'BigExtent_RRB_RRCA_buff')

studyArea <- spTransform(studyArea, CRS('+init=epsg:5070'))

area <- gArea(studyArea)
area.km2 <- area * 1e-6


rrca <- readOGR('C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/data/GIS/kml/RRCA_bound.kml','layer')
rrca <- spTransform(rrca, CRS('+init=epsg:5070'))
area <- gArea(rrca)
area.km2 <- area * 1e-6

rrb <- readOGR('C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/data/GIS/kml/RRB_basin_states.kml','boundExtent')
rrb <- spTransform(rrb, CRS('+init=epsg:5070'))
area <- gArea(rrb)
area.km2 <- area * 1e-6
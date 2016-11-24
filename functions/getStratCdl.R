#' Stratified sampling function for Cropland Data Layer
#'
#' Overview: turns the CDL raster into a data frame and performs stratified 
#' random sampling on CDL classes. Then converts data frame back to 
#' spatialPointsDataFrame. This is intended to be used on a cdl raster that has
#' already been masked by a polygon area of interest. Polygon should be in same 
#' projected coordinates as CDL layer
#' 
#' Because grassland is such a huge proportion of the landscape but relatively 
#' easy to classify as irr/nonirr, the function "downweights" this crop class. 
#' It does this in a gangsta way by increasingly the # points desired, and then 
#' removing a percentage of the grassland points based on argument 'grassthin'.
#' 
#' The following CDL classes are also removed from the sample: Any CDL class with
#'  'DEVELOPED' in it is also removed from the sample, forests, and wetlands. 
#'  While this could somewhat alter the density, I'm not too worried since they
#'  are relatively rare on the HPA landscape.
#' 
#' @param cdl.mask a cdl raster masked to polygon of interest. Masking is important so that cells outside the polgyon boundary are NA.
#' @param poly a polygon of interest in same projection as CDL, meters based
#' @param sampleDensity Put a point every "this many" square kilometers. Brown and Pervez 2010 ranged from every 19.3 km2 to 96.7 km2 and didn't see a difference in reported accuracy.
#' @param cdlKey name of R object for the CDL csv key Jill created from Cropscape dbf's
#' @param seed default set to 1 so function reproduces the same point dataset each time. User can change to any number to set a different random draw of points.
#' @param sampleFactor amount to increase number of sample points to offset grassland thinning
#' @param grassThin percentage (approximate due to random rbinom draw) of grassland points to keep. 
#' @keywords stratified random sampling CDL
#' @export
#' @examples
#' test <- cleanNassCensus(nass.df, year)


getStratCdl <- function(cdl.mask, poly, sampleDensity, sampleFactor, 
                        grassThin, cdlKey, seed=1){
  # get poly area and number of target points
  area <- gArea(poly) 
  area.km2 <- area*1e-6
  pointsDesired <- area.km2/sampleDensity
    
  # convert to df, rename columns, and remove NAs
  r.points <- as.data.frame(cdl.mask, xy = T, centroids=T)
  names(r.points) <- c('x','y','CDLcode')
  r.points <- r.points[!is.na(r.points$CDLcode),]
   
  # convert points desired to sample fraction
  ncells <- dim(r.points)[1]
  # inflate samFrac to allow downscale pruning of grassland samples
  samFrac <- (pointsDesired*sampleFactor)/ncells
  
  # take a proportional sample from all classes
  set.seed(seed)                        
  p1.r <- r.points %>%
          group_by(CDLcode) %>%
          sample_frac(size = samFrac)

    # add crop key (do after sampling if final method)
  p1.r2 <- merge(p1.r, cdlKey[,c('VALUE','CLASS_NAME')], 
            by.x='CDLcode', by.y='VALUE', all.y = F)

  # thin grassland
  p1.r2$keep <- 1  
  # update 'keep' binary based on a random assigmnet of 0,1 to grassland class
  grassSamples <- sum(p1.r2$CLASS_NAME == 'Grass/Pasture') # get number
  set.seed(seed)
  p1.r2[p1.r2$CLASS_NAME == 'Grass/Pasture','keep'] <- rbinom(grassSamples, 1, grassThin)  
  # flag developed classes for removal as well
  p1.r2[grepl('Developed',p1.r2$CLASS_NAME),'keep'] <- 0
  # flag forests and wetlands for removal, and water
  p1.r2[grepl('Forest',p1.r2$CLASS_NAME),'keep'] <- 0
  p1.r2[grepl('Wetlands',p1.r2$CLASS_NAME),'keep'] <- 0
  p1.r2[grepl('Water',p1.r2$CLASS_NAME),'keep'] <- 0
  
  # remove points
  p1.r2 <- p1.r2[p1.r2$keep ==1,]
  # drop keep column
  p1.r2 <- p1.r2[,c('CDLcode','CLASS_NAME','x','y')]

  # turn back to spatial points
  coordinates(p1.r2) <- ~ x + y
  proj4string(p1.r2) <- tweakProj
  
  return(p1.r2)  
}
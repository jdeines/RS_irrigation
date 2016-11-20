#' Convert an exported GEE .csv of point data to a spatial points data frame
#'
#' This function converts the .geo string column of the GEE export to coordinates,
#' removes the 'system.index' and '.geo' columns, add specifies the WGS84 latlong 
#' projection
#' 
#' @param csvFilename Full path and filename of GEE-exported csv
#' @keywords GEE csv spdf
#' @export
#' @examples
#' # specify file name
#' csvFile <- 'C:/Users/Documents/exportedPointFeatureCollection.csv'
#' 
#' # load data and convert to spdf
#' points <- csvToSpdf(csvFile)


GeeCsvToSpdf <- function(csvFilename){
  # load output from GEE
  validated <- read.csv(csvFilename, stringsAsFactors=F)
  
  # extract coordinates from messy .geo string
  coords <- sub(".*\\[(.*)\\].*", "\\1", validated$.geo, perl=TRUE)
  coords2 <- do.call(rbind, strsplit(coords, ','))   
  
  # add coordinates to df
  validated$x <- as.numeric(coords2[,1])
  validated$y <- as.numeric(coords2[,2])
  
  # drop a few columns
  validated2 <- validated[, -which(names(validated) %in% c('system.index','.geo'))]
  
  # spatialize points
  coordinates(validated2) <- ~ x + y
  proj4string(validated2) <- "+proj=longlat +datum=WGS84"
  
  return(validated2)
}

# a place for functions used across scripts


## GEE KML to spdf ---------------------------------------------------
csvToSpdf <- function(csvFilename){
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

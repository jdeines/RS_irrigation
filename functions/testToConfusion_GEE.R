#' Process test validation data sampled from GEE to confusion tables 
#'
#' Function to take test validation points exported from GEE, 
#' and produce confusion tables of the results. Currently hard-coded for Jill's specific class 
#'  category names and certainty system. Requires packages like 
#'  'caret' and helper functions at bottom of this script.
#'   This is poor coding practice...
#' 
#' @param datadir directory in which point files are located
#' @param dataname filename of point data as exported from GEE 05.0 and 05.01 accuracy assessments
#' @param bandName band name from the GEE classifier. 'b'+ year for interannual; else 'classification'
#' @param certainty  1 or 2 - 1 will process points with certainty level 1 only
#' @keywords confusion
#' @export
#' @examples
#' # example
#' 
#' 

testToConfusionGee <- function(datadir, dataname, bandName, certainty){
  # load data
  dataset <- read.csv(paste0(datadir, '/',dataname), stringsAsFactors = F)
  # make the GEE classification column a uniform name
  dataset$geeBand <- dataset[,bandName]
  
  #
  
  # binary lookup table for classification codes: GEE
  predictedCode = data.frame(geeBand = c(0:2),
                             predictedClass = c('notirrigated','irrigated','notirrigated'))
  # binary lookup table for classification codes: validation points (drop uncertain)
  referenceCode = data.frame(classNum = c(0,1,2,4),
                             referenceClass = c('notirrigated','irrigated',
                                                'notirrigated','notirrigated'))
  
  # add category names
  dataset <- merge(dataset, predictedCode)
  dataset <- merge(dataset, referenceCode)
  
  # subset to most certain points if needed
  if (certainty == 1) {
    dataset <- dataset[dataset$certainty == 1,]
  }
  
  # produce confusion matrix using 'caret' package
  cmatrix <- confusionMatrix(data = dataset$predictedClass, 
                             reference = dataset$referenceClass)
  
  # prettify it
  ctable <- prettyConfusionBinary(cmatrix)
  return(ctable)
}




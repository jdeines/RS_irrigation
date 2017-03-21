#' Process test validation data to confusion tables 
#'
#' Function to take test validation points exported from GEE, 
#' run an R random forest classifier on them, and produce confusion
#'  tables of the results. Currently hard-coded for Jill's specific class 
#'  category names and certainty system. Requires packages like 
#'  'caret' and 'randomForest', and helper functions at bottom of this script.
#'   This is poor coding practice...
#' 
#' @param dataset loaded point dataframe as exported from GEE validation 2.1_vpoints
#' @param numClasses number of classes, either 2 or 3 
#' @param certainty  1 or 2 - 1 will process points with certainty level 1 only
#' @param classifier random forest classifier as created by 'randomForest' function
#' @keywords confusion
#' @export
#' @examples
#' # example
#' 
#' 

testToConfusion <- function(dataset, numClasses, certainty, classifier){
  # clean up data
  pointsdf <- dataset[ , -which(names(dataset) %in% c('.geo'))]
  pointsdf$class <- as.character(pointsdf$class)

  # create a key to convert to the number of classes wanted
  if (numClasses == 3) {
    keyToClass <- data.frame(class = c('irrigated','dryland','noncrop','fallow'),
                       classes = c('irrigated','dryland','noncrop','noncrop'))
  } else if (numClasses == 2) {
    keyToClass <- data.frame(class = c('irrigated','dryland','noncrop','fallow'),
                             classes = c('irrigated','notirrigated','notirrigated','notirrigated'))
  } else {
    stop('invalid number of classes entered')
  }
  
  # merge new class column with points df.
  # incidentally, this also removes certainty level 3 as class 'Uncertain', as key is written
  pointsdf2 <- merge(pointsdf, keyToClass)
 
  # subset to most certain points if needed
  if (certainty == 1) {
    pointsdf2 <- pointsdf2[pointsdf2$certainty == 1,]
  }
  
  # run predictions/classification
  pointsdf2$randFor <- predict(classifier, newdata = pointsdf2, type='response')

  # adjust random forest predictions based on number of classes desired
  if (numClasses == 2) {
    # conversion key
    key2class.2 <- data.frame(randFor = c('dryland','irrigated','noncrop'),
                              randFor2 = c('notirrigated','irrigated','notirrigated'))
    # add randFor2 classes to dfs
    pointsdf2 <- merge(pointsdf2, key2class.2)
    
    # confusion tables
    ConTable <- makeConfusion2(pointsdf2$randFor2, pointsdf2$classes)
  } else if (numClasses == 3) {
    ConTable <- makeConfusion(pointsdf2$randFor, pointsdf2$classes) 
  }

  return(ConTable)
}


# quick function to run a confusion matrix then format confusion table
makeConfusion <- function(prediction, reference) {
  cmatrix <- confusionMatrix(data = prediction, reference = reference)
  ctable <- prettyConfusion(cmatrix)
}

# quick function to run a confusion matrix then format confusion table
makeConfusion2 <- function(prediction, reference) {
  cmatrix <- confusionMatrix(data = prediction, reference = reference)
  ctable <- prettyConfusionBinary(cmatrix)
}

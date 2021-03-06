# a 3 class and binary version

#' Improve table formatting from caret::confusionMatrix 
#'
#' This function converts the output from the confusionMatrix function
#' in the caret package into something that more closely resembles a 
#' "standard" confusion table for remote sensing accuracy reporting
#' 
#' @param confusionMatrixOutput object of class 'confusionMatrix'
#' @keywords confusion
#' @export
#' @examples
#' # run confusion stats
#' midRep07confusion <- caret::confusionMatrix(data = midRep07$predictedClass, reference = midRep07$referenceClass)
#' 
#' # prettify
#' midrep07.ct <- prettyConfusion(midRep07confusion)

prettyConfusion <- function(confusionMatrixOutput){
  core <- t(confusionMatrixOutput$table)
  core2 <- as.data.frame(unclass(core))
  core2$total <- rowSums(core2[,1:3])
  
  # make sure column and row names are lowercase
  names(core2) <- tolower(names(core2))
  
  # make column sum
  core3 <- rbind(core2, colSums(core2[,1:4]))
  core3$ProducerAccuracy <- c(core3$dryland[1]/core3$total[1]*100,
                              core3$irrigated[2]/core3$total[2]*100,
                              core3$noncrop[3]/core3$total[3]*100,
                              NA)
  overallAccuracy <- confusionMatrixOutput$overall[1] *100
  names(overallAccuracy) <- NULL
  core4 <- rbind(core3,c(core3$dryland[1]/core3$dryland[4]*100,
                          core3$irrigated[2]/core3$irrigated[4]*100,
                          core3$noncrop[3]/core3$noncrop[4]*100,
                          NA,
                          overallAccuracy))
  rownames(core4) <- c(rownames(core2),'Total', 'ConsumerAccuracy')
  return(core4)
}

# binary classifier
prettyConfusionBinary <- function(confusionMatrixOutput){
  core <- t(confusionMatrixOutput$table)
  core2 <- as.data.frame(unclass(core))
  core2$total <- rowSums(core2[,1:2])
  
  # make sure column and row names are lowercase
  names(core2) <- tolower(names(core2))
  
  # make column sum
  core3 <- rbind(core2, colSums(core2[,1:3]))
  core3$ProducerAccuracy <- c(core3$irrigated[1]/core3$total[1]*100,
                              core3$notirrigated[2]/core3$total[2]*100,
                                                            NA)
  overallAccuracy <- confusionMatrixOutput$overall[1] *100
  names(overallAccuracy) <- NULL
  core4 <- rbind(core3,c(core3$irrigated[1]/core3$irrigated[3]*100,
                          core3$notirrigated[2]/core3$notirrigated[3]*100,
                          NA,
                          overallAccuracy))
  rownames(core4) <- c(rownames(core2),'Total', 'ConsumerAccuracy')
  return(core4)
}
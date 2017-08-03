# Preparation for training points and test points to post
# July 25, 2017
# j deines

# goal: clean up the data tables associated with training and test datasets
# in preparation for posting to Hydroshare

# this uses the same code as in 1.16_trainingPoints_RRB.Rmd, but it
# retains the lat/long coordinates in the csv output and rearranges data
# columns (removes monthly Landsat index numbers as well)

library(rgdal)

# Training Points --------------------------------------------------------------
# file directory and filenames
trainFolder <- 'C:/Users/deinesji/Google Drive/GEE_validation/trainingSets'
files <- list.files(trainFolder, full.names = T, pattern='*_v02.csv')

exportFolder <- 'C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/manuscript/data/HydrosharePointData'


# load and merge
trainList <- list()
for (i in 1:length(files)){
  trainList[[i]] <- read.csv(files[i]) 
}
trainPoints <- do.call("rbind",trainList)

# number of total training points
nrow(trainPoints)

# make a crop/irrigated status crossed class type to maximize downstream flexibility
trainPoints$masterType <- paste0(trainPoints$crop,'_',trainPoints$class)
masterKey <- data.frame(masterType = sort(unique(trainPoints$masterType)),
                        masterNum = 0:(length(unique(trainPoints$masterType))-1))
trainPoints1 <- merge(trainPoints, masterKey)

# clean up data: needless columns
trainExport0 <- trainPoints1[ , -which(names(trainPoints1) %in% 
                  c(".geo",'certainty', 'system.index'))]

# clean up data: remove variables associated with monthly composites, as 
# not described/presented/used in the manuscript; reorganize
trainExport <- trainExport0[,c('lat','long','year','masterType','masterNum',
                               'class','classNum','crop','koep',
                               'GI_max_14','GI_range_14','NDVI_max_14',
                               'NDVI_range_14','NDWI_range_14','NDWI_max_14',
                               'EVI_range_14','EVI_max_14','aridity','pdsi_ann',
                               'pdsi_grow','pr_ann','pr_early','pr_grow','pr_paw',
                               'slope_mean','b1','b1_1','greenArid','ndwi_gi',
                               'GI_max_4','GI_max_5','GI_max_6','GI_max_7',
                               'GI_max_8','GI_max_9','GI_max_10','GI_max_11')]

# rename a few columns
names(trainExport)[names(trainExport) == 'b1'] <- 'paw_cm'
names(trainExport)[names(trainExport) == 'b1_1'] <- 'paw_vol'
names(trainExport)[names(trainExport) == 'greenArid'] <- 'AGI'
names(trainExport)[names(trainExport) == 'ndwi_gi'] <- 'WGI'

# replace 'dryland' with 'rainfed'
trainExport$class <- as.character(trainExport$class)
trainExport[trainExport$class == 'dryland','class'] <- 'rainfed'

# write out CSV 
write.csv(trainExport, row.names = F,
          paste0(exportFolder,
                 '/Deines_et_al_trainingPointData_2010_2012.csv'))

# Test Points --------------------------------------------------------------
#  data directory and filenames
dataDir <- 'C:/Users/deinesji/Google Drive/GEE_tableExports/Accuracy_PointsforConfusionTables_test5_randFor'

p2002_final <- '2002_RRB_NE_confusionData_RRB_test5_randFor_cleanedInterannual1x_binary_final.csv'
p2015_final <- '2015_RRB_confusionData_RRB_test5_randFor_cleanedInterannual1x_binary_final.csv'

# load
p2002 <- read.csv(paste0(dataDir, '/', p2002_final))
names(p2002)[2] <- 'AIM_RRB'
p2002$year <- 2002
p2002a <- p2002[,c('lat','long','year','class','classNum','certainty','AIM_RRB')]

p2015 <- read.csv(paste0(dataDir, '/', p2015_final))
names(p2015)[2] <- 'AIM_RRB'
p2015$year <- 2015
p2015a <- p2015[,c('lat','long','year','class','classNum','certainty','AIM_RRB')]

# combine
testpoints <- rbind(p2002a,p2015a)

# remove uncertain points
testpoints <- testpoints[testpoints$certainty > 0,]

# use only 'rainfed','irrigated', and 'noncrop' classes
testpoints$class <- as.character(testpoints$class)
testpoints[testpoints$class == 'dryland','class'] <- 'rainfed'
testpoints[testpoints$class == 'fallow','class'] <- 'noncrop'

# add a binary class column
testpoints$binaryClass <- testpoints$class
testpoints[testpoints$binaryClass == 'rainfed','binaryClass'] <- 'nonirrigated'
testpoints[testpoints$binaryClass == 'noncrop','binaryClass'] <- 'nonirrigated'
# and a numeric key
binaryKey <- data.frame(binaryNum = c(0,1),
                        binaryClass = c('nonirrigated', 'irrigated'))
testpoints2 <- merge(testpoints, binaryKey)

# rearrange for export
testpoints3 <- testpoints2[,c('lat','long','year','class','binaryClass','binaryNum',
                              'certainty','AIM_RRB')]

# write out CSV 
write.csv(trainExport, row.names = F,
          paste0(exportFolder,
                 '/Deines_et_al_testPointData_2002_2015.csv'))

# Preparation for training points and test points to post
# July 25, 2017
# j deines

# goal: clean up the data tables associated with training and test datasets
# in preparation for posting to Hydroshare

# this uses the same code as in 1.16_trainingPoints_RRB.Rmd, but it
# retains the lat/long coordinates in the csv output and rearranges data
# columns (removes monthly Landsat index numbers as well)

library(rgdal)

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

# write out CSV 
write.csv(trainExport, row.names = F,
          paste0(exportFolder,
                 '/Deines_et_al_trainingPointData_2010_2012.csv'))




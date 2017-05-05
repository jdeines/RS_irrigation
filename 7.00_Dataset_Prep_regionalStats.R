# Preparation for Supplementary Dataset 1
# May 5, 2017
# j deines

# goal: combine yearly table exports into one massive file and convert from 
# pixel count to area

# run for test 5 maps: 
# random forest 500 tree classification
# 2010 and 2012 training points, rainfed soy removed
# binary classification using annual composites + environmental variables
# 1 pass of a 3x3 majority filter
# interannual cleaning that removed any pixels irrigated only 1x between 1999-2016


#--------------------------------------------------------
# input data info (output from gEE 04.11_RegionalStats_interannual script)
regionsDir <- 'C:/Users/deinesji/Google Drive/GEE_tableExports/RRB_test5_regional_county_Stats'
fnameSuffix <- '*_test5_randFor_interannual_plusAncillary.csv'

# set cell area: rasters exported at 30 m resolution
cellArea <- 30*30 # m^2

# output info
outDir <- 'C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/manuscript/data'
outName <- 'Deines_etAl_HPAIM_RegionalStatistics_v1.0.csv'

# --------------------------------------------------------------

# get filenames
files <- list.files(regionsDir, pattern=fnameSuffix)

# read in to a single dataframe
all1 = as.data.frame(do.call(rbind, lapply(files, function(x) {
  csv1 <- read.csv(paste0(regionsDir,'/',x))
  csv1$year <- as.numeric(substr(x, start=1,stop=4))
  return(csv1)
})))

# convert cell count to area
all1$irrigated_m2 <- all1$X1 * cellArea
all1$irrigated_km2 <- all1$irrigated_m2 / 1000000

# pick columns to keep
dataOut0 <- all1[,c('masterID','year','irrigated_m2','irrigated_km2')]

# get mean area in each region to sort data by
areamean <- aggregate(irrigated_km2 ~ masterID, data = dataOut0, FUN = mean, na.rm=T)
sorted <- areamean[order(areamean$irrigated_km2, decreasing=T),]
sorted$masterID <- as.character(sorted$masterID)

# convert some names
sorted$master_ID <- sorted$masterID
sorted$master_ID[7] <- 'CO_full_area'
sorted$master_ID[6] <- 'KS_full_area'
sorted$master_ID[2] <- 'NE_full_area'
sorted$master_ID[3] <- 'RRCA_model_boundary'
sorted$master_ID[14] <- 'NE_Lower_NRD'
sorted$master_ID[18] <- 'NE_Middle_NRD'
sorted$master_ID[13] <- 'NE_Upper_NRD'
sorted$master_ID[8] <- 'NE_Tri_NRD'

# transfer new names to full dataset
dataOut <- merge(sorted[,c('masterID','master_ID')], dataOut0)

# remove extra ID and sort year
dataOut <- dataOut[,2:5]
dataOut <- dataOut[order(dataOut$year),]

# export
write.csv(dataOut, row.names=F, file = paste0(outDir, '/', outName))
          

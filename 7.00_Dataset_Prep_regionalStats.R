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

# update 7/20/17: changed to use revised boundaries


#--------------------------------------------------------
# input data info (output from gEE 04.11_RegionalStats_interannual script)
regionsDir <- 'C:/Users/deinesji/Google Drive/GEE_tableExports/RRB_test5_regional_county_Stats'
fnameSuffix <- '*_test5_randFor_interannual_plusAncillary_REVISION_BOUNDARIES_ksSplit_GRBstates.csv'

# set cell area: rasters exported at 30 m resolution
cellArea <- 30*30 # m^2

# output info
outDir <- 'C:/Users/deinesji/Dropbox/1PhdJill/hpa/irrigation/manuscript/data'
outName <- 'Deines_etAl_AIMRRB_RegionalStatistics.csv'

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
dataOut0 <- all1[,c('masterid','year','irrigated_m2','irrigated_km2')]
names(dataOut0)[1] <- 'masterID'
dataOut0$masterID <- as.character(dataOut0$masterID)

# improve area names for legibility -------------------------------------
# county names are fine, but update region names
# rrb regions - default naming is fine
nameKey <- data.frame(masterID = sort(unique(dataOut0$masterID)),
                      master_ID = sort(unique(dataOut0$masterID)),
                      stringsAsFactors = FALSE)

# buffer regions
nameKey[nameKey$masterID == 'Platte-rrcaRrbBuffExtent','master_ID'] <- 'Full_Buffer_Extent'
nameKey[nameKey$masterID == 'CO_full_RRCA','master_ID'] <- 'Full_Buffer_CO'
nameKey[nameKey$masterID == 'KS_full_RRCA','master_ID'] <- 'Full_Buffer_KS'
nameKey[nameKey$masterID == 'NE_full_RRCA','master_ID'] <- 'Full_Buffer_NE'
# rrca regions
nameKey[nameKey$masterID == 'RRCA_modifiedBorders','master_ID'] <- 'RRCA_model_boundary'
nameKey[nameKey$masterID == 'Platte-NE_RRCA','master_ID'] <- 'RRCA_NE_Platte'
nameKey[nameKey$masterID == 'RRCA_mod_CO','master_ID'] <- 'RRCA_CO'
nameKey[nameKey$masterID == 'RRCA_mod_NE','master_ID'] <- 'RRCA_NE'
nameKey[nameKey$masterID == 'RRCA_mod_KS_main','master_ID'] <- 'RRCA_KS_main'
nameKey[nameKey$masterID == 'RRCA_mod_KS_sliver','master_ID'] <- 'RRCA_KS_lowerSliver'
#nrds
nameKey[nameKey$masterID == 'Lower_NRD_basinClip','master_ID'] <- 'NE_Lower_NRD'
nameKey[nameKey$masterID == 'Middle_NRD_basinClip','master_ID'] <- 'NE_Middle_NRD'
nameKey[nameKey$masterID == 'Upper_NRD_basinClip','master_ID'] <- 'NE_Upper_NRD'
nameKey[nameKey$masterID == 'Tri_NRD_basinClip','master_ID'] <- 'NE_Tri_NRD'
# GRB
nameKey[nameKey$masterID == 'RRCA_RRB_Union','master_ID'] <- 'GRB'
nameKey[nameKey$masterID == 'Union_CO','master_ID'] <- 'GRB_CO'
nameKey[nameKey$masterID == 'Union_KS','master_ID'] <- 'GRB_KS'
nameKey[nameKey$masterID == 'Union_NE','master_ID'] <- 'GRB_NE'

# rrb*rrca intersection (Figure 4)
nameKey[nameKey$masterID == 'RRCARRB_intersection','master_ID'] <- 'RRB_RRCA'
nameKey[nameKey$masterID == 'RRCARRB_CO','master_ID'] <- 'RRB_RRCA_CO'
nameKey[nameKey$masterID == 'RRCARRB_NE','master_ID'] <- 'RRB_RRCA_NE'
nameKey[nameKey$masterID == 'RRCARRB_KS_main','master_ID'] <- 'RRB_RRCA_KS_main'
nameKey[nameKey$masterID == 'RRCARRB_KS_sliver','master_ID'] <- 'RRB_RRCA_lowerSliver'


# transfer new names to full dataset
dataOut <- merge(nameKey, dataOut0)

# remove extra ID and sort year
dataOut <- dataOut[,c('master_ID','year','irrigated_m2','irrigated_km2')]
dataOut <- dataOut[order(dataOut$master_ID, dataOut$year, decreasing=TRUE),]

# export
write.csv(dataOut, row.names=F, file = paste0(outDir, '/', outName))
          

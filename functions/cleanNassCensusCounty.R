#' Clean NASS field crop data
#'
#' Given a raw NASS data frame for the 'field crop' group and the summary year desired,
#' the function cleans the data  for the input region, split by irrigation status. 
#' This version retains county-level numbers for later manual aggregation.
#' For Hay, it tallies alfalfa hay/haylage
#' as one category, and "other hay" for all other hay/haylages (after removing superset 
#' categories). Returns a sorted data frame with commodity, irrigated area (km^2), 
#' non-irrigated area (km^2), and total area (km^2)
#' 
#' @param nass.df Raw output from function getNASS
#' @param year Year for cleaning and summarizing
#' @keywords NASS clean
#' @export
#' @examples
#' test <- cleanNassCensus(nass.df, year)


cleanNassCensusCounty <- function(nass.df, year){
  library(tidyr)
  # remove excess columns
  keepThese <- c('state_alpha','prodn_practice_desc','short_desc','Value', 'class_desc',
                 'county_name','unit_desc','commodity_desc','year','state_ansi','county_code')
  nass.df <- nass.df[,keepThese]
  
  # remove (D) values, remove commas (!), convert to numeric
  nass.df <- nass.df[!grepl('(D)',nass.df$Value),]
  nass.df$Value <- gsub(",", "", nass.df$Value, fixed = T)
  nass.df$Value <- as.numeric(nass.df$Value)
  
  # keep only records with "ACRES HARVESTED"
  phrase <- 'ACRES HARVESTED'
  nass.df <- nass.df[grepl(phrase,nass.df$short_desc),]
  
  # for now, just year specified (could split into list by year in future..)
  nass.df <- nass.df[nass.df$year == year,]
  
  # make 5 digit fips code
  nass.df$state_ansi <- sprintf("%02d",nass.df$state_ansi) 
  nass.df$county_code <- sprintf("%03d",nass.df$county_code) 
  nass.df$fips5 <- paste0(nass.df$state_ansi, nass.df$county_code)
  
  # so variable names match after a change  
  nass.df2 <- nass.df

  # change 'all production practices' to 'total
  nass.df2[nass.df2$prodn_practice_desc == 'ALL PRODUCTION PRACTICES',
           'prodn_practice_desc'] <- 'TOTAL'
  
#   # hay/haylage check
#   hay_commodities <- c('HAY','HAY & HAYLAGE','HAYLAGE')
#   hays <- nass.df2[nass.df2$commodity_desc %in% hay_commodities,
#                    c('short_desc','commodity_desc')]
#   
  # HAY: make an alfalfa category
  alfalfaRecords <- grepl('ALFALFA', nass.df2$short_desc) & 
                      !grepl('EXCL', nass.df2$short_desc)
  nass.df2[alfalfaRecords,'commodity_desc'] <- 'ALFALFA'
  
  
  # HAY: delete the supersets to leave just "other" classes
    supersets <- c('HAY & HAYLAGE - ACRES HARVESTED',
                 'HAY & HAYLAGE, IRRIGATED - ACRES HARVESTED',
                 'HAY - ACRES HARVESTED',
                 'HAY, IRRIGATED - ACRES HARVESTED',
                 'HAYLAGE - ACRES HARVESTED',
                 'HAYLAGE, IRRIGATED - ACRES HARVESTED')
  
  nass.df3 <- nass.df2[!nass.df2$short_desc %in% supersets,]
  
  # wheat, sunflower, cotton: remove subclasses
  cropsToAddress <- c('WHEAT','SUNFLOWER','COTTON')
  nass.df4 <- nass.df3[!(nass.df3$commodity_desc %in% cropsToAddress & 
                           nass.df3$class_desc != 'ALL CLASSES'),]
  
  #Remove the GRASSES and LEGUMES separate types, and GRASSES & LEGUMES, OTHER
  
  # rename HAY and HAYLAGE 
  nass.df4[grepl("HAY",nass.df4$commodity_desc),'commodity_desc'] <- 'HAY.OTHER'
  nass.df4[grepl("HAYLAGE",nass.df4$commodity_desc),'commodity_desc'] <- 'HAY.OTHER'
  
  # aggregate by crop type and irrigation status
  summary <- aggregate(Value ~ commodity_desc + prodn_practice_desc + fips5,
                       data = nass.df4, FUN='sum')
  
  # convert from acres to square kilometers
  summary$Value <- summary$Value / 247.105
  
  # fill in a non-irrigated value
  summary.wide <- spread(summary, key = prodn_practice_desc, value = Value)
  summary.wide[is.na(summary.wide$IRRIGATED),'IRRIGATED'] <- 0
  summary.wide$NON_IRR <- summary.wide$TOTAL - summary.wide$IRRIGATED
  
  # sort
  summary.wide <- summary.wide[rev(order(summary.wide$IRRIGATED)),]
  summary.wide <- summary.wide[rev(order(summary.wide$fips5)),]
  
  return(summary.wide)
}


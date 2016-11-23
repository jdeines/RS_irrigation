#' Clean NASS field crop data
#'
#' Given a raw NASS data frame for the 'field crop' group and the summary year desired,
#' the function cleans the data and aggregates all counties into total commodities for 
#' the input region, split by irrigation status. For Hay, it tallies alfalfa hay/haylage
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


cleanNassCensus <- function(nass.df, year){
  library(tidyr)
  # remove excess columns
  keepThese <- c('state_alpha','prodn_practice_desc','short_desc','Value',
                 'county_name','unit_desc','commodity_desc','year')
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
  
  # aggregate over counties
  commodityTally <- aggregate(Value ~ short_desc, data = nass.df, FUN = 'sum')
  
  # add back in some helpful columns
  mergeTable <- nass.df[,c('commodity_desc','unit_desc',
                           'prodn_practice_desc','short_desc')]
  mergeTable2 <- unique(mergeTable)
  nass.df2 <- merge(commodityTally, mergeTable2, by.x = 'short_desc', all=F,
                    by.y = 'short_desc',sort=F)
  
  # change 'all production practices' to 'total
  nass.df2[nass.df2$prodn_practice_desc == 'ALL PRODUCTION PRACTICES',
           'prodn_practice_desc'] <- 'TOTAL'
  
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
  
  # rename HAY and HAYLAGE to TOTAL HAY
  nass.df3[grepl("HAY",nass.df3$commodity_desc),'commodity_desc'] <- 'OTHER HAY'
  nass.df3[grepl("HAYLAGE",nass.df3$commodity_desc),'commodity_desc'] <- 'OTHER HAY'
  
  # aggregate by crop type and irrigation status
  summary <- aggregate(Value ~ commodity_desc + prodn_practice_desc,
                       data = nass.df3, FUN='sum')
  
  # convert from acres to square kilometers
  summary$Value <- summary$Value / 247.105
  
  # fill in a non-irrigated value
  summary.wide <- spread(summary, key = prodn_practice_desc, value = Value)
  summary.wide[is.na(summary.wide$IRRIGATED),'IRRIGATED'] <- 0
  summary.wide$NON_IRR <- summary.wide$TOTAL - summary.wide$IRRIGATED
  
  # sort
  summary.wide <- summary.wide[rev(order(summary.wide$IRRIGATED)),]
  
  return(summary.wide)
}


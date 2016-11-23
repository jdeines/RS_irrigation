#' Extract NASS data by county from API
#'
#' Based on list of county fips, downloads NASS crop data by county
#' This function currently operates at the GROUP level.
#' 
#' @param apiKey Obtained from http://quickstats.nass.usda.gov/api
#' @param program 'CENSUS' or 'SURVEY'
#' @param sector 'CROPS' here -or DEMOGRAPHICS, ECONOMICS, ENVIRONMENTAL, ANIMALS & pRODUCTS
#' @param group 'FIELD CROPS' -or HORTICULTURE, VEGETABLES, FRUIT & TREE NUTS
#' @param aggregation specify geographic granularity of the data: 'STATE','AG DISTRICT','COUNTY','REGION','ZIP CODE'. Function currently set up for county level only
#' @param state state 2 character alpha code
#' @param county county 3-digit ANSI code
#' @keywords NASS get
#' @export
#' @examples
#' # assumes you have a 'counties' data frame with a column for state code and 
#' # county fips
#' 
#' # set up variables for data calls
#' apiKey <- '28EAA9E6-8062-34A4-981A-B2ED4692228A' # my code, modified
#' program = 'CENSUS'
#' sector <- 'CROPS'
#' group <- 'FIELD CROPS'
#' aggregation <- 'COUNTY'
#' # make empty list to populate, and loop through counties
#' nass.list <- list()
#' for(i in 1:nrow(counties)){
#'   nass.list[[i]] <- getNASS(apiKey, program, sector, group, aggregation, 
#'                                 state = counties$abb[i],
#'                                 county = counties$COUNTYFP[i])
#' }
#' # convert list of data frames to 1 giant dataframe
#' nass.df <- do.call("rbind",nass.list) 
#' 

getNASS <- function(apiKey, program, sector, group, aggregation, state, county){
  library(httr)
  library(jsonlite)
  # build URL query
  baseurl <- 'http://quickstats.nass.usda.gov/api/api_GET'
  format = 'JSON'
  GETrequest <- paste0(baseurl,
                     '/?key=',apiKey,
                     '&source_desc=',program,
                     '&sector_desc=',sector,
                     '&group_desc=',group,
                     '&agg_level_desc=',aggregation,
                     '&state_alpha=', state,
                     '&county_ansi=', county,
                     '&format=',format)
  
  # if present, replace commas and spaces in url with encodings
  if(grepl(" ", GETrequest))  GETrequest <- gsub(" ", "%20", GETrequest)
  if(grepl(",", GETrequest))  GETrequest <- gsub(",", "%2C", GETrequest)
  
  # retrive data
  req <- GET(GETrequest)
  # check status and throw stop/error: 200 means successful
  stop_for_status(req)
  # extract content
  json <- content(req, as = 'text')
  # convert from JSON and extract df from list object
  outputdf <- fromJSON(json)[[1]]
}

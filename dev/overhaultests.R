library(DDSQLtools)

# notes for overall: dataset extract (could be multiple),
# a minimum of country and indicator.

# only need get functions for the 12-15 variables that help identify a subset.
# but a generic get function to get valid values for any arbitrary variable (Except datavalue of course)
# and another get to get the queryable columns

# TypeofLocation is set to Whole Area, and urban rural are filtered out. They can be expicitly requested by the user though.

# Works
L <- getLocations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "true")


library(tidyverse)
getCodes <- function(type = "Locations", save = FALSE, ...){
  as_tibble(read_API2(type, save = save, ...))
}

linkGenerator2 <- function(server = "http://24.239.36.16:9654/un3/api/", 
                           type,
                           ...) {
  
  # This list needs a major overhaul: some items are valid and some are not.
  
  types <- c("ages",
             "openAges",
             "Component",
             "DataCatalog",
             "dataProcessTypes",
             "DataReliability",
             "DataSource",
             "DataSourceStatus",
             "DataSourceType",
             "DataStatus",
             "DataType",
             "DefaultKeys",
             "Indicator",
             "IndicatorTypes",
             "locAreaTypes",
             "Locations",
             "PeriodGroup",
             "PeriodType",
             "Sex",
             "StatisticalConcept",
             "StructuredData",
             "subGroups",
             "SubGroupType",
             "TimeReference",
             "UserUtility")
  
  type  <- match.arg(tolower(type), choices = tolower(types))
  query <- build_filter2(...)
  link  <- paste0(server, type, query)
  return(link)
}

read_API2 <- function(type, save, ...){
  P <- linkGenerator2(type = type, ...)
  # Temporary, just to check how the URL is constructed
  print(P)
  X <- rjson::fromJSON(file = P)
  #
  out <- X %>% 
    lapply(unlist) %>%    # list elements can either be lists or vectors
    lapply(as.list) %>%   # here now everything is homogenously a vector
    bind_rows() %>%       # even if named elements differ still becomes rectangular
    as.data.frame()       # coerce to desired form
  if (type == "recordDataDetail") {
    out <- format.numeric.colums(out)
  }
  if (save) {
    save_in_working_dir(data = out, file_name = paste0("UNPD_", 
                                                       type))
  }
  return(out)
}

# All the optional args in build_filter2() need an overhaul: some work for some
# items and some for others. Some are required and some not. 
# Some can be used in combinations and some not. I don't
# see a pattern. 
build_filter2 <- function(
                          dataProcess = NULL,
                          startYear = NULL,
                          endYear = NULL,
                          AgeStart = NULL,
                          AgeEnd = NULL,
                          indicatorTypeIds = NULL,
                          isComplete = NULL,
                          isActive = NULL,
                          locIds = NULL,
                          LocAreaType = NULL,
                          SubGroup = NULL,
                          addDefault = NULL,
                          includeDependencies = NULL, 
                          includeFormerCountries = NULL) {
  I <- environment() %>% as.list() %>% unlist()
  if (length(I) > 0){
    S   <- paste(paste(names(I), I, sep = "="), collapse="&")
    out <- paste0("?", S)
  } else {
    out <- ""
  }
  return(out)
}

# Gets all locations
getCodes("locations")
getCodes("locations", includeFormerCountries = "false")
getCodes("locations",
         includeFormerCountries = "false",
         includeDependencies = "true")

# Gets all indicator Types
getCodes("indicatortypes")

# Get all location area types
getCodes("locareatypes")

getCodes("locareatypes",
         isActive = "1")

getCodes("locareatypes",
         isComplete = "1")

getCodes("locareatypes",
         startYear = 800,
         endYear = 1000)

getCodes("locareatypes",
         endYear = 2019,
         indicatorTypeIds = 8,
         isComplete = 0,
         locIds = 4,
         startYear = 1950
         )


# Get all age distributions
getCodes("ages")

## Why doesn't this work?
## It says in the new docs that these parameters are available
getCodes("ages", AgeStart = 0, AgeEnd = 30)
getCodes("ages", AgeStart = "0", AgeEnd = "30")

# Doesn't work, see docx file with inconsistencies
getCodes("openAges")

# Data process types
getCodes("dataprocesstypes",
         startYear = 1950
         endYear = 2019,
         indicatorTypeIds = 8,
         isComplete = 0,
         locIds = 4,
         )



getCodes("locAreaTypes", locIds = 8,indicatorTypeIds = 8) # works
getCodes("locAreaTypes", indicatorTypeIds = 8)            # fails
getCodes("locAreaTypes", locIds = 8)                      # fails
getCodes("locAreaTypes")  
getCodes("locareatypes")  
getCodes("locAreaTypes", locIds = "4,8", indicatorTypeIds = 8)  
"http://24.239.36.16:9654/un3/api/subGroups?endYear=2019&indicatorTypeIds=8&isComplete=0&locIds=4&startYear=1950"
getCodes(type = "subGroups", endYear = 2019, indicatorTypeIds = 8, isComplete = 0, locIds= 4, startYear = 1950) # works
getCodes(type = "subGroups", locIds = 4)                                          # fails
getCodes(type = "subGroups", locIds = 4, indicatorTypeIds = 8)                    # works
getCodes(type = "subGroups", locIds = 4, indicatorTypeIds = 8, startYear = 1980)  # works
getCodes(type="ages")
"http://24.239.36.16:9654/un3/api/dataProcessTypes?endYear=2019&indicatorTypeIds=8&isComplete=0&locIds=4&startYear=1950"
getCodes(type = "dataProcessTypes", endYear = 2019, indicatorTypeIds = 8, isComplete = 0, locIds= 4, startYear = 1950)

# Next to test.
# "http://24.239.36.16:9654/un3/api/locAreaTypes?endYear=2019&indicatorTypeIds=8&isComplete=0&locIds=4&startYear=1950"
# the below code implies this url: need new var names I think.
# http://24.239.36.16:9654/un3/api/locationType?indicatorType=8&isComplete=0&loc=818
library(magrittr);library(dplyr)
# read_API2(type="locAreaTypes", 
#           indicatorTypeIds = 8, 
#          locIds = 8,        
#          isComplete = 0,
#          startYear=1980,
#          save =FALSE)

# build_filter2()

# 
# build_filter2(dataProcess="Register",Location=818, includeDependencies="true")
# P <- getLocationTypes("LocAreaType",
#   indicatorType = 8, # Population by age and sex indicator;
#   loc = 818,         # Egypt
#   isComplete = 0)
# 
# 
# library(dplyr)
# bind_rows(list(list(A=1,B=2,D=5,E=6),
#                list(B=2,E=4,F=7)))
# X %>% bind_rows()
# X %>% lapply(unlist) %>% lapply(as.list) %>% bind_rows()
# X[[13]]
# str(X) == str(X %>% lapply(as.list))
# bind_rows(lapply(
#   list(list(A=1,B=2,D=5,E=6),
#                list(B=2,E=4,F=7)), 
#   as.list
#   ))
# 
# as.list(list(a=1,b=2))
# 
# 
# # keep only non-open ages, plus the highest open age. that is, throw out extra open ages
# # below the highest one.
# #filter(!(AgeSpan == -1 & AgeStart < max(AgeStart[AgeSpan == -1])))

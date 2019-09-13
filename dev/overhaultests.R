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
             "StructuredDataTable",
             "StructuredDataRecords",
             "StructuredDataSeries",
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
build_filter2 <- function(dataProcess = NULL,
                          dataProcessIds = NULL,
                          startYear = NULL,
                          endYear = NULL,
                          AgeStart = NULL,
                          AgeEnd = NULL,
                          indicatorTypeIds = NULL,
                          isComplete = NULL,
                          isActive = NULL,
                          locIds = NULL,
                          LocAreaType = NULL,
                          locAreaTypeIds = NULL,
                          SubGroup = NULL,
                          subGroupIds = NULL,
                          addDefault = NULL,
                          includeDependencies = NULL, 
                          includeFormerCountries = NULL) {

  # Keep as list because unlisting multiple ids for a single
  # parameters separates them into different strings
  I <- environment() %>% as.list()

  if (length(I) > 0) {
    # Collapse multiple ids to parameters
    I <- vapply(I, paste0, collapse = ",", FUN.VALUE = character(1))
    # and exclude the empty ones
    I <- I[I != ""]
    
    S   <- paste(paste(names(I), I, sep = "="), collapse="&")
    out <- paste0("?", S)
  } else {
    out <- ""
  }
  return(out)
}

getCodes("datareliability")

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

# Reurns same number of locations as other requests, why?
getCodes("locareatypes",
         startYear = 800,
         endYear = 1000)

# This can be read as give me locareatypes of:
# - Afghanistan
# - "Population by sex and age"
# - From 1950 to 2019
# - which are not complete
getCodes("locareatypes",
         locIds = 4,
         indicatorTypeIds = 8,
}         startYear = 1950,
         endYear = 2019,
         isComplete = 0
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
         startYear = 1950,
         endYear = 2019,
         indicatorTypeIds = 8,
         isComplete = 0,
         locIds = 4,
         )

# subGroups

# This can be read as give me the subgroups of:
# - Afghanistan
# - "Population by sex and age"
# - From 1950 to 2019
# - which are not complete
getCodes("subgroups",
         startYear = 1950,
         endYear = 2019,
         indicatorTypeIds = 8,
         isComplete = 0,
         locIds = 4,
         )

# Get data
# Example 1:
# - Data Process(dataProcessIds)=2 (Census)
# - Indicator Type(indicatorTypeIds)=8 (Pop by age/sex)
# - Age Distribution (isComplete)=0 (Abridged)
# - Country/Location(locIds)=4 (Afghanistan)
# - Location Area Type(locAreaTypeIds)=2 (Urban only)
# - Start Year(startYear)=1950 (default)
# - SubGroup(subGroupIds)=2 (Total or All Groups)

# I think this returns some time of metadata
getCodes("structureddataseries",
         locIds = 4,
         locAreaTypeIds = 2,
         dataProcessIds = 2,
         startYear = 1950,
         endYear = 3000,
         indicatorTypeIds = 8,
         isComplete = 0,
         subGroupIds = 2)


tst <-
  getCodes("structureddatatable",
           locIds = c(4, 32),
           locAreaTypeIds = 2,
           dataProcessIds = 2,
           startYear = 1950,
           endYear = 3000,
           indicatorTypeIds = 8,
           isComplete = 0,
           subGroupIds = 2)

# There is no way of uniquely identifying an observation
# because even if I specify the same columns as the parameters
# in the request, the response might bring *further* columns
# such as sex which would double the dataset in size.
# This means that in principle we can not reshape
# the data for the user (to wide or long).
tst %>%
  select(location,
         locationtype,
         dataprocess,
         sourceyear,
         year,
         datatype,
         sex,
         age,
         subgroup,
         FootNoteID,
         DataValue)


# Question to Tim #1
# Would the result of the request above be what the analyst receives?
# That is, all of the requested filters + the Data Value column?
# I'm trying to think in terms of whether we need more reshaping
# for the `DemoTools` wrappers.


# You left off here

# Next thing you wanted to do was to try some more examples
# of data extraction as the first one (try, Dominican Republic, fertility)
# and also try one with two or three countries (before you need to check
# that they indeed have the specific type of indicator available)


# Once you have that, you need to identify all API end points which are
# only useful to extract information to be used in the data extraction
# stage (location, locationtype, dataprocess, etc...). Why do you do that?
# Because I think a nice way of organizing everything is that these 'accessor'
# endpoints should have a separate function so that users can extract all their
# available values to search for the name. However, inside the main grab data
# function, the user will be able to just input either the code or the string name
# and we would do the extraction of these string names with these 'accessor'
# functions. Here I have a small dilemma which is whether each API request
# should return an empty data.frame with all columns of the request and thus
# if someone asks, let's say, for country asasda, you would get an empty
# data frame back, or whether we should throw an error on the 'accessor'
# functions that tell the user that country/dataprocess

# The other problem is whether making several API requests (through the accessor
# functions) every time you request a main data source would just break the API.
# This we have to talk with them. Two solutions is just adding a Sys.sleep(1)
# or something like that or just cacheing each accessor result in tmpdir
# for that session. If we add the Sys.sleep(1) solution (which I like)
# then we would have to avoid the empty dataframe workaround when the request
# is wrong and just fail fast to avoid spending time in the next requests
# when we already know that they've failed (not waste any more time).

# And the next stage is to identify the functions that do the actual extraction
# and just create wrappers around those and use the accessor function to
# check their values.


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

library(tidyverse)
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
  as_tibble(read_API(type, save = save, ...))
}

# All the optional args in build_filter2() need an overhaul: some work for some
# items and some for others. Some are required and some not. 
# Some can be used in combinations and some not. I don't
# see a pattern. 

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
         indicatorTypeIds = 8
         ## startYear = 1950,
         ## endYear = 2019,
         ## isComplete = 0
         )

# Get all age distributions
getCodes("ages")

## Why doesn't this work?
## It says in the new docs that these parameters are available
getCodes("ages", AgeStart = 0, AgeEnd = 30)
getCodes("ages", AgeStart = "0", AgeEnd = "30")

# Doesn't work, see docx file with inconsistencies
getCodes("openAges")
getCodes("openAges", 1)

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
tst1 <- getCodes("structureddataseries",
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


id <-
  getCodes("locations") %>%
  filter(Name == "Dominican Republic") %>%
  pull(PK_LocID)

getCodes("locareatype")
getCodes("dataprocesstypes")
getCodes("indicatortypes")


# To be able to make a query these parameters
# are required:
getCodes("structureddataseries",
         locIds = 214,
         locAreaTypeIds = 2,
         dataProcessIds = 2,
         indicatorTypeIds = 8,
         subGroupIds = 2)

tst <-
  getCodes("structureddatatable",
         locIds = 214,
         locAreaTypeIds = 2,
         dataProcessIds = 2,
         indicatorTypeIds = 8,
         subGroupIds = 2)


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


# You left off here

# Next thing you wanted to do was to try some more examples
# of data extraction as the first one (try, Dominican Republic, fertility)
# and also try one with two or three countries (before you need to check
# that they indeed have the specific type of indicator available)
tst <-
  getRecordData(locIds = "Mexico",
                # Options can be abridged, complete or total
                isComplete = "abridged",
                # Options can be:
                # Population by sex
                # Population by age and sex
                # Births by sex
                # Total fertility: Rates
                # Births by age of mother
                # Fertility rates by age of mother
                # Fertility age distribution measures
                # Deaths by age and sex
                # Infant mortality
                # Under-five mortality
                # Mortality rates by age and sex
                # Neonatal mortality
                # Postneonatal mortality
                # Child mortality (age 1-4)
                # Life table statistics
                indicatorTypeIds = "Total fertility: Rates",
                # Options can be:
                # Whole area
                # Urban
                # Rural
                locAreaTypeIds = "Whole area",
                # Options can be:
                # Census
                # Estimate
                dataProcessIds = "Survey: Demographic and Health Survey (DHS)",
                # Options are 5 thousand.
                # See getSubgroups()
                subGroupIds = "Total or All groups")

tst <- getCodes("ages")

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

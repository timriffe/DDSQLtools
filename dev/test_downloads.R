# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 16:58:45 2018
# --------------------------------------------------- #
remove(list = ls())
library(DDSQLtools)
library(tibble)

# Check what subgroups are available for:
S <- getSubGroups(indicatorTypeIds = 8,  # Population by age and sex indicator;
                  locIds = 818,          # Egypt
                  isComplete = 0)
S

L <- getLocations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false")
L

# Check what subgroups are available for:
P <- getLocationTypes(indicatorTypeIds = 8,  # Population by age and sex indicator;
                      locIds = 230,          # Venezuela
                      isComplete = 0)
P

I <- getIndicators(addDefault = "false")
I[, c("IndicatorTypeID", "Name", "ShortName")]


D <- getDataProcessTypes()
D[,c("PK_DataProcessTypeID","Name","ShortName")]

G <- getSeriesData(dataProcessIds = 2,    # Census
                   indicatorTypeIds = 8,  # Population
                   isComplete = 0,
                   locIds = 4,            # American Samoa
                   locAreaTypeIds = 2,    #
                   startYear = 1950,
                   subGroupIds = 2)       # Nationals only
G

X <- getRecordData(dataProcessIds = 2,   # Estimate
                   indicatorTypeIds = 8, # Population by age and sex - abridged 
                   isComplete = 0,    # Age Distribution: Abridged
                   locIds = 818,         # Egypt
                   locAreaTypeIds = 2,   # Whole area 
                   subGroupIds = 2       # Total or All groups
                   )  

# Test Links ------------------------------------------
# Link to country list
L1 <- linkGenerator(type = "country",
                    addDefault = "false",
                    includeDependencies = "false",
                    includeFormerCountries = "false")
L1

# Link to location types (for Egypt)
L2 <- linkGenerator(type = "locationType",
                    locIds = 818,
                    indicatorTypeIds = "8,9,10",
                    isComplete = 0)
L2

# Link to subgroup types (for Egypt)
L3 <- linkGenerator(type = "subGroup",
                    indicatorTypeIds = 8,
                    locIds = 818,
                    isComplete = 0)
L3

# Link to indicator list
L4 <- linkGenerator(type = "Indicator",
                    addDefault = "false")
L4

# Link to data process type list
L5 <- linkGenerator(type = "dataProcessTypes")
L5

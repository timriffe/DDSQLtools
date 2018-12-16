# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 16:58:45 2018
# --------------------------------------------------- #
remove(list = ls())
library(DDSQLtools)

# Check what subgroups are available for:
S <- getSubGroups(indicatorType = 8,  # Population by age and sex indicator;
                  loc = 818,          # Egypt
                  isComplete = 0)
S

L <- getLocations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false")
L

# Check what subgroups are available for:
P <- getLocationTypes(indicatorType = 8,  # Population by age and sex indicator;
                      loc = 818,          # Egypt
                      isComplete = 0)
P

I <- getIndicators(addDefault = "false")
I[, c("IndicatorTypeID", "Name", "ShortName")]


D <- getDataProcessTypes()
D

G <- getSeriesDataDetail(dataProcess = 2,
                         indicatorType = 8,
                         isComplete = 0,
                         loc = 4,
                         locAreaType = 2,
                         startYear = 1950,
                         subGroup = 2)
G

X <- getRecordDataDetail(dataProcess = 2,   # Estimate
                         indicatorType = 8, # Population by age and sex - abridged 
                         isComplete = 0,    # Age Distribution: Abridged
                         loc = 818,         # Egypt
                         locAreaType = 2,   # Whole area 
                         subGroup = 2       # Total or All groups
                         )  
as.tibble(X)
dim(X)

# Test Links ------------------------------------------
# Link to country list
L1 <- linkGenerator(type = "country",
                    addDefault = "false",
                    includeDependencies = "false",
                    includeFormerCountries = "false")
L1

# Link to location types (for Egypt)
L2 <- linkGenerator(type = "locationType",
                    loc = 818,
                    indicatorType = "8,9,10",
                    isComplete = 0)
L2

# Link to subgroup types (for Egypt)
L3 <- linkGenerator(type = "subGroup",
                    indicatorType = 8,
                    loc = 818,
                    isComplete = 0)
L3

# Link to indicator list
L4 <- linkGenerator(type = "Indicator",
                    addDefault = "false")
L4

# Link to data process type list
L5 <- linkGenerator(type = "dataProcessTypes")
L5

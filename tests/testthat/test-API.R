# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sun Dec 16 11:25:26 2018
# --------------------------------------------------- #

remove(list = ls())
library(testthat)
library(DDSQLtools)


## Test the linkGenerator

test_that("The linkGenerator() works fine", {
  
  L <- linkGenerator(type = "structureddatarecords",
                     locIds = 4,
                     indicatorTypeIds = 8,
                     dataProcessIds = c(2, 6))
  
  expect_output(print(L))                          # 1. Always expect an output;
  expect_true(is.character(L))                     # 2. The output is of the class "character";
  expect_error(linkGenerator(wrong_argument = 1))  # 3. Does not work with whatever argument in "...";
  expect_error(linkGenerator(type = "countryy"))   # 4. Is sensitive to typos;
  expect_equal(length(strsplit(L, split = " ")[[1]]), 1) # 5. Expect no spaces in the string. 
})


## Test API functions
validate_read_API <- function(Z) {
  test_that("The read_API works fine", {
    expect_output(print(Z))                     # 1. Always expect an output;
    expect_true(is.data.frame(Z))               # 2. The output is of the class "data.frame";
    expect_true(ncol(Z) >= 2)                   # 3. The output has al least 2 columns;
    expect_false(any(is.null(colnames(Z))))     # 4. All columns have names;
    expect_true(nrow(Z) >= 1)                   # 5. The output has at least 1 rows
  })
}

# ------------------------------------------
D <- getDataProcess()
validate_read_API(D)  # validate

# ------------------------------------------
S <- getSubGroups(indicatorTypeIds = 8,  # Population by age and sex indicator;
                  locIds = 818,          # Egypt
                  isComplete = 0)
validate_read_API(S)  # validate

# ------------------------------------------
L <- getLocations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false")
validate_read_API(L)  # validate

# ------------------------------------------
P <- getLocationTypes(indicatorTypeIds = 8,  
                      locIds = 818,
                      isComplete = 0)
validate_read_API(P)  # validate

# ------------------------------------------
I <- getIndicators(addDefault = "false")
validate_read_API(I)  # validate

# ------------------------------------------
G <- getSeriesData(dataProcessIds = 2,
                   indicatorTypeIds = 8,
                   isComplete = 0,
                   locIds = 4,
                   locAreaTypeIds = 2,
                   startYear = 1950,
                   subGroupIds = 2)

validate_read_API(G)  # validate

# ------------------------------------------
X <- getRecordData(dataProcessIds = 2,   # Census
                   indicatorTypeIds = 8, # Population by age and sex - abridged 
                   locIds = 818,         # Egypt
                   locAreaTypeIds = 2,   # Whole area 
                   subGroupIds = 2,      # Total or All groups
                   isComplete = 0)       # Age Distribution: Abridged

validate_read_API(X)  # validate

# Check whether it successfully accepts strings rather than codes
Y <- getRecordData(dataProcessIds = "Census",   # Estimate
                   indicatorTypeIds = "Population by age and sex", # Population by age and sex - abridged 
                   locIds = "Egypt",         # Egypt
                   locAreaTypeIds = "Whole area",   # Whole area 
                   subGroupIds = "Total or All groups",      # Total or All groups
                   isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(Y)  # validate

# Check whether it successfully mixed cases
mixed <- getRecordData(dataProcessIds = "census",   # Estimate
                       indicatorTypeIds = "population by age and sex", # Population by age and sex - abridged 
                       locIds = "egypt",         # Egypt
                       locAreaTypeIds = "Whole area",   # Whole area 
                       subGroupIds = "Total or All groups",      # Total or All groups
                       isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(mixed)  # validate


# mixed with codes
mixed_codes <- getRecordData(dataProcessIds = 2,   # Census
                             indicatorTypeIds = 8, # Population by age and sex - abridged 
                             locIds = 818,         # Egypt
                             locAreaTypeIds = "Whole area",   # Whole area 
                             subGroupIds = "Total or All groups",      # Total or All groups
                             isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(mixed_codes)  # validate

test_that("getRecordData with codes gives same output with strings", {
  expect_equal(X, Y)
  expect_equal(X, mixed)
  expect_equal(X, mixed_codes)
})


test_that("Looking up wrong input throws errors in getRecordData", {
  expect_error(getRecordData(locIds = "Wrong country"),
               regexp = "Location(s) 'Wrong country' not found. Check getLocations()",
               fixed = TRUE)

  expect_error(getRecordData(indicatorTypeIds = "Wrong"),
               regexp = "Location(s) 'Wrong' not found. Check getIndicators()",
               fixed = TRUE)

  expect_error(getRecordData(subGroupIds = "Wrong"),
               regexp = "Location(s) 'Wrong' not found. Check getSubGroups()",
               fixed = TRUE)

  expect_error(getRecordData(isComplete = "Wrong"),
               regexp = "IsComplete does not accept string 'Wrong'. Only 'abridged', 'complete', 'total'.",
               fixed = TRUE)
})

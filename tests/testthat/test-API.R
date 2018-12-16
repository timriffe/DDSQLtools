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
  
  L <- linkGenerator(type = "recordDataDetail",
                     loc = 4,
                     indicatorType = 8,
                     dataProcess = "2,6")
  
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
                                                # 5. ?
  })
}

# ------------------------------------------
D <- getDataProcessTypes()
validate_read_API(D)  # validate

# ------------------------------------------
S <- getSubGroups(indicatorType = 8,  # Population by age and sex indicator;
                  loc = 818,          # Egypt
                  isComplete = 0)
validate_read_API(S)  # validate

# ------------------------------------------
L <- getLocations(addDefault = "false",
                  includeDependencies = "false",
                  includeFormerCountries = "false")
validate_read_API(L)  # validate

# ------------------------------------------
P <- getLocationTypes(indicatorType = 8,  
                      loc = 818,
                      isComplete = 0)
validate_read_API(P)  # validate

# ------------------------------------------
I <- getIndicators(addDefault = "false")
validate_read_API(I)  # validate

# ------------------------------------------
G <- getSeriesDataDetail(dataProcess = 2,
                         indicatorType = 8,
                         isComplete = 0,
                         loc = 4,
                         locAreaType = 2,
                         startYear = 1950,
                         subGroup = 2)
validate_read_API(G)  # validate

# ------------------------------------------
X <- getRecordDataDetail(dataProcess = 2,   # Estimate
                         indicatorType = 8, # Population by age and sex - abridged 
                         isComplete = 0,    # Age Distribution: Abridged
                         loc = 818,         # Egypt
                         locAreaType = 2,   # Whole area 
                         subGroup = 2)       # Total or All groups
validate_read_API(X)  # validate












test_that("The linkGenerator() works fine", {
  
  L <- linkGenerator(type = "structureddatarecords",
                     locIds = 4,
                     indicatorTypeIds = 8,
                     DataProcessIDs = c(2, 6),
                     verbose_print = FALSE)
  
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
D <- get_dataprocess()
validate_read_API(D)  # validate

# ------------------------------------------
S <- get_subgroups(indicatorTypeIds = 8,  # Population by age and sex indicator;
                   locIds = 818,          # Egypt
                   isComplete = 0)
validate_read_API(S)  # validate

# ------------------------------------------
L <- get_locations(addDefault = "false",
                   includeDependencies = "false",
                   includeFormerCountries = "false")
validate_read_API(L)  # validate

# ------------------------------------------
P <- get_locationtypes(indicatorTypeIds = 8,  
                       locIds = 818,
                       isComplete = 0)
validate_read_API(P)  # validate

# ------------------------------------------
I <- get_indicators(addDefault = "false")
validate_read_API(I)  # validate

# ------------------------------------------
G <- get_seriesdata(DataProcessIDs = 2,
                    indicatorTypeIds = 8,
                    isComplete = 0,
                    locIds = 4,
                    locAreaTypeIds = 2,
                    startYear = 1950,
                    subGroupIds = 2)

validate_read_API(G)  # validate

# ------------------------------------------
X <- get_recorddata(DataProcessIDs = 2,   # Census
                    indicatorTypeIds = 8, # Population by age and sex - abridged 
                    locIds = 818,         # Egypt
                    locAreaTypeIds = 2,   # Whole area 
                    subGroupIds = 2,      # Total or All groups
                    isComplete = 0)       # Age Distribution: Abridged

validate_read_API(X)  # validate

# Check whether it successfully accepts strings rather than codes
Y <- get_recorddata(DataProcessIDs = "Census",   # Estimate
                    indicatorTypeIds = "Population by age and sex", # Population by age and sex - abridged 
                    locIds = "Egypt",         # Egypt
                    locAreaTypeIds = "Whole area",   # Whole area 
                    subGroupIds = "Total or All groups",      # Total or All groups
                    isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(Y)  # validate

# Check whether it successfully mixed cases
mixed <- get_recorddata(DataProcessIDs = "census",   # Estimate
                        indicatorTypeIds = "population by age and sex", # Population by age and sex - abridged 
                        locIds = "egypt",         # Egypt
                        locAreaTypeIds = "Whole area",   # Whole area 
                        subGroupIds = "Total or All groups",      # Total or All groups
                        isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(mixed)  # validate


# mixed with codes
mixed_codes <- get_recorddata(DataProcessIDs = 2,   # Census
                              indicatorTypeIds = 8, # Population by age and sex - abridged 
                              locIds = 818,         # Egypt
                              locAreaTypeIds = "Whole area",   # Whole area 
                              subGroupIds = "Total or All groups",      # Total or All groups
                              isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(mixed_codes)  # validate

# After changing the unpd server
options(unpd_server = "http://24.239.36.16:9654/un3/api/")

mixed_codes <- get_recorddata(DataProcessIDs = 2,   # Census
                              indicatorTypeIds = 8, # Population by age and sex - abridged 
                              locIds = 818,         # Egypt
                              locAreaTypeIds = "Whole area",   # Whole area 
                              subGroupIds = "Total or All groups",      # Total or All groups
                              isComplete = "Abridged")       # Age Distribution: Abridged

validate_read_API(mixed_codes)  # validate


test_that("get_recorddata returns error when setting wrong server", {
  # After changing the unpd server
  options(unpd_server = "http://0.0.0.0/")

  expect_error(
    suppressWarnings(
      get_recorddata(DataProcessIDs = 2,   # Census
                     indicatorTypeIds = 8, # Population by age and sex - abridged 
                     locIds = 818,         # Egypt
                     locAreaTypeIds = "Whole area",   # Whole area 
                     subGroupIds = "Total or All groups",      # Total or All groups
                     isComplete = "Abridged")       # Age Distribution: Abridged
    )
  )

  options(unpd_server = "http://24.239.36.16:9654/un3/api/")
})

test_that("get_recorddata with codes gives same output with strings", {
  expect_equal(X, Y)
  expect_equal(X, mixed)
  expect_equal(X, mixed_codes)
})

test_that("get_recorddata transforms TimeStart/TimeEnd to Date objects with DD/MM/YYYY formats", {
  res <- get_recorddata(DataProcessIDs = 9, # Register
                        startYear = 1920,
                        endYear = 2020,
                        indicatorTypeIds = 14, # Births by sex
                        isComplete = 2, # Total
                        locIds = 28, # Antigua and Barbuda
                        locAreaTypeIds = 2, # Whole area
                        subGroupIds = 2) # Total

  expect_type(res$TimeStart, "character")
  expect_type(res$TimeEnd, "character")

  # Here I'm testing that days, months and years have 2, 2 and 4
  # digits. The total is 8 plus the two slashes. Here we make sure
  # that we always have 10 characters.
  expect_equal(10, unique(nchar(res$TimeStart)))
  expect_equal(10, unique(nchar(res$TimeEnd)))

})


test_that("Looking up wrong input throws errors in get_recorddata", {
  expect_error(get_recorddata(locIds = "Wrong country"),
               regexp = "Location(s) 'Wrong country' not found. Check get_locations()",
               fixed = TRUE)

  expect_error(get_recorddata(indicatorTypeIds = "Wrong"),
               regexp = "Location(s) 'Wrong' not found. Check get_indicators()",
               fixed = TRUE)

  expect_error(get_recorddata(subGroupIds = "Wrong"),
               regexp = "Location(s) 'Wrong' not found. Check get_subgroups()",
               fixed = TRUE)

  expect_error(get_recorddata(isComplete = "Wrong"),
               regexp = "IsComplete does not accept string 'Wrong'. Only 'abridged', 'complete', 'total'.",
               fixed = TRUE)
})

ids <- "35444654"
res <- extract_data(ids)
validate_read_API(res)

test_that("extract_data returns the correct data when link is too long", {
  test_res <- function(res, ids) {
    all(
      all(ids %in% res$PK_StructuredDataID),
      nrow(res) == length(ids),
      all(table(res$PK_StructuredDataID) == 1)
    )
  }

  tst <- read_API("structureddatacriteria",
                  save = FALSE,
                  locIds = 4, # Afghanistan
                  indicatorIDs = c(60, 58), # Two indicators
                  includeDataIDs = "true"
                  )

  # Try reading different chunks of all codes
  # to make sure that the function can handle
  # reading different chunks of codes
  all_codes <- strsplit(tst$StructuredDataIDs, ",")[[1]]
  indices_test <- c(1, 50, 200, 201, 501, length(all_codes))
  all_test <-
    vapply(indices_test, function(i) {
      ids <- all_codes[1:i]
      res <- extract_data(ids)
      test_res(res, ids)
    }, logical(1))

  expect_true(all(all_test))
})

# The res data frame is resued from above
test_that("extract_data correctly formats TimeStart/TimeEnd to format DD/MM/YYYY", {
  expect_type(res$TimeStart, "character")
  expect_type(res$TimeEnd, "character")

  # Here I'm testing that days, months and years have 2, 2 and 4
  # digits. The total is 8 plus the two slashes. Here we make sure
  # that we always have 10 characters.
  expect_equal(10, unique(nchar(res$TimeStart)))
  expect_equal(10, unique(nchar(res$TimeEnd)))

})

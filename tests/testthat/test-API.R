test_that("The linkGenerator() works fine", {
  L <- linkGenerator(
    type = "structureddatarecords",
    locIds = 4,
    indicatorTypeIds = 8,
    dataProcessTypeIds = c(2, 6),
    verbose = FALSE
  )

  expect_output(print(L)) # 1. Always expect an output;
  expect_true(is.character(L)) # 2. The output is of the class "character";
  expect_error(linkGenerator(wrong_argument = 1)) # 3. Does not work with whatever argument in "...";
  expect_error(linkGenerator(type = "countryy")) # 4. Is sensitive to typos;
  expect_equal(length(strsplit(L, split = " ")[[1]]), 1) # 5. Expect no spaces in the string.
})


## Test API functions
validate_read_API <- function(Z) {
  test_that("The read_API works fine", {
    expect_output(print(Z)) # 1. Always expect an output;
    expect_true(is.data.frame(Z)) # 2. The output is of the class "data.frame";
    expect_true(ncol(Z) >= 2) # 3. The output has al least 2 columns;
    expect_false(any(is.null(colnames(Z)))) # 4. All columns have names;
    expect_true(nrow(Z) >= 1) # 5. The output has at least 1 rows
  })
}

validate_recordddata <- function(x) {
  validate_read_API(x) # validate
  # Test there aren't any NA in DataCatalogID
  expect_true(!any(is.na(x$DataCatalogID)))
}

# ------------------------------------------
D <- get_dataprocesstype()
validate_read_API(D) # validate

# ------------------------------------------
D <- get_datacatalog()
validate_read_API(D) # validate

D <- get_datacatalog(isSubnational = TRUE)
validate_read_API(D) # validate

D <- get_datacatalog(isSubnational = FALSE)
validate_read_API(D) # validate

# ------------------------------------------
S <- get_subgroups(
  indicatorTypeIds = 8, # Population by age and sex indicator;
  locIds = 818, # Egypt
  isComplete = 0
)
validate_read_API(S) # validate

# ------------------------------------------
L <- get_locations(
  addDefault = "false",
  includeDependencies = "false",
  includeFormerCountries = "false"
)
validate_read_API(L) # validate

# ------------------------------------------
P <- get_locationtypes(
  indicatorTypeIds = 8,
  locIds = 818,
  isComplete = 0
)
validate_read_API(P) # validate

# ------------------------------------------
IT <- get_indicatortypes(addDefault = "false")
validate_read_API(IT) # validate

# ------------------------------------------
IT <- get_iitypes(addDefault = "false")
validate_read_API(IT) # validate

# ------------------------------------------
I <- get_indicators(addDefault = "false")
validate_read_API(I) # validate

# ------------------------------------------
## TODO: Fix this test
## I <- get_datasources()
## validate_read_API(I)  # validate

# ------------------------------------------
G <- get_seriesdata(
  dataProcessTypeIds = 2,
  indicatorTypeIds = 8,
  isComplete = 0,
  locIds = 4,
  locAreaTypeIds = 2,
  startYear = 1950,
  subGroupIds = 2
)

validate_read_API(G) # validate

# ------------------------------------------

test_that("get_iitypes can subset correctly", {
  # No need to test each argument separately
  # otherwise the tests run the risk of running for
  # longer and longer.

  # Here I'm testing all argument combined. If they
  # work, it should be the same
  x <- get_iitypes(
    componentIds = 4,
    indicatorTypeIds = 38,
    indicatorIds = 323
  )
  expect_equal(unique(x[["IndicatorType.ComponentID"]]), 4)
  expect_equal(unique(x[["IndicatorType.PK_IndicatorTypeID"]]), 38)
  expect_equal(unique(x[["PK_IndicatorID"]]), 323)
})


# ------------------------------------------
X <- get_recorddata(
  dataProcessTypeIds = 2, # Census
  indicatorTypeIds = 8, # Population by age and sex - abridged
  locIds = 818, # Egypt
  locAreaTypeIds = 2, # Whole area
  subGroupIds = 2, # Total or All groups
  isComplete = 0
) # Age Distribution: Abridged

validate_recordddata(X) # validate

# Check whether it successfully accepts strings rather than codes
Y <- get_recorddata(
  dataProcessTypeIds = "Census", # Estimate
  indicatorTypeIds = "Population by age and sex", # Population by age and sex - abridged
  locIds = "Egypt", # Egypt
  locAreaTypeIds = "Whole area", # Whole area
  subGroupIds = "Total or All groups", # Total or All groups
  isComplete = "Abridged"
) # Age Distribution: Abridged

validate_recordddata(Y)

# Check whether it successfully mixed cases
mixed <- get_recorddata(
  dataProcessTypeIds = "census", # Estimate
  indicatorTypeIds = "population by age and sex", # Population by age and sex - abridged
  locIds = "egypt", # Egypt
  locAreaTypeIds = "Whole area", # Whole area
  subGroupIds = "Total or All groups", # Total or All groups
  isComplete = "Abridged"
) # Age Distribution: Abridged

validate_recordddata(mixed) # validate

# Check whether we can translate with dataProcessIds
mixed_dataid <- get_recorddata(
  dataProcessIds = "Population and Housing Census",
  startYear = 1920,
  endYear = 2020,
  indicatorIds = 58,
  isComplete = 0,
  locIds = 4,
  locAreaTypeIds = 2,
  subGroupIds = 2
)

validate_recordddata(mixed_dataid) # validate

# mixed with codes
mixed_codes <- get_recorddata(
  dataProcessTypeIds = 2, # Census
  indicatorTypeIds = 8, # Population by age and sex - abridged
  locIds = 818, # Egypt
  locAreaTypeIds = "Whole area", # Whole area
  subGroupIds = "Total or All groups", # Total or All groups
  isComplete = "Abridged"
) # Age Distribution: Abridged

validate_recordddata(mixed_codes) # validate


## For dataTypeGroupIds

# Check that it translates
chr_id <-
  get_recorddataadditional(
    dataTypeGroupIds = "Direct",
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

validate_recordddata(chr_id)

# Check that ti works with an id
num_id <-
  get_recorddataadditional(
    dataTypeGroupIds = 3,
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

validate_recordddata(num_id)

## For dataTypeGroupId2s

# Check that it translates
chr_id <-
  get_recorddataadditional(
    dataTypeGroupId2s = "Population (sample tabulation)",
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

validate_recordddata(chr_id)

num_id <-
  get_recorddataadditional(
    dataTypeGroupId2s = 11,
    indicatorTypeIds = 8,
    isComplete = 0,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2
  )

validate_recordddata(num_id)

# After changing the unpd server
options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")

mixed_codes <- get_recorddata(
  dataProcessTypeIds = 2, # Census
  indicatorTypeIds = 8, # Population by age and sex - abridged
  locIds = 818, # Egypt
  locAreaTypeIds = "Whole area", # Whole area
  subGroupIds = "Total or All groups", # Total or All groups
  isComplete = "Abridged"
) # Age Distribution: Abridged

validate_recordddata(mixed_codes) # validate


test_that("get_recorddata returns error when setting wrong server", {
  # After changing the unpd server
  options(unpd_server = "http://0.0.0.0/")

  expect_error(
    suppressWarnings(
      get_recorddata(
        dataProcessTypeIds = 2, # Census
        indicatorTypeIds = 8, # Population by age and sex - abridged
        locIds = 818, # Egypt
        locAreaTypeIds = "Whole area", # Whole area
        subGroupIds = "Total or All groups", # Total or All groups
        isComplete = "Abridged"
      ) # Age Distribution: Abridged
    )
  )

  options(unpd_server = "https://popdiv.dfs.un.org/DemoData/api/")
})

test_that("get_recorddata with codes gives same output with strings", {
  X <- X[order(X$StructuredDataID), ]
  Y <- Y[order(Y$StructuredDataID), ]
  mixed <- mixed[order(mixed$StructuredDataID), ]
  mixed_codes <- mixed_codes[order(mixed_codes$StructuredDataID), ]

  row.names(X) <- NULL
  row.names(Y) <- NULL
  row.names(mixed) <- NULL
  row.names(mixed_codes) <- NULL

  expect_equal(X, Y)
  expect_equal(X, mixed)
  expect_equal(X, mixed_codes)
})


validate_date <- function(res) {
  expect_type(res$TimeStart, "character")
  expect_type(res$TimeEnd, "character")

  # Here I'm testing that days, months and years have 2, 2 and 4
  # digits. The total is 8 plus the two slashes. Here we make sure
  # that we always have 10 characters.
  expect_equal(10, unique(nchar(res$TimeStart)))
  expect_equal(10, unique(nchar(res$TimeEnd)))

  # Test that the structure is 2 digits / 2 digits / 4 digits
  expect_true(all(grepl("[0-9]{2}/[0-9]{2}/[0-9]{4}", res$TimeStart)))
  expect_true(all(grepl("[0-9]{2}/[0-9]{2}/[0-9]{4}", res$TimeEnd)))
}

## TODO: This is failing due to the new unpd server. Fix this once
## Kyaw Kyaw does the correct migration.
## test_that("get_recorddata transforms TimeStart/TimeEnd to Date objects with DD/MM/YYYY formats", {
##   res <- get_recorddata(dataProcessTypeIds = 9, # Register
##                         startYear = 1920,
##                         endYear = 2020,
##                         indicatorTypeIds = 14, # Births by sex
##                         isComplete = 2, # Total
##                         locIds = 28, # Antigua and Barbuda
##                         locAreaTypeIds = 2, # Whole area
##                         subGroupIds = 2) # Total
##   validate_date(res)
## })


test_that("get_recorddata and get_recorddataadditional transform Name columns to labels", {
  res <- get_recorddata(
    dataProcessTypeIds = 2, # Census
    indicatorTypeIds = 8, # Population by age and sex - abridged
    locIds = 818, # Egypt
    locAreaTypeIds = 2, # Whole area
    subGroupIds = 2, # Total or All groups
    isComplete = 0,
    collapse_id_name = TRUE
  ) # Age Distribution: Abridged

  res_additional <- get_recorddataadditional(
    dataProcessTypeIds = 2, # Census
    indicatorTypeIds = 8, # Population by age and sex - abridged
    locIds = 818, # Egypt
    locAreaTypeIds = 2, # Whole area
    subGroupIds = 2, # Total or All groups
    isComplete = 0,
    collapse_id_name = TRUE
  ) # Age Distribution: Abridged


  subset_names <- res[names(values_env$id_to_fact)]
  subset_names_additional <- res_additional[names(values_env$id_to_fact)]

  expect_true(
    all(
      vapply(subset_names, function(x) inherits(x, "haven_labelled"),
        FUN.VALUE = logical(1)
      )
    )
  )

  expect_true(
    all(
      vapply(subset_names_additional, function(x) inherits(x, "haven_labelled"),
        FUN.VALUE = logical(1)
      )
    )
  )


  ## TODO
  ## You might want to add another test that checks that the ID
  ## in the label is a numeric and and value is a character
  ## to make sure you never mix them up.
})

test_that("get_recorddata and get_recorddataadditional keep the correct columns when collapse_id_name is set to different values", {


  collapse_opts <- c(TRUE, FALSE)
  cols_available <-
    list(
      setdiff(values_env$col_order, values_env$id_to_fact),
      values_env$col_order
    )

  for (ind in seq_along(cols_available)) {
    res <- get_recorddata(
      dataProcessTypeIds = 2, # Census
      indicatorTypeIds = 8, # Population by age and sex - abridged
      locIds = 818, # Egypt
      locAreaTypeIds = 2, # Whole area
      subGroupIds = 2, # Total or All groups
      isComplete = 0,
      includeUncertainty = TRUE,
      collapse_id_name = collapse_opts[ind]
    ) # Age Distribution: Abridged

    expect_equal(colnames(res), cols_available[[ind]])

    res <- get_recorddataadditional(
      dataProcessTypeIds = 2, # Census
      indicatorTypeIds = 8, # Population by age and sex - abridged
      locIds = 818, # Egypt
      locAreaTypeIds = 2, # Whole area
      subGroupIds = 2, # Total or All groups
      isComplete = 0,
      includeUncertainty = TRUE,
      collapse_id_name = collapse_opts[ind]
    ) # Age Distribution: Abridged

    expect_equal(colnames(res), cols_available[[ind]])
  }

})


test_that("Looking up wrong input throws errors in get_recorddata", {
  expect_error(get_recorddata(locIds = "Wrong country"),
    regexp = "Location(s) 'Wrong country' not found. Check get_locations()",
    fixed = TRUE
  )

  expect_error(get_recorddata(indicatorTypeIds = "Wrong"),
    regexp = "Location(s) 'Wrong' not found. Check get_indicatortypes()",
    fixed = TRUE
  )

  expect_error(get_recorddata(subGroupIds = "Wrong"),
    regexp = "Location(s) 'Wrong' not found. Check get_subgroups()",
    fixed = TRUE
  )

  expect_error(get_recorddata(isComplete = "Wrong"),
    regexp = "IsComplete does not accept string 'Wrong'. Only 'abridged', 'complete', 'total'.",
    fixed = TRUE
  )
})

ids <- "183578537"
res <- extract_data(ids)
validate_read_API(res)

## TODO: Fix this
## test_that("extract_data returns the correct data when link is too long", {
##   test_res <- function(res, ids) {
##     all(
##       all(ids %in% res$StructuredDataID),
##       nrow(res) == length(ids),
##       all(table(res$PK_StructuredDataID) == 1)
##     )
##   }

##   tst <- read_API("structureddatacriteria",
##                   save_file = FALSE,
##                   locIds = 4, # Afghanistan
##                   indicatorIds = c(60, 58), # Two indicators
##                   includeDataIDs = "true"
##                   )

##   # Try reading different chunks of all codes
##   # to make sure that the function can handle
##   # reading different chunks of codes
##   all_codes <- strsplit(tst$StructuredDataIDs, ",")[[1]]
##   indices_test <- c(1, 50, 200, 201, 501, length(all_codes))
##   all_test <-
##     vapply(indices_test, function(i) {
##       ids <- all_codes[1:i]
##       res <- extract_data(ids)
##       test_res(res, ids)
##     }, logical(1))

##   expect_true(all(all_test))
## })

## # The res data frame is resued from above
## test_that("extract_data correctly formats TimeStart/TimeEnd to format DD/MM/YYYY", {
##   expect_type(res$TimeStart, "character")
##   expect_type(res$TimeEnd, "character")

##   # Here I'm testing that days, months and years have 2, 2 and 4
##   # digits. The total is 8 plus the two slashes. Here we make sure
##   # that we always have 10 characters.
##   expect_equal(10, unique(nchar(res$TimeStart)))
##   expect_equal(10, unique(nchar(res$TimeEnd)))
## })

## TODO: This is failing due to the new unpd server. Fix this once
## Kyaw Kyaw does the correct migration.
## test_that("isComplete is set to 'Total' by default", {
##   myLocations <- 28
##   # A request without specifying `isComplete`
##   births <- get_recorddata(dataProcessTypeIds = 9,
##                            startYear = 1920,
##                            endYear = 2020,
##                            indicatorTypeIds = 14,
##                            locIds = myLocations,
##                            locAreaTypeIds = 2,
##                            subGroupIds = 2)

##   # Same request specifying that it's complete is set to 'Total' (2)
##   births_iscomplete <- get_recorddata(dataProcessTypeIds = 9,
##                                       startYear = 1920,
##                                       endYear = 2020,
##                                       indicatorTypeIds = 14,
##                                       isComplete = 2,
##                                       locIds = myLocations,
##                                       locAreaTypeIds = 2,
##                                       subGroupIds = 2)

##   # Both results are the same
##   expect_identical(births, births_iscomplete)
## })

test_that("get_recorddata grabs uncertainty columns when includeUncertainty = TRUE", {
  uncertainty_cols <- c(
    "HasUncertaintyRecord",
    "StandardErrorValue",
    "ConfidenceInterval",
    "ConfidenceIntervalLowerBound",
    "ConfidenceIntervalUpperBound"
  )

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0
  )

  # If includeUncertainty is NULL (default), the columns
  # are NOT included.
  expect_false(all(uncertainty_cols %in% names(X)))

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0,
    includeUncertainty = FALSE
  )

  # If includeUncertainty is FALSE, the columns
  # are NOT included.
  expect_false(all(uncertainty_cols %in% names(X)))

  X <- get_recorddata(
    dataProcessTypeIds = 2,
    indicatorTypeIds = 8,
    locIds = 818,
    locAreaTypeIds = 2,
    subGroupIds = 2,
    isComplete = 0,
    includeUncertainty = TRUE
  )

  # If includeUncertainty is TRUE, the columns
  # ARE included.
  expect_true(all(uncertainty_cols %in% names(X)))
})

test_that("Checks that SeriesID is a character vector", {
  # This is due to Patrick's request that this should never be a numeric
  # due to loss of precision when grabbing from fromJSON.

  expect_type(X$SeriesID, "character")
})

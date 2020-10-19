# Make sure to install both

# devtools::install_github("timriffe/DDSQLtools")
library(DDSQLtools)

# install.packages("tictoc")
library(tictoc)

############################# issue 1 #########################################
###############################################################################

# Issue 1: Patrick tried downloading all of the countries (42)
# in a single call to the API and it crashed.

# Jorge: This was because there is a limit to the length
# of the API string. We can only query a maximum of 30 countries. I've
# added examples in the vignette showing that when exceeding 30 countries
# users should use `lapply` or the `tidyverse` equivalent. Also added
# this in the documentation of `get_recorddata`.
# TODO: when 30 is confirmed, add this number in the documentation
# of get_recorddata and in the vignette

myLocations <- c(28, 492, 570, 674, 308, 96, 196, 8, 376, 662, 670, 642, 84,
                 188, 442, 100, 192, 170, 414, 616, 320, 480, 218, 818, 222,
                 300, 558, 40, 52, 348) # 470, 620, 702, 858, 56, 780, 388, 246, 352, 591, 144, 862)
tic()
deaths <- get_recorddata(dataProcessTypeIds = "Register",
                         indicatorTypeIds = "Deaths by age and sex",
                         locIds = myLocations,
                         locAreaTypeIds = "Whole area",
                         subGroupIds = "Total or All groups",
                         isComplete = "Abridged",
                         startYear = 1920,
                         endYear = 2020)
toc()

deaths2 <- get_recorddata(dataProcessTypeIds = 9, startYear = 1920, endYear = 2020, indicatorTypeIds = 20, isComplete = 0, locIds = c(28, 492, 570, 674, 308, 96, 196, 8, 376, 662, 670, 642, 84, 188, 442, 100, 192, 170, 414, 616, 320, 480, 218, 818, 222, 300, 558, 40, 52, 348, 470), locAreaTypeIds = 2, subGroupIds = 2)

# Doesn't work (only adds one location)
myLocations <- c(28)#492,570,674,308,96,196,8,376,662,670,642,84,188,442,100,192,170,414,616,320,480,218,818,222,300,558,40,52,348,470,591)

deaths <- get_recorddata(dataProcessTypeIds = "Register",
                         indicatorTypeIds = "Deaths by age and sex",
                         locIds = myLocations,
                         locAreaTypeIds = "Whole area",
                         subGroupIds = "Total or All groups",
                         isComplete = "Abridged",
                         startYear = 1920,
                         endYear = 2020)


############################# issue 2 #########################################
###############################################################################

# Issue 2: Patrick tried downloading all of the countries
# in issue 1 (around 42 countries) in a loop and it takes
# a long time.

# Jorge: This was because we were using a loop rather
# than an lapply call. I've added examples on the vignette
# on how to do this with base R and tidyverse examples,
# giving recommendations to the users to use lapply
# whenever the number of countries exceeds 30. In my benchmarks,
# it takes about 26 minutes rather than half a day.

# All your locations
myLocations <- c(28, 492, 570, 674, 308, 96, 196, 8, 376, 662, 670, 642, 84,
                 188, 442, 100, 192, 170, 414, 616, 320, 480, 218, 818, 222,
                 300, 558, 40, 52, 348, 470, 620, 702, 858, 56, 780, 388, 246,
                 352, 591, 144, 862)

tic()
# Loop through each location with `lapply`
deaths <- lapply(myLocations, function(x) {
  # Measure time of beginning
  tic()

  res <- get_recorddata(dataProcessTypeIds = 9,
                        startYear = 1920,
                        endYear = 2020,
                        indicatorTypeIds = 20,
                        isComplete = 0,
                        locIds = x,
                        locAreaTypeIds = 2,
                        subGroupIds = 2)

  # Print time it took to make the request
  cat("Country", x, ":")
  toc()

  # return the result
  res
})
toc()

# It took 26 minutes

# Merge all separate country data frames into
# one data frame.
deaths_bind <- do.call(rbind, deaths)
deaths_bind

############################# issue 3 #########################################
###############################################################################

# Issue 3: Patrick says we should be able to request
# the data without specifying isComplete.

# Jorge/Dennis: We've fixed this. isComplete is now set
# to 'Total' by default, so there's no need to specify it.

n_chunks <- 3
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)

# A request without specifying `isComplete`
births <- lapply(cnty_groups, function(x) {

  tic()
  res <- get_recorddata(dataProcessTypeIds = 9,
                        startYear = 1920,
                        endYear = 2020,
                        indicatorTypeIds = 14,
                        locIds = x,
                        locAreaTypeIds = 2,
                        subGroupIds = 2)
  cat("Country", x, ": ")
  toc()

  res
})

# Same request specifying that it's complete
births_iscomplete <- lapply(cnty_groups, function(x) {

  tic()
  res <- get_recorddata(dataProcessTypeIds = 9,
                        startYear = 1920,
                        endYear = 2020,
                        indicatorTypeIds = 14,
                        isComplete = 2,
                        locIds = x,
                        locAreaTypeIds = 2,
                        subGroupIds = 2)
  cat("Country", x, ": ")
  toc()

  res
})

# Each request takes about 2 minutes
births_bind <- do.call(rbind, births)
births_iscomplete_bind <- do.call(rbind, births_iscomplete)

# Both results are the same
identical(births_bind, births_iscomplete_bind)

head(births_bind)

## get_rd <- function(save_file = FALSE, verbose = TRUE, ...) {
##   read_API("structuredDataRecordsAdditional",
##            save_file = save_file,
##            verbose = verbose,
##            ...)
## }

## res <- get_rd(dataProcessTypeIds = 9,
##               startYear = 1920,
##               endYear = 2020,
##               indicatorTypeIds = 14,
##               isComplete = 2,
##               locIds = 28,
##               locAreaTypeIds = 2,
##               subGroupIds = 2)

############################# issue 4 #########################################
###############################################################################

# Issue 4: get_indicators()
# Should return the list of IndicatorID, IndicatorTypeID is something else to group IndicatorID.
# But some of this convention currently is probably related to the API itself with Dennis, so we need to review/discuss about it.
# Actually the list of fields to get back should be:
# ComponentID, ComponentName, IndicatorTypeID (not labeled PK_IndicatorTypeID), IndicatorTypeName, IndicatorID IndicatorName, IndicatorShortName, SortOrder, Description
# IndicatorShortName and Description  right now are NA!

# Jorge:
# Dennis seems to have implemented a fix to do this in the new endpoint
# indicatorindicatortypes but it doesn't work.
# http://24.239.36.16:9654/un3/api/indicatorindicatortypes?

# Jorge:
# Dennis fixed indicatorindicatortypes and now it works.
# Below some examples:

# Get all indicatorid and indicatortypeids
res1 <- as_tibble(get_iitypes())

# Filter the indicatorTypeIds
res1 <- as_tibble(get_iitypes(indicatorTypeIds = 6))

# Filter the indicatorIds
res1 <- as_tibble(get_iitypes(componentIds = 4))

############################# issue 5 #########################################
###############################################################################

# Issue 5: Patrick says that we should be able to pass both the IndicatorID
# and the IndicatorTypeID to structureddatarecords.

# Jorge: If the indicatorId only differentiates between abridged and complete,
# I think I can build some internally to search for the indicatorIds based
# on the isComplete parameter. However, if indicatorIds is expected to
# have more categories or more flexibility, then we'll probably need
# a new endpoint as Dennis mentions in the end of his comment.

# Internal for Jorge:
# get_indicators()[c("Name", "IndicatorTypeID")]
# The only difference between indicators is whether it's abridged or complete
# Except for Probability of dying. If it's still complete or abridged,
# you could create a look such that whe the indicatorid is supplied,
# you can translate it to either abridged or complete and pass it to
# isComplete. However, we need to confirm whether the only difference
# is abridged and complete

# TODO: send an example where all of these fields are empty

############################# issue 6 #########################################
###############################################################################

Y <- get_recorddata(dataProcessTypeIds = "Census",
                   indicatorTypeIds = 8, # and support numeric of string names
                   locIds = "egypt", # all arguments are case insensitive
                   locAreaTypeIds = "Whole area",
                   subGroupIds = "Total or All groups",
                   isComplete = "Abridged")

# Where is created SeriesID? What does it mean? What is it really measuring and is
# stable?

# For this we need a call because it's a bit unclear to me.

# Dennis just needs to delete the two internal keys the ID and the PK_ID

############################# issue 7 #########################################
###############################################################################

# This already comes form the API as 0. For example, search for "DataCatalogID"
# in this request:
# http://24.239.36.16:9654/un3/api/structureddatarecords?dataProcessTypeIds=9&startYear=1920&endYear=2020&indicatorTypeIds=14&isComplete=2&locIds=28&locAreaTypeIds=2&subGroupIds=2

# Jorge: This is already fixed. I've added a test checking that
# DataCatalogID is never NA

############################# issue 8 #########################################
###############################################################################

# Issue 8: TimeStart and TimeEnd should be date objects
# with consistent format (DD/MM/YYY).

# Jorge: This is now implemented in `get_recorddata` and added
# to it's documentation. Any user interested in this can see
# the format of the data by just typing ?get_recorddata
# Just want to confirm with Dennis that the format that is coming
# from the API is YYYY-MM-DD

res <- get_recorddata(dataProcessTypeIds = "Census",
                      startYear = 1920,
                      endYear = 2020,
                      indicatorTypeIds = 7, 
                      isComplete = "Total",
                      locIds = "Denmark",
                      locAreaTypeIds = "Whole area",
                      subGroupIds = "Total or All groups")

unique(res$TimeStart)
unique(res$TimeEnd)

############################# issue 9 #########################################
###############################################################################

# Issue 9: TimeStart and TimeEnd sometimes have the same
# date and TimeDuration = 0. We will correct our SQL database

# Jorge: As of 13th of May 2020, this hasn't been fixed.
# See the result from below:

res <- get_recorddata(dataProcessTypeIds = "Census",
                      startYear = 1920,
                      endYear = 2020,
                      indicatorTypeIds = 7, 
                      isComplete = "Total",
                      locIds = "Denmark",
                      locAreaTypeIds = "Whole area",
                      subGroupIds = "Total or All groups")

head(res[c("TimeStart", "TimeEnd", "TimeDuration")])

############################# issue 10 ########################################
###############################################################################

# Issue 10: The order of the columns is not consistent.

# Jorge: I've remove most _ID columns to their name columns
# which are now supposed to be factors. This reduced
# the total number of columns to 70. Moreover,
# the order of columns now reflects the order
# in the frontend API. Let me know if the order
# can be improved.

# For example
res <- get_recorddata(dataProcessTypeIds = "Census",
                      startYear = 1920,
                      endYear = 2020,
                      indicatorTypeIds = 7, 
                      isComplete = "Total",
                      locIds = "Denmark",
                      locAreaTypeIds = "Whole area",
                      subGroupIds = "Total or All groups")

# IndicatorName and IndicatorNameID are now summarized into
# indicatorName as a 'haven labelled factor'. 
res$IndicatorName

############################# issue 11 ########################################
###############################################################################

# Issue 11: convention for fieldnames and their use in the various R function
# options/parameters: I think that right now you follow the style provided
# through the SQL/API, right?  If so it is OK.

# Jorge: Yes, we're following the API conventions. If needed, let us know and
# we'll switch.

# TODO: when Dennis adds the sortOrder, just add the sortorder after
# the equivalent column name. The most elegant thing would be to
# put them as an extra attribute into the haven factor.

############################# issue 12 ########################################
###############################################################################

# Issue 12: Get from SQL/API extra set of SortOrder fields for selected set of
# fields:
# * Sex
# * Age
# * DataProcess
# * DataProcessType
# * DataStatus
# * StatisticalConcept
# * DataReliability
# * DataType
# * DataSource

# Jorge/Dennis: Dennis asks for which specific structured data endpoints.
# I think this applies to all structured data endpoints but Patrick
# might have in mind particular end points.

############################# Summary of points ###############################
###############################################################################

## I've made a full recap of all 12 points and flagged the complete ones and the incomplete ones:

## Issue 1: Fixed
##  * Patrick tried downloading all of the countries (42) in a single call to the API and it crashed.
## * Jorge: This was because there is a limit to the length of the API string. We can only query a maximum of 30 countries. I've added examples in the vignette showing that when exceeding 30 countries users should use `lapply` or the `tidyverse` equivalent. Also added this in the documentation of `get_recorddata`.

## Issue 2: Fixed* Patrick tried downloading all of the countries in issue 1 (around 42 countries) in a loop and it takes a long time.
## * Jorge: This was because we were using a loop rather than an lapply call. I've added examples on the vignette on how to do this with base R and tidyverse examples, giving recommendations to the users to use lapply whenever the number of countries exceeds 30. In my benchmarks, it takes about 26 minutes rather than half a day.

## Issue 3: Fixed* Patrick says we should be able to request the data without specifying isComplete.
## * Jorge: We've fixed this. isComplete is now set to 'Total' by default, so there's no need to specify it.

## Issue 4: TODO
## * Patrick says get_indicators() should return the list of IndicatorID as IndicatorTypeID is something else to group IndicatorID. But some of this convention currently is probably related to the API itself with Dennis, so we need to review/discuss about it. Actually the list of fields to get back should be: ComponentID, ComponentName, IndicatorTypeID (not labeled PK_IndicatorTypeID), IndicatorTypeName, IndicatorID IndicatorName, IndicatorShortName, SortOrder, Description IndicatorShortName and Description  right now are NA!
## * Jorge: Dennis seems to have implemented a fix to do this in the new endpoint indicatorindicatortypes but it's not up to date right now: http://24.239.36.16:9654/un3/api/indicatorindicatortypes?

## Issue 5: TODO* Patrick says that we should be able to pass both the IndicatorID and the IndicatorTypeID to structureddatarecords.
## * Jorge: If the indicatorId only differentiates between abridged and complete, I think I can build some internally to search for the indicatorIds based on the isComplete parameter. However, if indicatorIds is expected to have more categories or more flexibility, then we'll probably need a new endpoint as Dennis mentioned.

## Issue 6: TODO
## * Patrick: The recordset returned in R includes some fields that either should not be there, or should be labelled differently, and in any case currently have only 0 while they should have real values:
## agesort id PK_SubGroupID PK_SubGroupTypeID PK_DataProcessID PK_DataProcessTypeID
## For ages, SQL has already a field for SortOrder.
## For id, it is unclear what this is about. I presume it refers to dynamic IDs created by the SQL/API. Currently it is only with 0 in R, and in anycase it shoud be a more meaningful name if it refers to a series ID, may be SeriesID.  Ideally it would be best if such SeriesID would be stable and between different queries return the same ID for the same specific combination of keys.  I don’t think it is currently the case.
## PK_SubGroupID PK_SubGroupTypeID PK_DataProcessTypeID: these fields are unenecssary since you have the right version without PK_ prefix already with the right values
## DataProcessID is missing and PK_DataProcessID currently has 0 and should not appear.
## All SQL fields with PK_ prefix must be simplified without PK_ prefix.
## * Jorge: For this one we need to discuss because it's not clear what the next step to fix this is. We probably need to iterate what the best solution is for the design of the API.

## Issue 7: TODO
## Patrick: DataCatalogID is only currently returning 0 which should never happen.  The value should always be > 0.Jorge: Same as issue 6. Dennis has an extensive response which suggests that this more related to the design of the API. We need to discuss this in our talk.

## Issue 8: Fixed
## * Patrick: TimeStart and TimeEnd should be date objects with consistent format (DD/MM/YYY).
## * Jorge: This is now implemented in `get_recorddata` and added to it's documentation. Any user interested in this can see the format of the data by just typing ?get_recorddata. @Dennis, just to confirm 100%, the date format coming from the API is YYYY-MM-DD?
## Issue 9: TODOPatrick: TimeStart and TimeEnd sometimes have the same date and TimeDuration = 0. We will correct our SQL databaseJorge: As of today, this still happens.
## Issue 10: Fixed
## * Patrick: The order of the columns is not consistent.
## * Jorge: I've remove most _ID columns to their equivalent _Name columns which are now haven labelled factors. This reduced the total number of columns to 70. Moreover, the order of columns now reflects the order in the frontend API. Let me know if the order can be improved. I've included a section in the vignette of the package showing how to work with these factors to see the equivalent values/strings. These have examples in base R and `tidyverse`.
## Issue 11: FixedPatrick: convention for fieldnames and their use in the various R function options/parameters: I think that right now you follow the style provided through the SQL/API, right?  If so it is OK.
## Jorge: Yes, we're following the API conventions
## Issue 12: TODO
## Patric: We need from SQL/API extra set of SortOrder fields for selected set of fields:
## * Sex
## * Age
## * DataProcess
## * DataProcessType
## * DataStatus
## * StatisticalConcept
## * DataReliability
## * DataType
## * DataSource

## Jorge: Dennis asks which specific structured data endpoints need this. I think this applies to all structured data endpoints but Patrick might have in mind particular end points.


################## Patrick questions 24th August, 2020 ###########

# Install latest development
devtools::install_github("cimentadaj/DDSQLtools")

# 1. Do you think we can get in the output  of get_recorddata the field
# 'SeriesID' as long integer than in scientific notation.

# SeriesID is actually an integer. This is just a printing
# issue. If you run the options expression below you'll get integers printed
# without scientific notation
options(scipen = 9999)

Y <- get_recorddata(dataProcessTypeIds = "Census",
                    indicatorTypeIds = 8, # and support numeric of string names
                    locIds = "egypt", # all arguments are case insensitive
                    locAreaTypeIds = "Whole area",
                    subGroupIds = "Total or All groups",
                    isComplete = "Abridged")

head(Y$SeriesID)
head(Y["SeriesID"])

# 2. In the output recordset, can we also have AgeSort instead of 'agesort'

# Now available
head(Y$AgeSort)

# 3. With the example below if I use n_chunks <- 1 works but n_chunks > 1 fails
library(tictoc)
myLocations <-
  c(654, 660, 535, 92, 136, 212, 500, 652, 659, 663, 534, 796, 238, 584, 520,
    580, 585, 16, 184, 570, 772, 798, 876, 234, 833, 20, 292, 336, 674, 438,
    492, 60, 304, 666)

# Here replace with number of desired chunk of countries
n_chunks <- 3
chunk_groups <- rep(1:n_chunks, length.out = length(myLocations))
cnty_groups <- split(myLocations, chunk_groups)

# Loop through each location with `lapply`
myBirths <- lapply(cnty_groups, function(x) {
  # Measure time of beginning
  tic()

  res <- get_recorddata(dataProcessTypeIds = 9,
                        startYear = 1950,
                        endYear = 2020,
                        indicatorIds = 159,
                        isComplete = 2,
                        locIds = x,
                        locAreaTypeIds = 2,
                        subGroupIds = 2)

  res$DataCatalogNote <- as.character(res$DataCatalogNote)

  # Print time it took to make the request
  cat("Country", x, ": ")

  toc()
  # return the result
  res
})

## lab1 <- haven::labelled(c(2, 3), labels = c("Whatever" = 2, "Oh shit" = 3))
## lab2 <- haven::labelled(c(4, 5), labels = c("Oss" = 4, "ooss" = 5))
## lab3 <- haven::labelled(c(6, 7), labels = c("sppa" = 7, "uueaap" = 6))
## dt1 <- data.frame(col1 = as_factor(lab1))
## dt2 <- data.frame(col1 = as_factor(lab2))
## dt3 <- data.frame(col1 = as_factor(lab3))
## dplyr::bind_rows(list(dt1, dt2, dt3))$
## do.call(rbind, list(dt1, dt2, dt3))
## r1 <- head(myBirths[[1]]["LocName"])
## r2 <- head(myBirths[[2]]["LocName"])
## r3 <- head(myBirths[[3]]["LocName"])
## x <- haven::labelled(c(1, 2, 1, 2, 10), c(Yes = 1, No = 2, Unknown = 9, Refused = 10))

# Merge all separate country data frames into one data frame.
dplyr::bind_rows(myBirths2)
do.call(rbind, myBirths)

head(TBirths)

# Should I add these changes to the vignette?

# 4. I see also that various fields are not returned in the output recordset
# like LocID (right now only LocName), DataTypeID (right now only DataTypeName),
# DataProcessID (right now only DataProcess).  I would favor including these IDs
# also in the output recordset because we can use them for various purposes.
# But StaffMemberID is only an internal field that should not get returned.
# Idem for TimeReferenceID which is really only an internal ID.

# I included now
# LocID
# DataTypeID
all(c("LocID", "DataTypeID") %in% names(Y))

# And removed
# StaffMemberID
# TimeReferenceID
all(!c("StaffMemberID", "TimeReferenceID") %in% names(Y))

# Dennis, can you include also DataProcessID? It's not being
# returned in the API response

# 5. I would favor we include DataTypeID before DataTypeName
# And include in the output query these extra fields (Dennis: these
# are coming from rftDataTypeGroup2):
# DataTypeGroup2ID, DataTypeGroup2Name, DataTypeGroup2Sort

# I switched the order and DataTypeID now comes first than DataTypeName
# Dennis, can you include these three fields in the API response?

# 6. haven labels currently not working for DataProcess, but it works fine for
# LocID (as LocName), DataTypeID (as DataTypeName).
# For some reason I thought we were receiving the DataProcessID filed in the API
# response, but we're not. Dennis, can we include the DataProcessID
# in the API response? This is the same field as in point 4.

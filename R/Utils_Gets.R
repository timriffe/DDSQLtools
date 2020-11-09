#' API Link Generator Function
#'
#' @param server The path to the database. Check if the "unpd_server" option is
#' is set. If not, defaults to \code{"http://24.239.36.16:9654/un2/api/"}
#'
#' @param type Type of data. Various options are available.
#'
#' @param verbose Logical for whether to print the final translated call
#' to numeric arguments. If the translation of arguments take a lot of time,
#' it can have a substantial reduction in timing. This only makes sense if
#' the user provided values as strings which need translation (such as a
#' country name like 'Haiti' rather than its actually country code).
#'
#' @param ... Other arguments that might define the path to data. All arguments
#' accept a numeric code which is interpreted as the code of the specific
#' product requested. Alternatively, you can supply the equivalent product
#' name as a string which is case insensitive (see examples). Handle with
#' care, this is important! For a list of all options available, see the
#' parameters for each endpoint at http://24.239.36.16:9654/un3/swagger/ui/index#!/StructuredData/StructuredData_GetStructuredDataRecords
#'
#' @details The link generator is based on the structure of the database
#' created by Dennis Butler (in late 2018). To change the server used to make
#' the requests, set this at the beginning of your script:
#' options(unpd_server = "fill this out").
#'
#' When requesting data from the structured data format (usually called from
#' \code{\link{get_recorddata}}), the columns \code{TimeStart} and \code{TimeEnd}
#' are returned with format \code{DD/MM/YYYY}, where \code{DD} are days, \code{MM}
#' are months and \code{YYYY} are years.
#'
#' @examples
#' \dontrun{
#' # Link to country list
#' L1 <- linkGenerator(type = "locations",
#'                     addDefault = "false",
#'                     includeDependencies = "false",
#'                     includeFormerCountries = "false")
#' L1
#'
#' # Link to location types (for Egypt)
#' # With strings rather than codes
#' L2 <- linkGenerator(type = "locAreaTypes",
#'                     indicatorTypeIds = "Population by sex",
#'                     locIds = "Egypt",
#'                     isComplete = "Abridged")
#' L2
#'
#' # Link to subgroup types (for Egypt)
#' L3 <- linkGenerator(type = "subGroups",
#'                     indicatorTypeIds = 8,
#'                     locIds = 818,
#'                     isComplete = 0)
#' L3
#'
#' # Link to indicator list
#' L4 <- linkGenerator(type = "indicators",
#'                     addDefault = "false")
#' L4
#'
#' # Link to data process type list
#' L5 <- linkGenerator(type = "dataProcessTypes")
#' L5
#' }
#' @keywords internal
linkGenerator <- function(server = getOption("unpd_server", "http://24.239.36.16:9654/un3/api/"),
                          type,
                          verbose,
                          ...) {

  types <- c("ages",
             "openAges",
             "Component",
             "DataCatalog",
             "dataProcessTypes",
             "dataProcesses",
             "DataReliability",
             "DataSources",
             "DataSourceStatus",
             "DataSourceTypes",
             "DataStatus",
             "DataTypes",
             "DefaultKeys",
             "Indicators",
             "Indicatortypes",
             "IndicatorIndicatortypes",
             "locAreaTypes",
             "Locations",
             "PeriodGroups",
             "PeriodTypes",
             "Sex",
             "StatisticalConcepts",
             "StructuredData",
             "StructuredDataTable",
             "StructuredDataRecords",
             "structuredDataRecordsAdditional",
             "StructuredDataSeries",
             "structuredDataCriteria",
             "subGroups",
             "SubGroupTypes",
             "TimeReferences",
             # These are within UserUtility
             "dataEntryCount")

  type  <- match.arg(tolower(type), choices = tolower(types))
  query <- build_filter(..., verbose = verbose)
  link  <- utils::URLencode(paste0(server, type, query))
  link
}


#' Build the section of the path (link) responsible with filtering the data
#'
#' For a description of what each argument represents, see
#' http://24.239.36.16:9654/un3/swagger/ui/index#!/StructuredData/StructuredData_GetStructuredDataRecords
#'
#' @keywords internal
build_filter <- function(dataProcessIds = NULL,
                         dataProcessTypeIds = NULL,
                         dataSourceShortNames = NULL,
                         dataSourceYears = NULL,
                         startYear = NULL,
                         endYear = NULL,
                         AgeStart = NULL,
                         AgeEnd = NULL,
                         indicatorTypeIds = NULL,
                         indicatorIds = NULL,
                         componentIds = NULL,
                         isComplete = NULL,
                         isActive = NULL,
                         locIds = NULL,
                         ids = NULL,
                         locAreaTypeIds = NULL,
                         subGroupIds = NULL,
                         shortNames = NULL,
                         addDefault = NULL,
                         includeDependencies = NULL,
                         includeFormerCountries = NULL,
                         includeDataIDs = NULL,
                         includeUncertainty = NULL,
                         years = NULL,
                         verbose) {

  # Keep as list because unlisting multiple ids for a single
  # parameters separates them into different strings
  x <- as.list(environment())

  # Exclude verbose option
  x <- x[!names(x) == "verbose"]

  # For later, to print the translated code query
  # so the user gets the faster request
  x_str <- x[c("locIds", "indicatorTypeIds", "isComplete", "subGroupIds", "locAreaTypeIds", "dataProcessIds", "dataProcessTypeIds")]
  any_str <- any(vapply(x_str, is.character, FUN.VALUE = logical(1)))

  lookupParams <- list("locIds" = lookupLocIds,
                       "indicatorTypeIds" = lookupIndicatorIds,
                       "isComplete" = lookupIsCompleteIds,
                       "subGroupIds" = lookupSubGroupsIds)

  # Iterative over each lookupParams and apply their corresponding lookup
  # function to translate strings such as Germany to the corresponding code.
  # Only available for the names in lookupParams
  x[names(lookupParams)] <- mapply(
    function(fun, vec) fun(vec),
    lookupParams,
    x[names(lookupParams)]
  )

  # Here we need a separate call to the same thing bc
  # I reuse the translated parameters defined above
  # to make queries in the endpoints below
  extraParams <- list("locAreaTypeIds" = lookupAreaTypeIds,
                      "dataProcessIds" = lookupDataProcessIds,
                      "dataProcessTypeIds" = lookupDataProcessTypeIds)

  x[names(extraParams)] <- mapply(
    function(fun, vec, ...) fun(vec, ...),
    extraParams,
    x[names(extraParams)],
    # I pass the already translated parameter list
    # to avoid retranslating stuff like locations, etc...
    # This can save time in querying API
    MoreArgs = list(paramList = x)
  )

  if (length(x) > 0) {

    if (verbose && any_str) {
      # Print call for easier requests
      collapsed_x <- lapply(x, function(i) {
        if (length(i) > 1) paste0("c(", paste0(i, collapse = ", "), ")") else i
      })

      mockup <- unlist(collapsed_x)
      res <- paste0("get_recorddata(",
                    paste0(names(mockup), " = ", mockup, collapse = ", "),
                    ")")
      cat("If you run the same query again, use the one below (faster): \n ",
          res)

    }

    # Turn TRUE/FALSE to true/false
    is_logical <- vapply(x, is.logical, FUN.VALUE = logical(1))
    x[is_logical] <- lapply(x[is_logical], function(x) tolower(as.character(x)))

    # Collapse multiple ids to parameters
    x <- vapply(x, paste0, collapse = ",", FUN.VALUE = character(1))
    # and exclude the empty ones
    x <- x[x != ""]

    S   <- paste(paste(names(x), x, sep = "="), collapse = "&")
    out <- paste0("?", S)
  } else {
    out <- ""
  }
  out
}


#' Format data from character to numeric
#' @description When a data is downloaded from web it is saved as a list or
#' data.frame with columns containing strings of information (character format).
#' This function reads the values and if it sees in these columns only numbers
#' will convert the column to class numeric.
#' @param X data.frame
#' @keywords internal
format.numeric.colums <- function(X) {
  cn    <- colnames(X)
  isNum <- apply(X, 2, FUN = function(w) all(varhandle::check.numeric(w)))
  X[isNum] <- lapply(X[, isNum], as.numeric)

  X
}


#' Save downloaded data in a .Rdata file located in the working directory
#' @param data The dataset to be saved;
#' @param file_name Name to be assigned to the data.
#' @keywords internal
save_in_working_dir <- function(data, file_name) {
  assign(file_name, value = data)
  save(list = file_name, file = paste0(file_name, ".Rdata"))

  wd <- getwd()
  n  <- nchar(wd)
  wd <- paste0("...", substring(wd, first = n - 45, last = n))
  message(paste0(file_name, ".Rdata is saved in your working directory:\n", wd),
          appendLF = FALSE)
  cat("\n   ")
}

lookupLocIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)

  paramStr_low <- tolower(paramStr)

  locs <- get_locations()
  cnt_code <- locs[tolower(locs$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 countries, for example
  if (all(!paramStr_low %in% tolower(cnt_code$Name))) {
    stop("Location(s) ",
         paste0("'", paramStr[!paramStr_low %in% cnt_code$Name], "'", collapse = ", "),
         " not found. Check get_locations()")
  }

  cnt_code[["PK_LocID"]]
}

lookupIndicatorIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- get_indicatortypes()
  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 indicators, for example
  if (all(!tolower(paramStr_low) %in% tolower(inds_code$Name))) {
    stop("Location(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check get_indicatortypes()")
  }

  inds_code[["PK_IndicatorTypeID"]]
}

lookupSubGroupsIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- get_subgroups()
  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 indicators, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Location(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check get_subgroups()")
  }

  inds_code[["PK_SubGroupID"]]
}

lookupAreaTypeIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- get_locationtypes(locIds = paramList[["locIds"]],
                            indicatorTypeIds = paramList[["indicatorTypeIds"]],
                            isComplete = paramList[["isComplete"]])

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]
  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Area Type(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check get_locationtypes()")
  }

  inds_code[["PK_LocAreaTypeID"]]
}

lookupDataProcessTypeIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- get_dataprocesstype(locIds = paramList[["locIds"]],
                              indicatorTypeIds = paramList[["indicatorTypeIds"]],
                              isComplete = paramList[["isComplete"]])

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]
  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Data type(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check get_dataprocesstype()")
  }

  inds_code[["PK_DataProcessTypeID"]]
}

lookupDataProcessIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- get_dataprocess(locIds = paramList[["locIds"]],
                          indicatorTypeIds = paramList[["indicatorTypeIds"]],
                          isComplete = paramList[["isComplete"]])

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Data type(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check get_dataprocess()")
  }

  inds_code[["PK_DataProcessID"]]
}

lookupIsCompleteIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)

  paramStr_low <- tolower(paramStr)

  res <- switch(paramStr_low,
                "abridged" = 0,
                "complete" = 1,
                "total" = 2,
                stop("IsComplete does not accept string '",
                     paramStr, "'",
                     ". Only 'abridged', 'complete', 'total'.")
                )

  res
}

values_env <- new.env()

# Columns to turn into labelled factors
values_env$id_to_fact <- c(AreaName = "AreaID",
                           ## DataCatalogName = "DataCatalogID",
                           DataReliabilityName = "DataReliabilityID",
                           ## SubGroupName = "PK_SubGroupID",
                           ## DataSourceName = "DataSourceID",
                           DataStatusName = "DataStatusID",
                           ## DataTypeName = "DataTypeID",
                           DataTypeGroupName = "DataTypeGroupID",
                           IndicatorName = "IndicatorID",
                           ## LocName = "LocID",
                           LocAreaTypeName = "LocAreaTypeID",
                           LocTypeName = "LocTypeID",
                           ModelPatternName = "ModelPatternID",
                           ModelPatternFamilyName = "ModelPatternFamilyID",
                           PeriodGroupName = "PeriodGroupID",
                           PeriodTypeName = "PeriodTypeID",
                           RegName = "RegID",
                           SexName = "SexID",
                           StatisticalConceptName = "StatisticalConceptID",
                           SubGroupTypeName = "SubGroupTypeID"
                           ## DataProcess = "PK_DataProcessID"
                           )

values_env$col_order <- c("IndicatorName",
                          "IndicatorID",
                          "LocName",
                          "LocID",
                          "StructuredDataID",
                          "LocTypeName",
                          "LocTypeID",
                          "RegName",
                          "RegID",
                          "AreaName",
                          "AreaID",
                          "LocAreaTypeName",
                          "LocAreaTypeID",
                          "SubGroupTypeName",
                          "SubGroupTypeID",
                          "SubGroupID1",
                          "SubGroupName",
                          "SubGroupCombinationID",
                          "SubGroupTypeSortOrder",
                          "IndicatorShortName",
                          "DataCatalogID",
                          "DataProcessTypeID",
                          "DataProcessType",
                          "DataProcessTypeSort",
                          "DataProcess",
                          "DataProcessID",
                          "DataProcessSort",
                          "DataCatalogName",
                          "DataCatalogShortName",
                          "ReferencePeriod",
                          "ReferenceYearStart",
                          "ReferenceYearEnd",
                          "ReferenceYearMid",
                          "FieldWorkStart",
                          "FieldWorkEnd",
                          "FieldWorkMiddle",
                          "DataCatalogNote",
                          "DataSourceID",
                          "DataSourceAuthor",
                          "DataSourceYear",
                          "DataSourceName",
                          "DataSourceShortName",
                          "DataSourceSort",
                          "HasUncertaintyRecord",
                          "StandardErrorValue",
                          "ConfidenceInterval",
                          "ConfidenceIntervalLowerBound",
                          "ConfidenceIntervalUpperBound",
                          "DataStatusName",
                          "DataStatusID",
                          "DataStatusSort",
                          "StatisticalConceptName",
                          "StatisticalConceptID",
                          "StatisticalConceptSort",
                          "SexName",
                          "SexID",
                          "SexSort",
                          "AgeID",
                          "AgeUnit",
                          "AgeStart",
                          "AgeEnd",
                          "AgeSpan",
                          "AgeMid",
                          "AgeLabel",
                          "AgeSort",
                          "agesort",
                          "DataTypeGroupName",
                          "DataTypeGroupID",
                          "DataTypeName",
                          "DataTypeID",
                          "DataTypeSort",
                          "ModelType",
                          "ModelPatternFamilyName",
                          "ModelPatternFamilyID",
                          "ModelPatternName",
                          "ModelPatternID",
                          "DataReliabilityName",
                          "DataReliabilityID",
                          "DataReliabilitySort",
                          "PeriodTypeName",
                          "PeriodTypeID",
                          "PeriodGroupName",
                          "PeriodGroupID",
                          "PeriodStart",
                          "PeriodEnd",
                          "PeriodSpan",
                          "PeriodMiddle",
                          "Weight",
                          ## "TimeReferenceID",
                          "TimeUnit",
                          "TimeStart",
                          "TimeEnd",
                          "TimeDuration",
                          "TimeMid",
                          "TimeLabel",
                          ## "StaffMemberID",
                          "FootNoteID",
                          "id",
                          "SeriesID",
                          "DataValue")

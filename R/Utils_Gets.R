# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 12:14:32 2018
# --------------------------------------------------- #

#' API Link Generator Function
#' @param server The path to the database. Default: 
#' \code{"http://24.239.36.16:9654/un2/api/"};
#' @param type Type of data. Various options are available.
#' @param ... Other arguments that might define the path to data. All arguments
#' accept a numeric code which is interpreted as the code of the specific
#' product requested. Alternatively, you can supply the equivalent product
#' name as a string which is case insensitive (see examples). Handle with
#' care, this is important! The following options are available: \itemize{
#'   \item{\code{dataProcessIds}} -- Data process ID as defined by the UNPD. 
#'   Run the \code{\link{getDataProcess}} function to see the available 
#'   options;
#'   \item{\code{startYear}} -- Start year. Default: NULL;
#'   \item{\code{endYear}} -- End year. Default: NULL;
#'   \item{\code{indicatorTypeIds}} -- Indicator type ID as defined by the UNPD. 
#'   Run the \code{\link{getIndicators}} function to see the available options;
#'   \item{\code{isComplete}} -- isComplete;
#'   \item{\code{locIds}} -- Location ID as defined by the UNPD. Run the
#'   \code{\link{getLocations}} function to see the available options;
#'   \item{\code{locAreaTypeIds}} -- Location area type ID as defined by the UNPD. 
#'   Run the \code{\link{getLocationTypes}} function to see the available 
#'   options;
#'   \item{\code{subGroupIds}} -- SubGroup ID as defined by the UNPD. Run the 
#'   \code{\link{getSubGroups}} function to see the available options;
#'   \item{\code{addDefault}} -- Logical. Default: FALSE;
#'   \item{\code{includeDependencies}} -- Logical. Default: FALSE;
#'   \item{\code{includeFormerCountries}} -- Logical. Default: FALSE.
#'   }
#' @details The link generator is based on the structure of the database 
#' created by Dennis Butler (in late 2018). If the web address or the structure 
#' of the database changes this will have to be updated as well.
#' @examples 
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
#' @export
linkGenerator <- function(server = "http://24.239.36.16:9654/un3/api/", 
                           type,
                           ...) {
  
  types <- c("ages",
             "openAges",
             "Component",
             "DataCatalog",
             "dataProcessTypes",
             "DataReliability",
             "DataSources",
             "DataSourceStatus",
             "DataSourceTypes",
             "DataStatus",
             "DataTypes",
             "DefaultKeys",
             "Indicators",
             "IndicatorTypes",
             "locAreaTypes",
             "Locations",
             "PeriodGroups",
             "PeriodTypes",
             "Sex",
             "StatisticalConcepts",
             "StructuredData",
             "StructuredDataTable",
             "StructuredDataRecords",
             "StructuredDataSeries",
             "structuredDataCriteria",
             "subGroups",
             "SubGroupTypes",
             "TimeReferences",
             # These are within UserUtility
             "dataEntryCount")
  
  type  <- match.arg(tolower(type), choices = tolower(types))
  query <- build_filter(...)
  link  <- paste0(server, type, query)
  link
}


#' Build the section of the path (link) responsible with filtering the data
#' @param dataProcessIds 
#' @param startYear Start year. Default: NULL;
#' @param endYear End year. Default: NULL;
#' @param AgeStart 
#' @param AgeEnd 
#' @param indicatorTypeIds 
#' @param isComplete isComplete;
#' @param isActive 
#' @param locIds 
#' @param locAreaTypeIds 
#' @param subGroupIds 
#' @param addDefault Logical. Default: FALSE;
#' @param includeDependencies Logical. Default: FALSE;
#' @param includeFormerCountries Logical. Default: FALSE;
#' @param dataProcess Data process ID as defined by the UNPD. Run the
#' \code{\link{getDataProcess}} function to see the available options;
#' TODOOOOOOOOOOOOO!!
#' @param indicatorType Indicator type ID as defined by the UNPD. Run the
#' \code{\link{getIndicators}} function to see the available options;
#' @param locIds Location ID as defined by the UNPD. Run the
#' \code{\link{getLocations}} function to see the available options;
#' @param locAreaTypeIds Location area type ID as defined by the UNPD. Run the
#' \code{\link{getLocations}} function to see the available options;
#' @param subGroup SubGroup ID as defined by the UNPD.
#' Run the \code{\link{getSubGroups}} function to see the available options;
#' @keywords internal
build_filter <- function(dataProcessIds = NULL,
                         startYear = NULL,
                         endYear = NULL,
                         AgeStart = NULL,
                         AgeEnd = NULL,
                         indicatorTypeIds = NULL,
                         indicatorIDs = NULL,
                         isComplete = NULL,
                         isActive = NULL,
                         locIds = NULL,
                         ids = NULL,
                         locAreaTypeIds = NULL,
                         subGroupIds = NULL,
                         addDefault = NULL,
                         includeDependencies = NULL, 
                         includeFormerCountries = NULL,
                         includeDataIDs = NULL) {

  # Keep as list because unlisting multiple ids for a single
  # parameters separates them into different strings
  I <- environment() %>% as.list()
  lookupParams <- list("locIds" = lookupLocIds,
                       "indicatorTypeIds" = lookupIndicatorIds,
                       "isComplete" = lookupIsCompleteIds,
                       "subGroupIds" = lookupSubGroupsIds)

  # Iteratire over each lookupParams and apply their correspoding lookup
  # function to translate strings such as Germany to the corresponding code.
  # Only available for the names in lookupParams
  I[names(lookupParams)] <- mapply(
    function(fun, vec) fun(vec),
    lookupParams,
    I[names(lookupParams)]
  )

  # Here we need a separate call to the same thing bc
  # I reuse the translated parameters defined above
  # to make queries in the endpoints below
  extraParams <- list("locAreaTypeIds" = lookupAreaTypeIds,
                      "dataProcessIds" = lookupDataProcess)

  I[names(extraParams)] <- mapply(
    function(fun, vec, ...) fun(vec, ...),
    extraParams,
    I[names(extraParams)],
    # I pass the already translated parameter list
    # to avoid retranslating stuff like locations, etc...
    # This can save time in querying API
    MoreArgs = list(paramList = I)
  )

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



#' Format data from character to numeric
#' @description When a data is downloaded from web it is saved as a list or 
#' data.frame with columns containing strings of information (character format).
#' This function reads the values and if it sees in these columns only numbers
#' will convert the column to class numeric.
#' @param X data.frame
#' @keywords internal
format.numeric.colums <- function(X) {
  cn    <- colnames(X) 
  isNum <- apply(X, 2, FUN = function(w) all(check.numeric(w)))
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
  
  locs <- getLocations()
  cnt_code <- locs[tolower(locs$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 countries, for example
  if (all(!paramStr_low %in% tolower(cnt_code$Name))) {
    stop("Location(s) ",
         paste0("'", paramStr[!paramStr_low %in% cnt_code$Name], "'", collapse = ", "),
         " not found. Check getLocations()")
  }

  cnt_code[["PK_LocID"]]
}

lookupIndicatorIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)
  
  inds <- getIndicators()
  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 indicators, for example
  if (all(!tolower(paramStr_low) %in% tolower(inds_code$Name))) {
    stop("Location(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check getIndicators()")
  }

  inds_code[["PK_IndicatorTypeID"]]
}

lookupSubGroupsIds <- function(paramStr) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- getSubGroups()
  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]

  # The all statement is in case you provide 2 indicators, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Location(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check getSubGroups()")
  }

  inds_code[["PK_SubGroupID"]]
}

lookupAreaTypeIds <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)
  
  inds <- getLocationTypes(locIds = paramList[["locIds"]],
                           indicatorTypeIds = paramList[["indicatorTypeIds"]],
                           isComplete = paramList[["isComplete"]])

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]
  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Area Type(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check getLocationTypes()")
  }
  
  inds_code[["PK_LocAreaTypeID"]]
}

lookupDataProcess <- function(paramStr, paramList) {
  if (is.numeric(paramStr) || is.null(paramStr)) return(paramStr)
  paramStr_low <- tolower(paramStr)

  inds <- getDataProcess(locIds = paramList[["locIds"]],
                         indicatorTypeIds = paramList[["indicatorTypeIds"]],
                         isComplete = paramList[["isComplete"]])

  inds_code <- inds[tolower(inds$Name) %in% paramStr_low, ]
  # The all statement is in case you provide 2 area types, for example
  if (all(!tolower(paramStr) %in% tolower(inds_code$Name))) {
    stop("Data type(s) ",
         paste0("'", paramStr[!paramStr_low %in% inds_code$Name], "'", collapse = ", "),
         " not found. Check getDataProcess()")
  }
  
  inds_code[["PK_DataProcessTypeID"]]
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

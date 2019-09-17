# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 15:26:04 2018
# --------------------------------------------------- #


#' Get information about available locations (LocID)
#' @inheritParams read_API
#' @examples 
#' L <- getLocations(addDefault = "false",
#'                   includeDependencies = "false",
#'                   includeFormerCountries = "false")
#' L
#' @export
getLocations <- function(save = FALSE, ...) {
  
  read_API("locations", save, ...)
}


#' Get information about available location types (LocAreaTypeID)
#' @inheritParams read_API
#' @examples 
#' # Check what subgroups are available for:
#' P <- getLocationTypes(indicatorTypeIds = 8, # Population by age and sex indicator;
#'                       locIds = 818,      # Egypt
#'                       isComplete = 0)
#' P
#' @export
getLocationTypes <- function(save = FALSE, ...) {
  
  read_API("locareatypes", save, ...)
}


#' Get information about available sub-group-types (SubGroupTypeID)
#' @inheritParams read_API
#' @examples 
#' # Check what subgroups are available for:
#' S <- getSubGroups(indicatorTypeIds = 8,  # Population by age and sex indicator;
#'                   locIds = 818,       # Egypt
#'                   isComplete = 0)
#' S
#' @export
getSubGroups <- function(save = FALSE, ...) {
  
  read_API("subGroups", save, ...)
}


#' Get information about available indicators (IndicatorTypeID)
#' @inheritParams read_API
#' @examples 
#' I <- getIndicators(addDefault = "false")
#' I[, c("PK_IndicatorTypeID", "Name", "ShortName")]
#' @export
getIndicators <- function(save = FALSE, ...) {
  
  read_API("indicatortypes", save, ...)
}

#' Get information about available data-types (DataProcessTypeID)
#' @inheritParams read_API
#' @examples 
#' D <- getDataProcess()
#' D[, c("PK_DataProcessTypeID", "Name", "ShortName")]
#' @export
getDataProcess <- function(save = FALSE, ...) {
  
  read_API("dataProcessTypes", save, ...)
}

#' Get information about available details for a given series of data
#' @inheritParams read_API
#' @examples 
#' G <- getSeriesData(dataProcessIds = 0:15, # possible processes
#'                    indicatorTypeIds = 25,    # M[x]
#'                    locIds = 818,             # Egypt
#'                    locAreaTypeIds = c(2, 3, 4), # all possible types
#'                    subGroupIds = 2)
#' G
#' @export
getSeriesData <- function(save = FALSE, ...) {
  
  read_API("structureddataseries", save, ...)
}


#' Download data from UNPD portal
#' @inheritParams read_API
#' @examples 
#' X <- getRecordData(dataProcessIds = 6,   # Estimate
#'                    indicatorTypeIds = 8, # Population by age and sex - abridged 
#'                    locIds = 818,         # Egypt
#'                    locAreaTypeIds = 2,   # Whole area 
#'                    subGroupIds = 2,      # Total or All groups
#'                    isComplete = 0)       # Age Distribution: Abridged
#' head(X)
#' @export
getRecordData <- function(save = FALSE, ...) {
  
  read_API("structureddatarecords", save, ...)
}



#' Download data
#' @param save Logical. Choose whether or not to save the data in an external 
#' \code{.Rdata} file in the working directory. Default: 
#' \code{FALSE};
#' @inheritParams linkGenerator
#' @keywords internal
read_API <- function(type, save, ...){
  P <- linkGenerator(type = type, ...)
  # Temporary, just to check how the URL is constructed
  print(P)
  X <- rjson::fromJSON(file = P)
  #
  out <-
    X %>% 
    lapply(unlist) %>%    # list elements can either be lists or vectors
    lapply(as.list) %>%   # here now everything is homogenously a vector
    dplyr::bind_rows() %>%  # even if named elements differ still becomes rectangular
    lapply(trimws) %>%    # Remove leading/trailing spaces from the names
    as.data.frame(stringsAsFactors = FALSE)       # coerce to desired form
  if (type == "recordDataDetail") {
    out <- format.numeric.colums(out)
  }
  if (save) {
    save_in_working_dir(data = out, file_name = paste0("UNPD_", 
                                                       type))
  }
  out
}


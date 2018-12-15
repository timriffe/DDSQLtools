# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 12:13:27 2018
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
  
  read_API("country", save, ...)
}


#' Get information about available location types (LocAreaTypeID)
#' @inheritParams read_API
#' @examples 
#' # Check what subgroups are available for:
#' P <- getLocationTypes(indicatorType = 8, # Population by age and sex indicator;
#'                       loc = 818,         # Egypt
#'                       isComplete = 0)
#' P
#' @export
getLocationTypes <- function(save = FALSE, ...) {
  
  read_API("locationType", save, ...)
}


#' Get information about available sub-group-types (SubGroupTypeID)
#' @inheritParams read_API
#' @examples 
#' # Check what subgroups are available for:
#' S <- getSubGroups(indicatorType = 8,  # Population by age and sex indicator;
#'                   loc = 818,          # Egypt
#'                   isComplete = 0)
#' S
#' @export
getSubGroups <- function(save = FALSE, ...) {
  
  read_API("subGroup", save, ...)
}


#' Get information about available indicators (IndicatorTypeID)
#' @inheritParams read_API
#' @examples 
#' I <- getIndicators(addDefault = "false")
#' I[, c("IndicatorTypeID", "Name", "ShortName")]
#' @export
getIndicators <- function(save = FALSE, ...) {
  
  read_API("Indicator", save, ...)
}


#' Get information about available data-types (DataProcessTypeID)
#' @inheritParams read_API
#' @examples 
#' D <- getDataProcessTypes()
#' D
#' @export
getDataProcessTypes <- function(save = FALSE, ...) {
  
  read_API("dataProcessTypes", save, ...)
}


#' Get information about available details for a given series of data
#' @inheritParams read_API
#' @examples 
#' G <- getSeriesDataDetail(dataProcess = paste(0:15, collapse = ","), # all possible processes
#'                          indicatorType = 25,    # M[x]
#'                          loc = 818,             # Egypt
#'                          locAreaType = "2,3,4", # all possible types
#'                          subGroup = 2)
#' G
#' @export
getSeriesDataDetail <- function(save = FALSE, ...) {
  
  read_API("seriesDataDetail", save, ...)
}


#' Download data from UNPD portal
#' @inheritParams read_API
#' @examples 
#' X <- getRecordDataDetail(dataProcess = 6,   # Estimate
#'                          indicatorType = 8, # Population by age and sex - abridged 
#'                          loc = 818,         # Egypt
#'                          locAreaType = 2,   # Whole area 
#'                          subGroup = 2,      # Total or All groups
#'                          isComplete = 0)    # Age Distribution: Abridged
#' X
#' @export
getRecordDataDetail <- function(save = FALSE, ...) {
  
  read_API("seriesDataDetail", save, ...)
}



#' Download data
#' @param save Logical. Choose whether or not to save the data in an external 
#' \code{.Rdata} file in the working directory. Default: 
#' \code{FALSE};
#' @inheritParams linkGenerator
#' @keywords internal
read_API <-function(type, 
                    save, 
                    ...) {
  
  P <- linkGenerator(type = type, ...) # path
  X <- fromJSON(file = P)
  
  if (type %in% c("Indicator", "dataProcessTypes")) {
    
    X1 <- X %>% lapply(unlist) 
    # X1 may be a list with elements of different length, therefore if we do
    # do.call("rbind", X1) we might have some errors/warnings and funny output;
    # So we build a matrix and populate it row by row as follows:
    n  <- length(X1)
    cn <- X1 %>% lapply(names) %>% unlist %>% unique # unique names
    Z  <- matrix(NA, ncol = length(cn), nrow = n, dimnames = list(1:n, cn))
    for (j in 1:n) Z[j, names(X1[[j]])] <- X1[[j]]
  
  } else if (type %in% c("seriesDataDetail")) {
    X1 <- X %>% lapply(unlist) 
    Z  <- do.call("rbind", X1)
    
  } else {
    Z <- do.call("rbind", X)
  }
  
  out <- as.data.frame(Z)
  
  if (type == "recordDataDetail"){
    out <- format.numeric.colums(out)
  } 
  
  if (save) {
    save_in_working_dir(data = out, file_name = paste0("UNPD_", type))
  }
  
  return(out)
}




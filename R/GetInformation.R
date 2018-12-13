# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Thu Dec 13 17:07:36 2018
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


#' Download data
#' @param save Logical. Choose whether or not to save the data in an external 
#' \code{.Rdata} file in the working directory. Default: 
#' \code{FALSE};
#' @inheritParams linkGenerator
#' @keywords internal
read_API <-function(type, 
                    save, 
                    ...) {
  
  . <- NULL # hack CRAN check note
  P <- linkGenerator(type = type, ...) # path
  X <- fromJSON(file = P)
  
  if (type %in% c("Indicator", "dataProcessTypes")) {
    X <- X %>% lapply(unlist) 
  }
  
  Z <- suppressWarnings(X %>% do.call("rbind", .) %>% as.data.frame)
  # Some of the lists might have different number of elements. When we rbind
  # a warning is displayed. We suppress it here. It seems to work fine.
  
  if (save) save_in_working_dir(data = Z, 
                                file_name = paste0("UNPD_", type))
  return(Z)
}




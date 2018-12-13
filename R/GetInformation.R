# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Thu Dec 13 16:17:30 2018
# --------------------------------------------------- #


#' Get information about available locations (LocID)
#' @inheritParams read_API_a
#' @examples 
#' L <- getLocations(addDefault = "false",
#'                   includeDependencies = "false",
#'                   includeFormerCountries = "false")
#' L
#' @export
getLocations <- function(save = FALSE, ...) {
  
  read_API_a("country", save, ...)
}


#' Get information about available location types (LocAreaTypeID)
#' @inheritParams read_API_a
#' @examples 
#' # Check what subgroups are available for:
#' P <- getLocationTypes(indicatorType = 8,  # Population by age and sex indicator;
#'                       loc = 818,          # Egypt
#'                       isComplete = 0)
#' P
#' @export
getLocationTypes <- function(save = FALSE, ...) {
  
  read_API_a("locationType", save, ...)
}


#' Get information about available sub-group-types (SubGroupTypeID)
#' @inheritParams read_API_a
#' @examples 
#' # Check what subgroups are available for:
#' S <- getSubGroups(indicatorType = 8,  # Population by age and sex indicator;
#'                   loc = 818,          # Egypt
#'                   isComplete = 0)
#' S
#' @export
getSubGroups <- function(save = FALSE, ...) {
  
  read_API_a("subGroup", save, ...)
}


#' Get information about available indicators (IndicatorTypeID)
#' @inheritParams read_API_b
#' @examples 
#' I <- getIndicators(addDefault = "false")
#' I[, c("IndicatorTypeID", "Name", "ShortName")]
#' @export
getIndicators <- function(save = FALSE, ...) {
  
  read_API_b("Indicator", save, ...)
}


#' Get information about available data-types (DataProcessTypeID)
#' @inheritParams read_API_b
#' @examples 
#' D <- getDataProcessTypes()
#' D
#' @export
getDataProcessTypes <- function(save = FALSE, ...) {
  
  read_API_b("dataProcessTypes", save, ...)
}


#' Download data - version A
#' @param this_table Type of data;
#' @param server The path to the database. Default: 
#' \code{"http://24.239.36.16:9654/un2/api/"};
#' @param save Logical. Choose whether or not to save the data in an external 
#' \code{.Rdata} file in the working directory. Default: 
#' \code{FALSE};
#' @param ... Other arguments that might define the path to data. Handle with 
#' care, this is important! The following options are available: \itemize{
#'   \item{\code{dataProcess}} -- Data process ID as defined by the UNPD. 
#'   Run the \code{\link{getDataProcessTypes}} function to see the available 
#'   options;
#'   \item{\code{startYear}} -- Start year. Default: NULL;
#'   \item{\code{endYear}} -- End year. Default: NULL;
#'   \item{\code{indicatorType}} -- Indicator type ID as defined by the UNPD. 
#'   Run the \code{\link{getIndicators}} function to see the available options;
#'   \item{\code{isComplete}} -- isComplete;
#'   \item{\code{loc}} -- Location ID as defined by the UNPD. Run the
#'   \code{\link{getLocations}} function to see the available options;
#'   \item{\code{locAreaType}} -- Location area type ID as defined by the UNPD. 
#'   Run the \code{\link{getLocationTypes}} function to see the available 
#'   options;
#'   \item{\code{subGroup}} -- SubGroup ID as defined by the UNPD. Run the 
#'   \code{\link{getSubGroups}} function to see the available options;
#'   \item{\code{addDefault}} -- Logical. Default: FALSE;
#'   \item{\code{includeDependencies}} -- Logical. Default: FALSE;
#'   \item{\code{includeFormerCountries}} -- Logical. Default: FALSE.
#'   }
#' @keywords internal
read_API_a <-function(this_table, 
                      save, 
                      server = "http://24.239.36.16:9654/un2/api/", 
                      ...) {
  
  . <- NULL # hack CRAN check note
  
  this_data <- build_filter(...)
  this_path <- paste0(server, this_table, this_data)
  
  X <- fromJSON(file = this_path)
  out <- suppressWarnings(X %>% 
                            lapply(unlist) %>% 
                            do.call("rbind", .) %>% 
                            as.data.frame)
  # Some of the lists might have different number of elements. When we rbind
  # a warning is displayed. We suppress it here. It seems to work fine.
  
  if (save) save_in_working_dir(data = out, 
                                file_name = paste0("UNPD_", this_table))
  return(out)
}


#' Download data - version B
#' @inheritParams read_API_a
#' @keywords internal
read_API_b <-function(this_table, 
                      save, 
                      server = "http://24.239.36.16:9654/un2/api/", 
                      ...) {
  
  . <- NULL # hack CRAN check note
  
  this_data <- if (this_table == "dataProcessTypes") "All" else build_filter(...)
  this_path <- paste0(server, this_table, this_data)
  
  X <- fromJSON(file = this_path)
  out <- suppressWarnings(X %>% 
                            do.call("rbind", .) %>% 
                            as.data.frame)
  
  if (save) save_in_working_dir(data = out, 
                                file_name = paste0("UNPD_", this_table))
  return(out)
}



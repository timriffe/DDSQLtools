# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Thu Dec 13 13:15:56 2018
# --------------------------------------------------- #


#' Get information about available locations (LocID)
#' @inheritParams build_filter
#' @param server The path to the database. Default: 
#' \code{"http://24.239.36.16:9654/un2/api/"};
#' @param save Logical. Choose whether or not to save the data in an external 
#' \code{.Rdata} file in the working directory. Default: 
#' \code{FALSE};
#' @param ... Other conditions that might define the path to data.
#' @examples 
#' getLocations()
#' @export
getLocations <- function(addDefault = "false",
                         includeDependencies = "false",
                         includeFormerCountries = "false",
                         server = "http://24.239.36.16:9654/un2/api/",
                         save = FALSE,
                         ...) {
  
  this_table <- "country"
  this_data  <- build_filter(addDefault = addDefault,
                             includeDependencies = includeDependencies,
                             includeFormerCountries = includeFormerCountries,
                             ...)
  this_path  <- paste0(server, this_table, this_data)
  
  C <- fromJSON(file = this_path) %>% lapply(unlist)
  out <- do.call("rbind", C) %>% as.data.frame
  
  if (save) save_data_in_working_directory(out, file_name = "UNPD_Countries")
  
  return(out)
}


#' Get information about available indicators (IndicatorTypeID)
#' @inheritParams build_filter
#' @inheritParams getLocations
#' @examples 
#' Ind <- getIndicators()
#' Ind[, c("IndicatorTypeID", "Name", "ShortName")]
#' @export
getIndicators <- function(addDefault = "false",
                          server = "http://24.239.36.16:9654/un2/api/",
                          save = FALSE,
                          ...) {
  
  this_table <- "Indicator"
  this_data  <- build_filter(addDefault = addDefault, ...)
  this_path  <- paste0(server, this_table, this_data)
  
  I   <- fromJSON(file = this_path)
  out <- do.call("rbind", I) %>% as.data.frame
  
  if (save) save_data_in_working_directory(out, file_name = "UNPD_Indicators")
  
  return(out)
}


#' Get information about available data-types (DataProcessTypeID)
#' @inheritParams getLocations
#' @examples 
#' getDataProcessTypes()
#' @export
getDataProcessTypes <- function(server = "http://24.239.36.16:9654/un2/api/",
                                save = FALSE) {
  
  this_table <- "dataProcessTypes"
  this_data  <- "All"
  this_path  <- paste0(server, this_table, this_data)
  
  DT <- fromJSON(file = this_path)
  out <- do.call("rbind", DT) %>% as.data.frame
  
  if (save) save_data_in_working_directory(out, file_name = "UNPD_DataProcessTypes")
  
  return(out)
}


#' Get information about available sub-group-types (SubGroupTypeID)
#' @inheritParams build_filter
#' @inheritParams getLocations
#' @examples 
#' # Check what subgroups are available for:
#' S <- getSubGroup(indicatorType = 8,  # Population by age and sex indicator;
#'                  loc = 818)          # Egypt
#' S
#' @export
getSubGroup <- function(indicatorType = 8,
                        loc = 818,
                        isComplete = 0,
                        server = "http://24.239.36.16:9654/un2/api/",
                        save = FALSE,
                        ...) {
  
  . <- NULL # hack CRAN check note
  
  this_table <- "subGroup"
  this_data  <- build_filter(indicatorType = indicatorType, 
                             loc = loc,
                             isComplete = isComplete,
                             ...)
  this_path  <- paste0(server, this_table, this_data)
  
  X <- fromJSON(file = this_path)
  out <- suppressWarnings(X %>% 
                            lapply(unlist) %>% 
                            do.call("rbind", .) %>% 
                            as.data.frame)
  # Some of the lists might have different number of elements. When we rbind
  # a warning is displayed. We surpress it here. It seems to work fine.
  
  if (save) save_data_in_working_directory(out, file_name = "UNPD_SubGroupTypes")
  
  return(out)
}



#' Format data from character to numeric
#' @description When a data is downloaded from web it is saved as a list or 
#' data.frame with columns containing strings of information (character format).
#' This function reads the values and if it sees in these columns only numbers
#' will convert the column to class numeric.
#' @param A data.frame or matrix.
#' @keywords internal
format.numeric.colums <- function(X) {
  cn <- colnames(X) 
  isNum <- apply(X, 2, FUN = function(w) w %>% check.numeric %>% all)
  out <- data.frame(X[, !isNum], apply(X[, isNum], 2, as.numeric))
  out[, cn]
}


#' Build the section of the path (link) responsible with filtering the data
#' @param addDefault Logical. Default: FALSE;
#' @param dataProcess Data process ID as defined by the UNPD. Run the
#' \code{\link{getDataProcessTypes}} function to see the available options;
#' @param startYear Start year. Default: NULL;
#' @param endYear End year. Default: NULL;
#' @param indicatorType Indicator type ID as defined by the UNPD. Run the
#' \code{\link{getIndicators}} function to see the available options;
#' @param isComplete Default: NULL;
#' @param loc Location ID as defined by the UNPD. Run the
#' \code{\link{getLocations}} function to see the available options;
#' @param locAreaType Default: NULL;
#' @param subGroup SubGroup ID as defined by the UNPD. 
#' Run the \code{\link{getSubGroup}} function to see the available options;
#' @param includeDependencies Logical. Default: FALSE;
#' @param includeFormerCountries Logical. Default: FALSE;
#' @keywords internal
build_filter <- function(dataProcess = NULL,
                         startYear = NULL,
                         endYear = NULL,
                         indicatorType = NULL,
                         isComplete = NULL,
                         loc = NULL,
                         locAreaType = NULL,
                         subGroup = NULL,
                         addDefault = NULL,
                         includeDependencies = NULL,
                         includeFormerCountries = NULL) {
  
  I <- as.list(environment())
  S <- NULL
  for (i in 1:length(I)) {
    if (!is.null(I[[i]])) {
      x <- paste0(names(I)[i], "=", I[[i]])
      S <- paste(S, x, sep = "&")
    }
  }
  
  S <- substr(S, 2, nchar(S)) # remove 1st "&"...
  out <- paste0("?", S)       # and add "?" instead
  return(out)
}


#' Save downloaded data in a .Rdata file located in the working directory
#' @param data The dataset to be saved;
#' @param file_name Name to be assined to the data.
#' @keywords internal 
save_data_in_working_directory <- function(data, file_name) {
  assign(file_name, value = data)
  save(list = file_name, file = paste0(file_name, ".Rdata"))
  
  wd <- getwd()
  n  <- nchar(wd)
  wd <- paste0("...", substring(wd, first = n - 45, last = n))
  message(paste0(file_name, ".Rdata is saved in your working directory:\n  ", wd), 
          appendLF = FALSE)
  cat("\n   ")
}


# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Wed Dec 12 16:10:33 2018
# --------------------------------------------------- #


#' Download country names and LocID's
#' 
#' @param addDefault Logical. Default: FALSE;
#' @param includeDependencies Logical. Default: FALSE;
#' @param includeFormerCountries Logical. Default: FALSE;
#' @param server The path to the database. Default: 
#' "http://24.239.36.16:9654/un2/api/"
#' @param ... Other conditions that might define the path to data.
#' @examples 
#' getCountries()
#' @export
getCountries <- function(addDefault = "false",
                         includeDependencies = "false",
                         includeFormerCountries = "false",
                         server = "http://24.239.36.16:9654/un2/api/",
                         ...) {
  
  this_table <- "country"
  this_data  <- build_filter(addDefault = addDefault,
                             includeDependencies = includeDependencies,
                             includeFormerCountries = includeFormerCountries,
                             ...)
  
  this_path  <- paste0(server, this_table, this_data)
  
  C <- fromJSON(file = this_path) %>% lapply(unlist)
  out <- do.call("rbind", C) %>% as.data.frame
  
  return(out)
}


#' Download indicator names and IndicatorTypeID's
#' 
#' @inheritParams getCountries
#' @examples 
#' Ind <- getIndicators()
#' Ind[, c("IndicatorTypeID", "Name", "ShortName")]
#' @export
getIndicators <- function(addDefault = "false",
                          server = "http://24.239.36.16:9654/un2/api/",
                          ...) {
  
  this_table <- "Indicator"
  this_data  <- build_filter(addDefault = addDefault, ...)
  this_path  <- paste0(server, this_table, this_data)
  
  I   <- fromJSON(file = this_path)
  out <- do.call("rbind", I) %>% as.data.frame
  
  return(out)
}


#' Download data-types and DataProcessTypeID's
#' 
#' @inheritParams getCountries
#' @examples 
#' getDataProcessTypes()
#' @export
getDataProcessTypes <- function(server = "http://24.239.36.16:9654/un2/api/") {
  
  this_table <- "dataProcessTypes"
  this_data  <- "All"
  this_path  <- paste0(server, this_table, this_data)

  DT <- fromJSON(file = this_path)
  out <- do.call("rbind", DT) %>% as.data.frame
  
  return(out)
}



#' Format data from character to numeric
#' @description When a data is downloaded from web it is saved as a list or 
#' data.frame with columns containing strings of infromation (character format).
#' This function reads the values and if it sees in these columns anly numbers
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
#' @param dataProcess Default: NULL;
#' @param startYear Default: NULL;
#' @param endYear Default: NULL;
#' @param indicatorType Default: NULL;
#' @param isComplete Default: NULL;
#' @param loc Default: NULL;
#' @param locAreaType Default: NULL;
#' @param subGroup Default: NULL;
#' @inheritParams getCountries
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


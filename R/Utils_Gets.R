# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Thu Dec 13 15:20:55 2018
# --------------------------------------------------- #


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
#' @param dataProcess Data process ID as defined by the UNPD. Run the
#' \code{\link{getDataProcessTypes}} function to see the available options;
#' @param startYear Start year. Default: NULL;
#' @param endYear End year. Default: NULL;
#' @param indicatorType Indicator type ID as defined by the UNPD. Run the
#' \code{\link{getIndicators}} function to see the available options;
#' @param isComplete isComplete;
#' @param loc Location ID as defined by the UNPD. Run the
#' \code{\link{getLocations}} function to see the available options;
#' @param locAreaType Location area type ID as defined by the UNPD. Run the
#' \code{\link{getLocations}} function to see the available options;
#' @param subGroup SubGroup ID as defined by the UNPD.
#' Run the \code{\link{getSubGroups}} function to see the available options;
#' @param addDefault Logical. Default: FALSE;
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



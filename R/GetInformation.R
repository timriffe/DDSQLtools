##' Request data records directly from their ID
##'
##' @param ids a character vector of ID's that identify each data point
##' @inheritParams read_API
##' @return A data frame with the same rows as the number of ID's containing
##' the data values for that specific ID.
##'
##' @examples
##'
##' # TODO: Add good example once the data filtering of all data available
##' # is ready.
##' 
##' \dontrun{
##' extractData("35444654")
##' }
##'
##' @keywords internal 
extract_data <- function(ids, save = FALSE) {
  len_ids <- length(ids)

  if (len_ids > 200) {

    values <- c(seq(1, len_ids, by = 199), len_ids)
    seq_values <- seq_along(values)[-length(values)]

    all_res <-
      lapply(seq_values, function(i) {
        cat(paste0("\r\r Reading chunks: [", i, "/", length(seq_values + 1), "]"))
        seq_ch <- values[i:(i + 1)]
        if (i > 1) seq_ch[1] <- seq_ch[1] + 1
        id_chunk <- ids[seq_ch[1]:seq_ch[2]]
        read_API("structureddatarecords",
                 save = save,
                 verbose = FALSE,
                 ids = id_chunk)
      })

    collapsed_res <- do.call(rbind, all_res)
    collapsed_res$TimeStart <- chr_to_date(collapsed_res$TimeStart)
    collapsed_res$TimeEnd <- chr_to_date(collapsed_res$TimeEnd)

    return(collapsed_res)
  }

  cat(paste0("\r\r Reading chunks: [0/1]"))  
  collapsed_res <- read_API("structureddatarecords",
                            save = save,
                            verbose = FALSE,
                            ids = ids)

  collapsed_res$TimeStart <- chr_to_date(collapsed_res$TimeStart)
  collapsed_res$TimeEnd <- chr_to_date(collapsed_res$TimeEnd)
  cat(paste0("\r\r Reading chunks: [1/1]"))
  collapsed_res
}

#' Get information about available locations (LocID)
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' L <- get_locations(addDefault = "false",
#'                   includeDependencies = "false",
#'                   includeFormerCountries = "false")
#' L
#' }
#' 
#' @export
get_locations <- function(save = FALSE, ...) {
  
  read_API("locations", save, ...)
}


#' Get information about available location types (LocAreaTypeID)
#' @inheritParams read_API
#' @examples
#'
#' \dontrun{
#' # Check what subgroups are available for:
#' P <- get_locationtypes(indicatorTypeIds = 8, # Population by age and sex indicator;
#'                       locIds = "egypt",
#'                       isComplete = "abridged")
#' P
#' }
#' @export
get_locationtypes <- function(save = FALSE, ...) {
  
  read_API("locareatypes", save, ...)
}


#' Get information about available sub-group-types (SubGroupTypeID)
#' @inheritParams read_API
#' @examples
#' 
#' \dontrun{
#' # Check what subgroups are available for:
#' S <- get_subgroups(indicatorTypeIds = 8,  # Population by age and sex indicator;
#'                   locIds = 818,       # Egypt
#'                   isComplete = 0)
#' S
#' }
#' @export
get_subgroups <- function(save = FALSE, ...) {
  
  read_API("subGroups", save, ...)
}


#' Get information about available indicators (IndicatorTypeID)
#' @inheritParams read_API
#' @examples 
#' \dontrun{
#' I <- get_indicatortypes(addDefault = "false")
#' I[, c("PK_IndicatorTypeID", "Name", "ShortName")]
#' }
#' @export
get_indicatortypes <- function(save = FALSE, ...) {
  
  read_API("indicatortypes", save, ...)
}

#' Get information about available indicators (IndicatorID)
#' @inheritParams read_API
#' @examples 
#' \dontrun{
#' I <- get_indicators(addDefault = "false")
#' I[, c("PK_IndicatorTypeID", "Name", "ShortName")]
#' }
#' @export
get_indicators <- function(save = FALSE, ...) {
  
  read_API("indicators", save, ...)
}


#' Get information about available data-types (DataProcessTypeID)
#' @inheritParams read_API
#' @examples 
#' \dontrun{
#' D <- get_dataprocess()
#' D[, c("PK_DataProcessTypeID", "Name", "ShortName")]
#' }

#' @export
get_dataprocess <- function(save = FALSE, ...) {
  
  read_API("dataProcessTypes", save, ...)
}

#' Get information about available details for a given series of data
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' # You can provide all strings, all codes, or a combination of both
#' G <- get_seriesdata(dataProcessIds = 0:15, # possible processes
#'                    indicatorTypeIds = 25,    # M[x]
#'                    locIds = "Egypt",             # Egypt
#'                    locAreaTypeIds = c("whole area", "rural", "urban"), # all possible types
#'                    subGroupIds = 2)
#' G
#' }
#' @export
get_seriesdata <- function(save = FALSE, ...) {
  
  read_API("structureddataseries", save, ...)
}


#' Download data from UNPD portal
#' @inheritParams read_API
#' @param verbose Whether to print the translated query from strings to digits
#' for faster queries. By default set to TRUE.
#' @examples
#'
#' \dontrun{
#' #  You can provide all strings, all codes, or a combination of both
#' Y <- get_recorddata(dataProcessIds = "Census",
#'                    indicatorTypeIds = 8, # and support numeric of string names
#'                    locIds = "egypt", # all arguments are case insensitive
#'                    locAreaTypeIds = "Whole area",
#'                    subGroupIds = "Total or All groups",
#'                    isComplete = "Abridged")
#'
#' head(Y)
#'
#' # Same thing only with codes
#' X <- get_recorddata(dataProcessIds = 2,   # Census
#'                    indicatorTypeIds = 8, # Population by age and sex - abridged 
#'                    locIds = 818,         # Egypt
#'                    locAreaTypeIds = 2,   # Whole area 
#'                    subGroupIds = 2,      # Total or All groups
#'                    isComplete = 0)       # Age Distribution: Abridged
#'
#' head(X)
#' }
#' 
#' @export
get_recorddata <- function(save = FALSE, verbose = TRUE, ...) {
  res <- read_API("structureddatarecords", save, verbose = verbose, ...)

  # Make sure dates are in 00/00/0000 format
  # Note that the result is not of class date
  # but of chr!
  res$TimeStart <- chr_to_date(res$TimeStart)
  res$TimeEnd <- chr_to_date(res$TimeEnd)

  # Loop through name and id names
  # and save the labelled character
  # to the Name columns
  res[names(values_env$id_to_fact)] <- Map(function(nm, id) {
    # Extract the columns from the df
    nm_vec <- res[, nm]
    id_vec <- res[, id]
    
    if (length(unique(nm_vec)) != length(unique(id_vec))) {
      stop("Column ", nm, " and ", id, " have different ",
           "unique values. Please report the exact same call that ",
           "raised this error at https://github.com/timriffe/DDSQLtools/issues")
    }

    # Set names of id to names to pass it to labelled
    # with correct labels
    vct_nm <- setNames(unique(nm_vec), unique(id_vec))

    # Create name column with ID as labels
    haven::labelled(nm_vec, labels = vct_nm)
  }, names(values_env$id_to_fact), values_env$id_to_fact)

  ## # Exclude ID columns
  res <- res[!grepl(paste(values_env$id_to_fact, collapse = "|"), names(res))]

  res
}

#' Download data
#' @param save Logical. Choose whether or not to save the data in an external 
#' \code{.Rdata} file in the working directory. Default: 
#' \code{FALSE};
#' @inheritParams linkGenerator
#' @keywords internal
read_API <- function(type, save, verbose = FALSE, ...){
  P <- linkGenerator(type = type, verbose_print = verbose, ...)
  # Temporary, just to check how the URL is constructed
  print(P)

  out <- rjson::fromJSON(file = P)
  ## print("URL saved")

  out <-
    out %>% 
    lapply(unlist) %>%    # list elements can either be lists or vectors
    lapply(as.list) %>%   # here now everything is homogenously a vector
    dplyr::bind_rows() %>%  # even if named elements differ still becomes rectangular
    lapply(trimws) %>%    # Remove leading/trailing spaces from the names
    as.data.frame(stringsAsFactors = FALSE)  # coerce to desired form

  out <- format.numeric.colums(out)

  if (save) {
    save_in_working_dir(data = out, file_name = paste0("UNPD_", 
                                                       type))
  }
  out
}

chr_to_date <- function(x) {
  x <- gsub("\\s{1}.+$", "", x)
  x <- format(as.Date(x, format = "%m/%d/%Y"), format = "%d/%m/%Y")
  x
}

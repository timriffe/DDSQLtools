#' Request data records directly from their ID
#'
#' @param ids a character vector of ID's that identify each data point
#'
#' @inheritParams read_API
#'
#' @return A data frame with the same rows as the number of ID's containing
#' the data values for that specific ID.
#'
#' @examples
#'
#' \dontrun{
#' extract_data("35444654")
#' }
#'
#' @keywords internal
extract_data <- function(ids, save_file = FALSE) {
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
                 save_file = save_file,
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
                            save_file = save_file,
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
get_locations <- function(save_file = FALSE, ...) {
  read_API("locations", save_file, ...)
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
get_locationtypes <- function(save_file = FALSE, ...) {
  read_API("locareatypes", save_file, ...)
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
get_subgroups <- function(save_file = FALSE, ...) {
  read_API("subGroups", save_file, ...)
}


#' Get information about available indicators (IndicatorTypeID)
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' I <- get_indicatortypes(addDefault = "false")
#' I[, c("PK_IndicatorTypeID", "Name", "ShortName")]
#' }
#' @export
get_indicatortypes <- function(save_file = FALSE, ...) {
  read_API("indicatortypes", save_file, ...)
}

#' Get information about available dataTypes (DataTypeID)
#' @inheritParams read_API
#' @examples
#'
#' \dontrun{
#' # Check which datatypes are available:
#' P <- get_datatypes()
#' head(P)
#' }
#' @export
get_datatypes <- function(save_file = FALSE, ...) {
  read_API("datatypes", save_file, ...)
}

#' Get information about available indicators (IndicatorID)
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' I <- get_indicators(addDefault = "false")
#' I[, c("PK_IndicatorTypeID", "Name", "ShortName")]
#' }
#' @export
get_indicators <- function(save_file = FALSE, ...) {
  read_API("indicators", save_file, ...)
}

#' Get information about available indicators (IndicatorID) and indicatortypeids (IndicatorTypeId)
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' I <- get_iitypes()
#' I
#'
#' only_itypes <- get_iitypes(indicatorTypeIds = 6)
#' only_itypes
#'
#' only_iids <- get_iitypes(indicatorIds = 229)
#' only_iids
#'
#' components <- get_iitypes(componentIds = 4)
#' components
#' }
#' @export
get_iitypes <- function(save_file = FALSE, ...) {
  read_API("indicatorindicatortypes", save_file, ...)
}

#' Get information about available types of data processes (DataProcessTypeID)
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' D <- get_dataprocesstype()
#' D[, c("PK_DataProcessTypeID", "Name", "ShortName")]
#' }
#' @export
get_dataprocesstype <- function(save_file = FALSE, ...) {
  read_API("dataProcessTypes", save_file, ...)
}

#' Get information about available data processes (DataProcessID and DataProcessTypeID)
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' D <- get_dataprocess()
#' D[, c("PK_DataProcessID", "Name", "ShortName")]
#' }
#' @export
get_dataprocess <- function(save_file = FALSE, ...) {
  read_API("dataProcesses", save_file, ...)
}

#' Gets all DataCatalog records (DataCatalogs)
#' @inheritParams read_API
#'
#' @details For an improved output, we combine DataCatalog data
#' with locations (\code{\link{get_locations}}),
#' dataprocess (\code{\link{get_dataprocess}}),
#' dataprocesstype (\code{\link{get_dataprocesstype}}) together with some
#' transformations (convert dates to date columns, renaming columns, etc...).
#'
#' @examples
#' \dontrun{
#'
#' dc <- get_datacatalog()
#' head(dc[, c("DataCatalogID", "DataProcessType", "LocName")])
#'
#' # Filter using locIds and DataProcessTypeIds
#' dc <- get_datacatalog(locIds = 76, dataProcessTypeIds = 2)
#'
#' }
#' @export
get_datacatalog <- function(save_file = FALSE, ...) {
  DataCatalog <- read_API("dataCatalogs", save_file, ...)

  Locations <- get_locations(
    addDefault = "false",
    includeDependencies = "false",
    includeFormerCountries = "false"
  )

  Locations <- within(Locations, {
    LocID <- PK_LocID
    PK_LocID <- NULL
    LocName <- Name
    Name <- NULL
  })[c("LocID", "LocTypeID", "LocName")]

  DataProcess <- get_dataprocess(addDefault = "false")

  DataProcess <- within(DataProcess, {
    DataProcessID <- PK_DataProcessID
    PK_DataProcessID <- NULL
    DataProcess <- Name
    Name <- NULL
    DataProcessShortName <- ShortName
    ShortName <- NULL
  })[c("DataProcessID", "DataProcessTypeID", "DataProcess", "DataProcessShortName")]

  DataProcessType <- get_dataprocesstype(addDefault = "false")

  DataProcessType <- within(DataProcessType, {
    DataProcessTypeID <- PK_DataProcessTypeID
    PK_DataProcessTypeID <- NULL
    DataProcessType <- Name
    Name <- NULL
    DataProcessTypeShortName <- ShortName
    ShortName <- NULL
  })[c("DataProcessTypeID", "DataProcessType", "DataProcessTypeShortName")]

  DataCatalog <- merge(
    DataCatalog,
    DataProcess,
    by = "DataProcessID",
    all.x = TRUE,
    all.y = FALSE
  )

  DataCatalog <- merge(
    DataCatalog,
    DataProcessType,
    by = "DataProcessTypeID",
    all.x = TRUE,
    all.y = FALSE
  )

  DataCatalog <-
    merge(
      DataCatalog,
      Locations,
      by = c("LocID", "LocName"),
      all.x = TRUE,
      all.y = FALSE
    )

  ind <- which(grepl("PK_DataCatalogID", names(DataCatalog)))
  names(DataCatalog)[ind] <- "DataCatalogID"

  cols_select <- c("DataCatalogID",
                   "LocID",
                   "LocTypeID",
                   "LocName",
                   "DataProcessTypeID",
                   "DataProcessType",
                   "DataProcessTypeShortName",
                   "DataProcessID",
                   "DataProcess",
                   "DataProcessShortName",
                   "Name",
                   "ShortName",
                   "OfficialName",
                   "OfficialShortName",
                   "ReferencePeriod",
                   "ReferenceYearStart",
                   "ReferenceYearEnd",
                   "ReferenceYearMid",
                   "FieldWorkStart",
                   "FieldWorkEnd",
                   "FieldWorkMiddle",
                   "ParentDataCatalogID",
                   "isSubnational")

  DataCatalog <- DataCatalog[cols_select]

  DataCatalog <- within(DataCatalog, {
    FieldWorkStart <- as.Date(FieldWorkStart, format = "%m/%d/%Y")
    FieldWorkEnd <- as.Date(FieldWorkEnd, format = "%m/%d/%Y")
  })

  DataCatalog <- DataCatalog[!is.na(DataCatalog$LocTypeID), ]

  order_rows <-
    order(
      DataCatalog$LocName,
      DataCatalog$ShortName,
      DataCatalog$ReferenceYearStart,
      na.last = FALSE
    )

  DataCatalog <- DataCatalog[order_rows, ]
  DataCatalog
}

#' Get information about available details for a given series of data
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' # You can provide all strings, all codes, or a combination of both
#' G <- get_seriesdata(dataProcessTypeIds = 0:15, # possible processes
#'                    indicatorTypeIds = 25,    # M[x]
#'                    locIds = "Egypt",             # Egypt
#'                    locAreaTypeIds = c("whole area", "rural", "urban"), # all possible types
#'                    subGroupIds = 2)
#' G
#' }
#' @export
get_seriesdata <- function(save_file = FALSE, ...) {
  read_API("structureddataseries", save_file, ...)
}

#' Get information about available data sources
#' @inheritParams read_API
#' @examples
#' \dontrun{
#' all_ds <- get_datasources()
#' head(all_ds)
#'
#' limit_ds <- get_datasources(years = 1981)
#'
#' head(limit_ds)
#'
#' shortn_ds <- get_datasources(shortNames = "DYB")
#'
#' head(shortn_ds)
#' }
#' @export
get_datasources <- function(save_file = FALSE, ...) {
  read_API("dataSources", save_file, ...)
}


#' Download structuredDataRecords data from the UNPD portal
#'
#' @inheritParams read_API
#' @param verbose Whether to print the translated query from strings to digits
#' for faster queries. By default set to TRUE.
#'
#' @details
#'
#' \code{get_recorddata} directly queries the endpoint
#' \code{structuredDataRecords}. For a list of all options available for this
#' endpoint, see the parameters for each endpoint at http://24.239.36.16:9654/un3/swagger/ui/index#!/StructuredData/StructuredData_GetStructuredDataRecords
#'
#' \code{get_recorddata} allows the user to supply string names for all
#' arguments that have equivalent \code{get_*} functions. For example,
#' \code{get_iitypes} for \code{indicatorIds}. The string used for all
#' of these arguments should be the one from the column \code{Name} in
#' the response from the \code{get_*} functions.
#'
#' By default, all results exclude columns related to the uncertainty
#' of the values requested (StandardErrorValue, ConfidenceInterval, etc...).
#' By setting the argument \code{includeUncertainty = TRUE}, the uncertainty
#' fields will be included in the final data frame.
#'
#' Once the data is read from the API, some transformations are applied:
#'
#' \itemize{
# \item{Columns \code{AreaName}, \code{DataReliabilityName}, \code{SubGroupName}, \code{DataStatusName}, \code{DataTypeName}, \code{DataTypeGroupName}, \code{IndicatorName}, \code{LocName}, \code{LocAreaTypeName}, \code{LocTypeName}, \code{ModelPatternName}, \code{ModelPatternFamilyName}, \code{PeriodGroupName}, \code{PeriodTypeName}, \code{RegName}, \code{SexName}, \code{StatisticalConceptName}, \code{SubGroupTypeName} are converted to labelled factors with \code{\link[haven]{labelled}}}
#' \item{\code{TimeStart} and \code{TimeEnd} are returned with format \code{'DD/MM/YYYY'}}
#' }
#'
#' @examples
#' \dontrun{
#'
#' #  You can provide all strings, all codes, or a combination of both
#' Y <- get_recorddata(dataProcessTypeIds = "Census",
#'                    indicatorTypeIds = 8, # and support numeric of string names
#'                    locIds = "egypt", # all arguments are case insensitive
#'                    locAreaTypeIds = "Whole area",
#'                    subGroupIds = "Total or All groups",
#'                    isComplete = "Abridged")
#'
#' head(Y)
#'
#' # Same thing only with codes
#' X <- get_recorddata(dataProcessTypeIds = 2,   # Census
#'                    indicatorTypeIds = 8, # Population by age and sex - abridged
#'                    locIds = 818,         # Egypt
#'                    locAreaTypeIds = 2,   # Whole area
#'                    subGroupIds = 2,      # Total or All groups
#'                    isComplete = 0)       # Age Distribution: Abridged
#'
#' head(X)
#'
#' # Same thing but limited to DataSourceYears
#' X <- get_recorddata(dataProcessTypeIds = 2,
#'                    indicatorTypeIds = 8,
#'                    locIds = 818,
#'                    locAreaTypeIds = 2,
#'                    subGroupIds = 2,
#'                    isComplete = 0,
#'                    dataSourceYears = 1980)
#'
#' head(X)
#'
#' # Same thing but limited to DataSourceShortNames and including the uncertainty
#' X <- get_recorddata(dataProcessTypeIds = 2,
#'                    indicatorTypeIds = 8,
#'                    locIds = 818,
#'                    locAreaTypeIds = 2,
#'                    subGroupIds = 2,
#'                    isComplete = 0,
#'                    dataSourceShortNames = "OECD 1980",
#'                    includeUncertainty = TRUE)
#'
#' head(X)
#'
#'}
#'
#'
#'
#' @export
get_recorddata <- function(save_file = FALSE, verbose = TRUE, ...) {
  res <- read_API("structureddatarecords",
                  save_file = save_file,
                  verbose = verbose,
                  ...)

  # Make sure dates are in 00/00/0000 format
  # Note that the result is not of class date
  # but of chr!
  res$TimeStart <- chr_to_date(res$TimeStart)
  res$TimeEnd <- chr_to_date(res$TimeEnd)

  # Loop through name and id names
  # and save the labelled character
  # to the Name columns
  ## res[names(values_env$id_to_fact)] <- Map(function(nm, id) {
  ##   # Extract the columns from the df
  ##   nm_vec <- res[, nm]
  ##   id_vec <- res[, id]

  ##   if (length(unique(nm_vec)) != length(unique(id_vec))) {
  ##     stop("Column ", nm, " and ", id, " have different ",
  ##          "unique values. Please report the exact same call that ",
  ##          "raised this error at https://github.com/timriffe/DDSQLtools/issues")
  ##   }

  ##   # Set names of id to names to pass it to labelled
  ##   # with correct labels
  ##   vct_nm <- stats::setNames(unique(nm_vec), unique(id_vec))

  ##   # Create name column with ID as labels
  ##   haven::labelled(nm_vec, labels = vct_nm)
  ## }, names(values_env$id_to_fact), values_env$id_to_fact)

  ## # Currently includes some ID columns
  ## browser()

  res <- res[values_env$col_order]

  uncertainty <- list(...)$includeUncertainty
  if (isFALSE(uncertainty) || is.null(uncertainty)) {
    uncertainty_cols <- c("HasUncertaintyRecord",
                          "StandardErrorValue",
                          "ConfidenceInterval",
                          "ConfidenceIntervalLowerBound",
                          "ConfidenceIntervalUpperBound")

    res <- res[setdiff(names(res), uncertainty_cols)]
  }


  res
}

#' Download data
#' @inheritParams linkGenerator
#' @param save_file Logical. Choose whether or not to save the data in an
#' external \code{.Rdata} file in the working directory. Default: \code{FALSE}.
#'
#' @keywords internal
read_API <- function(type, save_file, verbose = FALSE, ...) {
  P <- linkGenerator(type = type, verbose = verbose, ...)

  # Temporary, just to check how the URL is constructed
  cat("\n", P, "\n")

  out <- jsonlite::fromJSON(txt = P, flatten = TRUE)
  ## print("URL saved")

  # Cleaning up columns
  out <- as.data.frame(lapply(out, trimws), stringsAsFactors = FALSE)
  out <- format.numeric.colums(out)

  if (save_file) {
    save_in_working_dir(data = out, file_name = paste0("UNPD_", type))
  }

  out
}

chr_to_date <- function(x) {
  x <- gsub("T.+$", "", x)
  x <- format(as.Date(x, format = "%F"), format = "%d/%m/%Y")
  x
}

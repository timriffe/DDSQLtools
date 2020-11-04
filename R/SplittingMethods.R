#' Wrapper for Age-Splitting Methods
#'
#' @param X Input data. UN format.
#' @param fn Method to be called from DemoTools. Available alternatives:
#' \code{"beers", "grabill", "sprague"}.
#' @param verbose Logical value. If \code{TRUE} messages are printed
#' as the method is applied. Set \code{verbose = FALSE} to silent the function.
#' @param ... Other arguments to be passed on to other methods and functions.
#' @return A data.frame having the same number of columns as input data.
#' Different numbers of rows. UN format.
#' @seealso \code{\link[DemoTools]{graduate_beers}},
#' \code{\link[DemoTools]{graduate_grabill}},
#' \code{\link[DemoTools]{graduate_sprague}}.
#'
#' @examples
#'
#' # Example 1 --- Abridged data
#' P5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#'
#' W1 <- do_splitting(P5, fn = "beers")
#' W2 <- do_splitting(P5, fn = "grabill")
#' W3 <- do_splitting(P5, fn = "sprague")
#'
#' # Example 2 --- 1-year age groups
#' P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
#'
#' V1 <- do_splitting(P1, fn = "beers")
#' V2 <- do_splitting(P1, fn = "grabill")
#' V3 <- do_splitting(P1, fn = "sprague")
#'
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#'
#' W1[, select_columns]
#' V1[, select_columns]
#'
#' @export
do_splitting <- function(X,
                        fn = c("beers", "grabill", "sprague"),
                        verbose = TRUE,
                        ...) {

  A <- X$DataValue
  B <- X$AgeStart
  names(A) <- B
  OAG <- is_OAG(X)
  fn  <- match.arg(fn)
  C   <- match.call()

  E <- switch(fn,
              beers = graduate_beers(Value = A, Age = B, OAG = OAG, ...),
              grabill = graduate_grabill(Value = A, Age = B, OAG = OAG, ...),
              sprague = graduate_sprague(Value = A, Age = B, OAG = OAG, ...))

  fn <- paste0("graduate_", fn)
  G <-
    within(data.frame(DataValue = E), {
      AgeID <- NA_real_
      AgeStart <- as.numeric(names(E))
      AgeSpan <- 1
      AgeEnd <- AgeStart + AgeSpan
      AgeMid <- AgeStart + AgeSpan / 2
      AgeLabel <- AgeStart
      DataTypeName <- paste0("DemoTools::", fn)
      DataTypeID <- paste(deparse(C), collapse = "")
      ReferencePeriod <- unique(X$ReferencePeriod)
    })

  if (verbose) output_msg(fn, names(C))
  out <- format_output(X, G)
  out

}



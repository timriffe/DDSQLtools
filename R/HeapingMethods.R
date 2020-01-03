#' Wrapper for Age-Heaping Methods
#' 
#' @inheritParams do_splitting
#' @inheritParams DemoTools::Myers
#' @seealso 
#' \code{\link[DemoTools]{Whipple}},
#' \code{\link[DemoTools]{Myers}},
#' \code{\link[DemoTools]{Bachi}},
#' \code{\link[DemoTools]{CoaleLi}},
#' \code{\link[DemoTools]{Noumbissi}},
#' \code{\link[DemoTools]{Spoorenberg}},
#' \code{\link[DemoTools]{ageRatioScore}},
#' \code{\link[DemoTools]{KannistoHeap}},
#' \code{\link[DemoTools]{Jdanov}}.
#' @examples 
#' P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
#' 
#' H1 <- do_heaping(P1, fn = "Whipple")
#' H2 <- do_heaping(P1, fn = "Myers")
#' H3 <- do_heaping(P1, fn = "Bachi")
#' H4 <- do_heaping(P1, fn = "CoaleLi")
#' H5 <- do_heaping(P1, fn = "Noumbissi")
#' H6 <- do_heaping(P1, fn = "Spoorenberg")
#' H7 <- do_heaping(P1, fn = "ageRatioScore")
#' H8 <- do_heaping(P1, fn = "KannistoHeap")
#' H9 <- do_heaping(P1, fn = "Jdanov")
#' 
#' H <- rbind(H1, H2, H3, H4, H5, H6, H7, H8, H9)
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#' H[, select_columns]
#' 
#' # Silence the function with verbose = FALSE
#' H1 <- do_heaping(P1, fn = "Whipple", verbose = FALSE)
#' # ... or by specifying all arguments
#' H1 <- do_heaping(P1, fn = "Whipple", ageMin = 10, ageMax = 90, digit = 1)
#' @export
do_heaping <- function(X, 
                      fn = c("Whipple", 
                             "Myers", 
                             "Bachi", 
                             "CoaleLi", 
                             "Noumbissi", 
                             "Spoorenberg", 
                             "ageRatioScore",
                             "KannistoHeap", 
                             "Jdanov"), 
                      verbose = TRUE, 
                      ...) {
  
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))

  AgeStart = AgeEnd <- NULL # hack CRAN note
  
  A   <- X$DataValue
  B   <- X$AgeStart
  C   <- match.call()
  OAG <- is_OAG(X)
  fn  <- match.arg(fn)
  
  E <- switch(fn,
    Whipple = check_heaping_whipple(A, B, ...),
    Myers = check_heaping_myers(A, B, ...), 
    Bachi = check_heaping_bachi(A, B, ...), 
    CoaleLi = check_heaping_coale_li(A, B, ...),
    Noumbissi = check_heaping_noumbissi(A, B, ...),
    Spoorenberg = check_heaping_spoorenberg(A, B, ...),
    ageRatioScore = ageRatioScore(A, B, OAG = OAG, ...),
    KannistoHeap = check_heaping_kannisto(A, B, ...),
    Jdanov = check_heaping_jdanov(A, B, ...)
  )
  
  G <- data.frame(DataValue = E) %>%  
        mutate(AgeID = NA,
               AgeStart = min(X$AgeStart), 
               AgeEnd = max(X$AgeEnd),
               AgeMid = sum(X$AgeMid - X$AgeStart),
               AgeSpan = AgeEnd - AgeStart, 
               AgeLabel = paste0(AgeStart, "-", rev(X$AgeLabel)[1]),
               DataTypeName = paste0("DemoTools::", fn),
               DataTypeID = paste(deparse(C), collapse = ""),
               ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) controlOutputMsg2(fn, arg_names)
  out <- formatOutputTable(X, G)
  out
}


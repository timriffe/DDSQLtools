
#' Wrapper for Age-Heaping Methods
#' 
#' @inheritParams doSplitting
#' @inheritParams DemoTools::Myers
#' @seealso \code{\link[DemoTools]{Whipple}},
#' \code{\link[DemoTools]{Myers}},
#' \code{\link[DemoTools]{Bachi}},
#' \code{\link[DemoTools]{CoaleLi}},
#' \code{\link[DemoTools]{Noumbissi}},
#' \code{\link[DemoTools]{Spoorenberg}},
#' \code{\link[DemoTools]{ageRatioScore}},
#' \code{\link[DemoTools]{AHI}},
#' \code{\link[DemoTools]{WI}}.
#' @examples 
#' P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
#' 
#' H1 <- doHeaping(P1, fn = "Whipple")
#' H2 <- doHeaping(P1, fn = "Myers")
#' H3 <- doHeaping(P1, fn = "Bachi")
#' H4 <- doHeaping(P1, fn = "CoaleLi")
#' H5 <- doHeaping(P1, fn = "Noumbissi")
#' H6 <- doHeaping(P1, fn = "Spoorenberg")
#' H7 <- doHeaping(P1, fn = "ageRatioScore")
#' H8 <- doHeaping(P1, fn = "AHI")
#' H9 <- doHeaping(P1, fn = "WI")
#' 
#' H <- rbind(H1, H2, H3, H4, H5, H6, H7, H8, H9)
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#' H[, select_columns]
#' 
#' # Silence the function with verbose = FALSE
#' H1 <- doHeaping(P1, fn = "Whipple", verbose = FALSE)
#' # ... or by specifying all arguments
#' H1 <- doHeaping(P1, fn = "Whipple", ageMin = 10, ageMax = 90, digit = 1)
#' @export
#' 
doHeaping <- function(X, fn = c("Whipple", "Myers", "Bachi", "CoaleLi", 
                                "Noumbissi", "Spoorenberg", "ageRatioScore",
                                "AHI", "WI"), verbose = TRUE, ...) {
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))

  AgeStart = AgeEnd <- NULL # hack CRAN note
  
  A   <- X$DataValue
  B   <- X$AgeStart
  C   <- match.call()
  OAG <- is_OAG(X)
  fn  <- match.arg(fn)
  
  E <- switch(fn,
    Whipple = Whipple(A, B, ...),
    Myers = Myers(A, B, ...), 
    Bachi = Bachi(A, B, ...), 
    CoaleLi = CoaleLi(A, B, ...),
    Noumbissi = Noumbissi(A, B, ...),
    Spoorenberg = Spoorenberg(A, B, ...),
    ageRatioScore = ageRatioScore(A, B, OAG = OAG, ...),
    AHI = AHI(A, B, ...),
    WI = WI(A, B, ...)
  )
  
  G <- E %>% as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%  
    mutate(AgeID = NA,
           AgeStart = min(X$AgeStart), 
           AgeEnd = max(X$AgeEnd),
           AgeMid = sum(X$AgeMid - X$AgeStart),
           AgeSpan = AgeEnd - AgeStart, 
           AgeLabel = paste0(AgeStart, "-", rev(X$AgeLabel)[1]),
           DataTypeName = paste0("DemoTools::", fn),
           DataTypeID = deparse(C),
           ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) controlOutputMsg2(fn, arg_names)
  out <- formatOutputTable(X, G)
  return(out)
}






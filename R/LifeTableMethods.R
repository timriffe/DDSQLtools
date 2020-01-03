#' Wrapper for Life Table Methods
#' @inheritParams doSplitting
#' @inherit doSplitting return
#' @seealso \code{\link[DemoTools]{LTabr}}
#' @examples 
#' mx <- DDSQLtools.data$Mx5
#' doLifeTable(X = mx)
#' @export
doLifeTable <- function(X, 
                        verbose = TRUE, 
                        ...) {
  
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))
  
  Age = AgeStart = AgeEnd = AgeInt <- NULL # hack CRAN note
  
  A   <- X$DataValue
  B   <- X$AgeStart
  C   <- match.call()
  # OAG <- is_OAG(X)
  fn  <- "lt_abridged"
  sex <- unique(X$SexID) 
  sex <- if (sex == 1) "m" else if (sex == 2) "f" else "b"
  
  E <- lt_abridged(nMx = A,
                   Age = X$AgeStart,
                   AgeInt = X$AgeSpan,
                   Sex = sex, 
                   ...)
  
  G <- gather(E, key = "IndicatorID", value = "DataValue", -c(1:2)) %>% 
    dplyr::rename(AgeSpan = AgeInt, AgeStart = Age) %>%  
    mutate(AgeID = NA,  # This NA field should be updated when real UN data will be available
           AgeEnd = NA,
           AgeMid = NA,
           AgeLabel = NA,
           DataTypeName = paste0("DemoTools::", fn),
           DataTypeID = paste(deparse(C), collapse = ""),
           ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) controlOutputMsg2(fn, arg_names)
  out <- formatOutputTable(X, G)
  out
}

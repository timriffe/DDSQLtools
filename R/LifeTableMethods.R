# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Tue Nov 27 20:50:05 2018
# --------------------------------------------------- #


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
  fn  <- "LTabr"
  sex <- unique(X$SexID) 
  sex <- if (sex == 1) "m" else if (sex == 2) "f" else "b"
  
  E <- LTabr(nMx = A,
             Age = X$AgeStart,
             AgeInt = X$AgeSpan,
             Sex = sex, 
             ...)
  
  G <- gather(E, key = "IndicatorID", value = "DataValue", -c(1:2)) %>% 
    dplyr::rename(AgeSpan = AgeInt, AgeStart = Age) %>%  
    mutate(AgeID = NA,
           # AgeEnd = max(X$AgeEnd),
           # AgeMid = sum(X$AgeMid - X$AgeStart),
           # AgeLabel = paste0(AgeStart, "-", rev(X$AgeLabel)[1]),
           DataTypeName = paste0("DemoTools::", fn),
           DataTypeID = deparse(C),
           ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) controlOutputMsg2(fn, arg_names)
  out <- formatOutputTable(X, G)
  return(out)
}


  
  

# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Wed Nov 28 11:38:26 2018
# --------------------------------------------------- #

#' Wrapper for extrapolating old-age human mortality data using 
#' various mortality models ("laws")
#' @inheritParams DemoTools::extra_mortality
#' @inheritParams doLifeTable
#' @inherit doLifeTable return
#' @source The function is based on the methods implemented in the 
#' \code{MortalityLaws} R package.
#' @seealso 
#' \code{\link[DemoTools]{extra_mortality}}
#' \code{\link[MortalityLaws]{MortalityLaw}}
#' @examples 
#' # Example 1 - abridged data
#' # Extrapolate old-age mortality data
#' E <- doExtrapolate(X = DDSQLtools.data$Mx5,           # data - UN format 
#'                    x_fit = c(75, 80, 85, 90),         # ages to be used in fitting
#'                    x_extr = c(90, 95, 100, 105, 110), # ages for which the extrapolation is applied
#'                    law = "kannisto")                  # the model
#' E
#' 
#' # Build 2 life table: 1st using the original data and the 2nd using the extended data
#' LT1 <- doLifeTable(X = DDSQLtools.data$Mx5)
#' LT2 <- doLifeTable(X = E)
#' 
#' LT1[LT1$IndicatorID == "ex", c("AgeStart", "DataValue")]
#' LT2[LT2$IndicatorID == "ex", c("AgeStart", "DataValue")]
#' 
#' dim(LT1)
#' dim(LT2) # note the 2nd life table has few extra rows
#' @export
doExtrapolate <- function(X,
                          x_fit,
                          x_extr,
                          law = "kannisto",
                          verbose = TRUE,
                          ...) {

  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))

  Age = AgeStart = AgeEnd = AgeInt <- NULL # hack CRAN note
  
  A   <- X$DataValue
  B   <- X$AgeStart
  C   <- match.call()
  # OAG <- is_OAG(X)
  fn  <- "extra_mortality"
  
  E <- extra_mortality(mx = A, 
                       x = B, 
                       x_fit = x_fit, 
                       x_extr = x_extr, 
                       law = law, 
                       ...)
  
  G <- E$values %>% 
    as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%  
    mutate(AgeID = NA,
           AgeStart = as.numeric(names(E$values)),
           AgeSpan = NA,
           # AgeEnd =  NA,
           # AgeMid = sum(X$AgeMid - X$AgeStart),
           # AgeLabel = paste0(AgeStart, "-", rev(X$AgeLabel)[1]),
           SexID = unique(X$SexID), 
           IndicatorID = unique(X$IndicatorID),
           DataTypeName = paste0("DemoTools::", fn),
           DataTypeID = paste(deparse(C), collapse = ""),
           ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) controlOutputMsg2(fn, arg_names)
  out <- formatOutputTable(X, G)
  return(out)
}





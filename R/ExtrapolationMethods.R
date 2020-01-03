#' Wrapper for Extrapolating Old-Age Human Mortality Data Using 
#' Various Mortality Models ("laws")
#' @inheritParams DemoTools::lt_rule_m_extrapolate
#' @inheritParams do_lifetable
#' @inherit do_lifetable return
#' @source The function is based on the methods implemented in the 
#' \code{MortalityLaws} R package.
#' @seealso 
#' \code{\link[DemoTools]{lt_rule_m_extrapolate}}
#' \code{\link[MortalityLaws]{MortalityLaw}}
#' @examples 
#' # Example 1 - abridged data
#' # Extrapolate old-age mortality data
#' E <- do_extrapolate(X = DDSQLtools.data$Mx5,           # data - UN format 
#'                    x_fit = c(75, 80, 85, 90),         # ages to be used in fitting
#'                    x_extr = c(90, 95, 100, 105, 110), # ages for which the extrapolation is applied
#'                    law = "kannisto")                  # the model
#' E
#' 
#' # Build 2 life table: 1st using the original data and the 2nd using the extended data
#' LT1 <- do_lifetable(X = DDSQLtools.data$Mx5)
#' LT2 <- do_lifetable(X = E)
#' 
#' LT1[LT1$IndicatorID == "ex", c("AgeStart", "DataValue")]
#' LT2[LT2$IndicatorID == "ex", c("AgeStart", "DataValue")]
#' 
#' dim(LT1)
#' dim(LT2) # note the 2nd life table has few extra rows
#' @export
do_extrapolate <- function(X,
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
  fn  <- "lt_rule_m_extrapolate"
  
  E <- lt_rule_m_extrapolate(mx = A, 
                             x = B, 
                             x_fit = x_fit, 
                             x_extr = x_extr, 
                             law = law, 
                             ...)

  age_names <- as.numeric(names(E$values))
  
  G <-
    E$values %>% 
    as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%  
    mutate(AgeID = NA_real_,
           AgeStart = age_names,
           AgeSpan = age2int(age_names),
           AgeEnd = NA_real_,
           AgeMid = NA_real_,
           AgeLabel = NA_real_,
           SexID = unique(X$SexID), 
           IndicatorID = unique(X$IndicatorID),
           DataTypeName = paste0("DemoTools::", fn),
           DataTypeID = paste(deparse(C), collapse = ""),
           ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) output_msg(fn, arg_names)
  out <- format_output(X, G)
  out
}

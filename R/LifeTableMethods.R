#' Wrapper for Life Table Methods
#' @inheritParams do_splitting
#' @inherit do_splitting return
#' @seealso \code{\link[DemoTools]{lt_abridged}}
#' @examples 
#' mx <- DDSQLtools.data$Mx5
#' do_lifetable(X = mx)
#' @export
do_lifetable <- function(X, 
                        verbose = TRUE, 
                        ...) {
  
  C   <- match.call()
  # OAG <- is_OAG(X)
  fn  <- "lt_abridged"
  sex <- unique(X$SexID) 
  sex <- if (sex == 1) "m" else if (sex == 2) "f" else "b"

  E <- lt_abridged(nMx = X$DataValue,
                   Age = X$AgeStart,
                   AgeInt = X$AgeSpan,
                   Sex = sex,
                   ...)
  
  G <-
    gather(E, key = "IndicatorID", value = "DataValue", -c(1:2)) %>% 
    dplyr::rename(AgeSpan = AgeInt, AgeStart = Age) %>%  
    mutate(AgeID = NA_real_,  # This NA field should be updated when real UN data will be available
           AgeEnd = NA_real_,
           AgeMid = NA_real_,
           AgeLabel = NA_real_,
           DataTypeName = paste0("DemoTools::", fn),
           DataTypeID = paste(deparse(C), collapse = ""),
           ReferencePeriod = unique(X$ReferencePeriod))
  
  if (verbose) output_msg(fn, names(C))
  out <- format_output(X, G)
  out
}

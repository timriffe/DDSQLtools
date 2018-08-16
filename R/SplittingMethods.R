
#' Wrapper for Age-Splitting Methods
#' 
#' @param X Input data. UN format.
#' @param fn Method to be called from DemoTools. Available aternatives: 
#' \code{"beers", "grabill", "sprague"}.
#' @param verbose Logical value. If \code{TRUE} messages are printed 
#' as the method is applied. Set \code{verbose = FALSE} to silent the function.
#' @param ... Other arguments to be passed on to other methods and functions.
#' @return A data.frame having the same number of colums as input data. Different 
#' numbers of rows. UN format.
#' @seealso \code{\link[DemoTools]{beers}}, 
#' \code{\link[DemoTools]{grabill}}, 
#' \code{\link[DemoTools]{sprague}}.
#' @examples 
#' # Example 1 --- Abridged data
#' P5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' 
#' W1 <- doSplitting(P5, fn = "beers") 
#' W2 <- doSplitting(P5, fn = "grabill")
#' W3 <- doSplitting(P5, fn = "sprague")
#' 
#' # Example 2 --- 1-year age groups   
#' P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
#' 
#' V1 <- doSplitting(P1, fn = "beers") 
#' V2 <- doSplitting(P1, fn = "grabill") 
#' V3 <- doSplitting(P1, fn = "sprague") 
#' @export
#' 
doSplitting <- function(X, fn = c("beers", "grabill", "sprague"), 
                        verbose = TRUE, ...) {
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))
  AgeStart = AgeSpan <- NULL # hack CRAN note
  
  A <- X$DataValue
  B <- X$AgeStart
  names(A) <- B
  OAG <- is_OAG(X)
  fn  <- match.arg(fn)
  
  DTF <- get(fn)  # DemoTools Function
  E   <- DTF(popmat = A, Age = B, OAG = OAG, ...)
  G   <- E %>% as.data.frame() %>% 
    dplyr::rename(DataValue = "V1") %>%
    mutate(AgeStart = as.numeric(rownames(E)), 
           AgeSpan = 1, 
           AgeEnd = AgeStart + AgeSpan,
           AgeMid = AgeStart + AgeSpan/2,
           AgeLabel = AgeStart,
           DataProcessType = fn,
           ReferencePeriod = unique(X$ReferencePeriod)) 
  
  C <- match.call()
  if (verbose) controlOutputMsg2(fn, arg_names)
  G$DataProcess <- deparse(C)
  out <- formatOutputTable(X, G)
  return(out)
}



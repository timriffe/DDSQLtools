

#' Wrap function for the age-splitting methods: beers, grabill and sprague
#' 
#' @description Wrap function for \code{\link[DemoTools]{beers}}, 
#' \code{\link[DemoTools]{grabill}} and \code{\link[DemoTools]{sprague}}
#' @param X Input data. UN format.
#' @param fn Method to be called from DemoTools
#' @param ... Other arguments to be passed on to other methods and functions.
#' @return A data.frame having the same number of colums as input data. Different 
#' numbers of rows. UN format.
#' @examples 
#' # Example 1 --- Abridged data
#' P5 <- DDSQLtools.data$Pop5_Egypt_DB
#' 
#' W1 <- doSplitting(P5, fn = "beers") 
#' W2 <- doSplitting(P5, fn = "grabill")
#' W3 <- doSplitting(P5, fn = "sprague")
#' 
#' # Example 2 --- 1-year age group   
#' P1 <- DDSQLtools.data$Pop1_Egypt_DB
#' 
#' V1 <- doSplitting(P1, fn = "beers") 
#' @export
#' 
doSplitting <- function(X, fn = c("beers", "grabill", "sprague"), ...) {
  AgeStart = AgeSpan <- NULL # hack CRAN note
  
  A <- X$DataValue
  B <- X$AgeStart
  names(A) <- B
  OAG <- is.OAG(X)
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
           DataProcess = paste0("DemoTools::", fn),
           DataProcessType = fn,
           ReferencePeriod = unique(X$ReferencePeriod)) 
  
  out <- formatOutputTable(X, G)
  return(out)
}


#' Check whether the input data contains an open age interval 
#' @inheritParams doSplitting
#' @examples 
#' p5 <- DDSQLtools.data$Pop5_Egypt_DB
#' is.OAG(p5)
#' @export
is.OAG <- function(X){
  cond <- !(X$AgeLabel %in% c("Total", "Unknown")) # Check for "Total"
  Y    <- X[cond, ] 
  out  <- any(Y$AgeSpan == -1)
  return(out)
}



#' Prepare output table in wrap functions
#' @inheritParams doSplitting
#' @param G The G object in wrap functions
#' @keywords internal
#' 
formatOutputTable <- function(X, G) {
  H <- data.frame(matrix(NA, ncol = ncol(X), nrow = nrow(G)))
  colnames(H) <- colnames(X)
  CnameX <- colnames(X)
  CnameG <- colnames(G)
  
  for (i in 1:length(CnameG)) {
    ii <- CnameG[i]
    H[, ii] <- G[, ii]
  }
  
  for (j in 1:length(CnameX)) {
    jj <- CnameX[j]
    isEmpty <- !(jj %in% CnameG)
    
    if (isEmpty) {
      value  <- unlist(X[1, jj])
      isUniqueValue <- length(unique(value)) == 1
      H[, jj] <- if (isUniqueValue) value else NaN
    }
  }
  
  return(as.tibble(H))
}


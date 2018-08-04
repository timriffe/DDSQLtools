

#' Wrapper for the Age-Splitting Methods: beers, grabill and sprague
#' 
#' @param X Input data. UN format.
#' @param fn Method to be called from DemoTools
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
#' # Example 2 --- 1-year age group   
#' P1 <- DDSQLtools.data$Pop1_Egypt_M_DB
#' 
#' V1 <- doSplitting(P1, fn = "beers") 
#' V2 <- doSplitting(P1, fn = "grabill") 
#' V3 <- doSplitting(P1, fn = "sprague") 
#' @export
#' 
doSplitting <- function(X, fn = c("beers", "grabill", "sprague"), ...) {
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
  controlOutputMsg(fn, C)
  G$DataProcess <- deparse(C)
  out <- formatOutputTable(X, G)
  return(out)
}


#' Check whether the input data contains an open age interval 
#' @inheritParams doSplitting
#' @examples 
#' p5 <- DDSQLtools.data$Pop5_Egypt_M_DB
#' is_OAG(p5)
#' @export
is_OAG <- function(X){
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


#' Print messages
#' @inheritParams doSplitting
#' @keywords internal
#' @export
controlOutputMsg <- function(fn, C) {
  msg = "Additional arguments to control the output in "
  switch(fn,
         beers = if (is.null(C$method) | is.null(C$johnson)) {
           message(msg, fn, ": `method` and `johnson`. Default: method = 'mod', johnson = FALSE.")
         }, 
         ageSexAccuracy = if (is.null(C$method) | is.null(C$adjust)) {
           message(msg, fn, ": `method` and `adjust`. Default: method = 'UN', adjust = TRUE.")
         }, 
         AHI = if (is.null(C$Agei)) message(msg, fn, ": `Agei`. Default: 90."),
         WI = if (is.null(C$Ages)) message(msg, fn, ": `Ages`. Default: seq(95, 105, by = 5)."),
         Whipple = if (is.null(C$digit)) message(msg, fn, ": `digit`. Default: c(0, 5)."),
         Bachi   = if (is.null(C$pasex)) message(msg, fn, ": `pasex`. Default: FALSE."), 
         CoaleLi = if (is.null(C$terms) | is.null(C$digit)) {
           message(msg, fn, ": `terms` and `digit`. Default: terms = 5, digit = 0.")
         }, 
         Noumbissi     = if (is.null(C$digit)) message(msg, fn, ": `digit`. Default: 0."),
         ageRatioScore = if (is.null(C$method)) message(msg, fn, ": `method`. Default: 'UN'.")
  )
}


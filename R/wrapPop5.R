

#' Wrap function for beers, grabill and sprague functions
#' 
#' @description Wrap function for \code{\link[DemoTools]{beers}}, 
#' \code{\link[DemoTools]{grabill}} and \code{\link[DemoTools]{sprague}}
#' @param X Input data. UN format.
#' @param fn Method to be called from DemoTools
#' @param ... Other arguments to be passed on to the \code{\link[DemoTools]{beers}}, 
#' \code{\link[DemoTools]{grabill}} or \code{\link[DemoTools]{sprague}} functions.
#' @return A data.frame having the same number of colums as input data. Different 
#' numbers of rows. UN format.
#' @examples 
#' W1 <- wrapPop5(P5_Egypt1976, fn = "beers") 
#' W2 <- wrapPop5(P5_Egypt1976, fn = "grabill")
#' W3 <- wrapPop5(P5_Egypt1976, fn = "sprague") 
#' @export
#' 
wrapPop5 <- function(X, fn = c("beers", "grabill", "sprague"), ...) {
  ReferencePeriod = AgeStart = DataValue = AgeSpan = AgeLabel <- NULL # hack to pass CRAN checks
  
  C <- X %>% select(ReferencePeriod, AgeStart, DataValue) %>%
    spread(key = ReferencePeriod, value = DataValue) %>% 
    as.data.frame()
  Age <- C$AgeStart
  rownames(C) <- Age
  D   <- C %>% select(-AgeStart)
  OAG <- isOAG(D)
  fn  <- match.arg(fn)
  DTF <- get(fn)  # DemoTools Function
  E   <- DTF(popmat = D, Age = Age, OAG = OAG, ...)
  G   <- E %>% as.data.frame() %>% 
    mutate(AgeStart = as.numeric(rownames(E)), 
           AgeSpan = 1, 
           AgeEnd = AgeStart + AgeSpan,
           AgeMid = AgeStart + AgeSpan/2,
           AgeLabel = AgeStart) %>% 
    gather(key = ReferencePeriod, value = DataValue, -(AgeStart:AgeLabel)) %>% 
    mutate(ReferencePeriod = as.numeric(ReferencePeriod))
  
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
      H[, jj] <- unlist(X[1, jj])
    }
    
    IdontKnowTheRule <- jj %in% c("PK_StructuredDataID", "AgeID")
    if (IdontKnowTheRule) {
      H[, jj] <- NA
    }
  }
  return(as.tibble(H))
}


#' Check whether the input data contains an open age interval 
#' @inheritParams wrapPop5
#' @keywords internal
#' 
isOAG <- function(X){
  any(X$AgeSpan == -1)
}


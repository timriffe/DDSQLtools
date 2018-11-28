

#' Check whether the input data contains an open age interval 
#' @inheritParams doSplitting
#' @return Logical.
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
controlOutputMsg2 <- function(fn, arg_names) {
  # DemoTools arguments renamed or fed automatically in DDSQL functions
  args_ren <- c("Age", "Value", "OAG", "popmat", "Males", "Females", "mx", "x") 
  # DDSQLtools arguments
  args_DDSQL <- c(arg_names, args_ren)
  # DemoTools arguments
  args_DT <- formalArgs(get(fn))
  # Logical test
  L  <- !(args_DT %in% args_DDSQL)
  missing_args <- args_DT[L]
  
  if (sum(L) != 0) {
    message("Additional (optional) arguments to control the output for the ",
            "DemoTools::", fn, " method:\n", paste(missing_args, collapse = " "), 
            "\nCheck ?", fn, " for details and default values.\n")
  }
}




# ----------------------------------------------

#' Utility functions for wrappers. Name and value checking, for example.
#' 
#' make sure values in a column are as expected
#' @description A generic checking utility. This will need various
#' presets of values for the various possible columns, and may have its own
#' wrappers for each.
#' 
#' @param x vector of values, \code{factor},\code{character},\code{numeric},\code{integer}, etc.
#' @param expected.values set of valid values if \code{categorical == TRUE}, otherwise the valid value range.
#' @param categorical logical. Does the value represent categories? Default \code{FALSE} for magnitudes or similar.
#' 
#' 
#' @examples 
#' expected.values <- c(0,1)
#' # can be a check on probabilities, perhaps they're in logit and we dont' realize it?
#' x <- runif(10)
#' x <- log(x/(1-x))
#' #out <- try(check_values(x,c(0,1),categorical = FALSE))
#' # de facto unit test..
#' #stopifnot(class(out) == "try-error")
#' 
#' # factors are checked against characters here.
#' y <- as.factor(c("a","b","c"))
#' check_values(y,letters,categorical = TRUE)
#' @keywords internal
#' @export 
check_values <- function(x, expected.values, categorical = FALSE){
	
	if (!categorical){
		
		# if we're checking numeric values, just check range
		if (class(x) == "numeric"){
			stopifnot(min(x) >= min(expected.values) &
							max(x) >= max(expected.values))
		}
		
	} else {
		# otherwise, coerce to character, as case may be
		x <- as.character(x)
		stopifnot(all(x %in% expected.values))
	}

}





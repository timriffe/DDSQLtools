
# utility functions for wrappers. Name and value checking, for example.

#' make sure values in a column are as expected
#' @description A generic checking utility. This will need various
#' presets of values for the various possible columns, and may have its own
#' wrappers for each.
#' 
#' @param x vector of values, \code{factor},\code{character},\code{numeric},\code{integer}, etc.
#' @param expected.values set of valid values if \code{categorical == TRUE}, otherwise the valid value range.
#' @param categorical logical. Does the value represent categories? Default \code{FALSE} for magnitudes or similar.
#' 
#' @export 
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





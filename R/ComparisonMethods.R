

#' Wrapper for Population Comparison
#' 
#' @param pop1 Input data for population 1. UN format.
#' @param pop2 Input data for population 2. UN format.
#' @param fn Method to be called from DemoTools. Available aternatives: 
#' \code{"ID", "IRD"}.
#' @inheritParams doSplitting
#' @inherit doSplitting return
#' @seealso 
#' \code{\link[DemoTools]{ID}}, 
#' \code{\link[DemoTools]{IRD}}.
#' @examples 
#' # ----------------------------------------------
#' # 1-year age groups
#' P1m <- DDSQLtools.data$Pop1_Egypt_M_DB
#' P1f <- DDSQLtools.data$Pop1_Egypt_F_DB
#' 
#' C1 <- doCompare(pop1 = P1m, pop2 = P1f, fn = "ID")
#' C2 <- doCompare(pop1 = P1m, pop2 = P1f, fn = "IRD")
#' 
#' # ----------------------------------------------
#' # 5-year age groups
#' P5m <- DDSQLtools.data$Pop5_Egypt_M_DB
#' P5f <- DDSQLtools.data$Pop5_Egypt_F_DB
#' 
#' C3 <- doCompare(pop1 = P5m, pop2 = P5f, fn = "ID")
#' C4 <- doCompare(pop1 = P5m, pop2 = P5f, fn = "IRD")
#' 
#' 
#' C <- rbind(C1, C2, C3, C4)
#' C[, c("DataProcess", "DataProcessType", "ReferencePeriod", "DataValue")]
#' @export
#' 
doCompare <- function(pop1, pop2, fn = c("ID", "IRD"), 
                            verbose = TRUE, ...) {
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))
  validateInput(input)
  
  A1  <- pop1$DataValue
  A2  <- pop2$DataValue
  B   <- pop1$AgeStart
  # OAG <- is_OAG(pop1)
  fn  <- match.arg(fn)
  sex <- c("Male", "Female", "Both sexes")
  sex_id   <- if (pop1$SexID[1] == pop2$SexID[1]) pop1$SexID[1] else 3
  sex_name <- sex[sex_id]
  
  E <- switch(fn,
              ID = ID(A1, A2),
              IRD = IRD(A1, A2)
  )
  
  AgeMid = AgeStart = AgeEnd <- NULL # hack CRAN note
  G <- E %>% as.data.frame() %>% 
    dplyr::rename(DataValue = ".") %>%
    mutate(AgeStart = min(B), 
           AgeMid = sum(pop1$AgeMid - pop1$AgeStart),
           AgeEnd = AgeMid * 2,
           AgeSpan = AgeEnd - AgeStart, 
           AgeLabel = paste0(min(B), "-", rev(pop1$AgeLabel)[1]),
           DataProcessType = fn,
           ReferencePeriod = unique(pop1$ReferencePeriod),
           SexID = sex_id,
           SexName = sex_name)
  
  C <- match.call()
  if (verbose) controlOutputMsg2(fn, arg_names)
  G$DataProcess <- deparse(C)
  out <- formatOutputTable(pop1, G)
  return(out)  
}










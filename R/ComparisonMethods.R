# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sun Dec 16 13:01:40 2018
# --------------------------------------------------- #

#' Wrapper for Population Comparison
#' 
#' @param pop1 Input data for population 1. UN format;
#' @param pop2 Input data for population 2. UN format;
#' @param fn Method to be called from DemoTools. Available aternatives: 
#' \code{"ID", "IRD", "ADM", "RDM"};
#' @inheritParams doSplitting
#' @inherit doSplitting return
#' @seealso 
#' \code{\link[DemoTools]{ID}}, 
#' \code{\link[DemoTools]{IRD}},
#' \code{\link[DemoTools]{ADM}},
#' \code{\link[DemoTools]{RDM}}.
#' @examples 
#' # ------------------------------------------
#' # Example 1 - Compute the Index of dissimilarity between 2 populations
#' 
#' P1m <- DDSQLtools.data$Pop1_Egypt_M_DB # 1-year age groups data
#' P1f <- DDSQLtools.data$Pop1_Egypt_F_DB
#' P5m <- DDSQLtools.data$Pop5_Egypt_M_DB # 5-year age groups data
#' P5f <- DDSQLtools.data$Pop5_Egypt_F_DB
#' 
#' C1 <- doCompare(pop1 = P1m, pop2 = P1f, fn = "ID")
#' C2 <- doCompare(pop1 = P5m, pop2 = P5f, fn = "ID")
#' 
#' # ------------------------------------------
#' # Example 2 - Compute the Index of relative difference between 2 populations
#' C3 <- doCompare(pop1 = P1m, pop2 = P1f, fn = "IRD")
#' C4 <- doCompare(pop1 = P5m, pop2 = P5f, fn = "IRD")
#' 
#' select_columns <- c("AgeID", "AgeStart", "AgeMid", "AgeEnd", "AgeLabel",
#'                     "DataTypeName", "DataTypeID", "DataValue")
#' C <- rbind(C1, C2, C3, C4)
#' C[, select_columns]
#' 
#' # ------------------------------------------
#' # Example 3 - Compute the Mean Absolute Difference in survival 
#' # rates between 2 populations
#' 
#' # Since we do not have l[x] data yet, we will make up some dummy data from 
#' # death-rates just to check the example (to be updated).
#' library(dplyr)
#' Mx <- DDSQLtools.data$Mx5
#' 
#' lx.A <- Mx %>% doLifeTable %>% filter(IndicatorID == "lx")
#' lx.B <- Mx %>% mutate(DataValue = DataValue * 0.98, SexID = 0) %>% 
#'   doLifeTable %>% filter(IndicatorID == "lx")
#' 
#' C5 <- doCompare(pop1 = lx.A, pop2 = lx.B, fn = "ADM")
#' 
#' # ------------------------------------------
#' # Example 4 - Compute the Mean Absolute Difference in age-ratios of survival
#' # rates between 2 populations
#' C6 <- doCompare(pop1 = lx.A, pop2 = lx.B, fn = "RDM")
#' @export
doCompare <- function(pop1, 
                      pop2, 
                      fn = c("ID", "IRD", "ADM", "RDM"), 
                      verbose = TRUE, ...) {
  
  input <- as.list(environment())
  arg_names <- c(names(input), names(list(...)))
  validateInput(input)
  
  A1 <- pop1$DataValue
  A2 <- pop2$DataValue
  B  <- pop1$AgeStart
  C  <- match.call()
  # OAG <- is_OAG(pop1)
  fn  <- match.arg(fn)
  sex <- c("Male", "Female", "Both sexes")
  sex_id   <- if (pop1$SexID[1] == pop2$SexID[1]) pop1$SexID[1] else 3
  sex_name <- sex[sex_id]
  
  E <- switch(fn,
              ID = ID(pop1 = A1, pop2 = A2),
              IRD = IRD(pop1 = A1, pop2 = A2),
              ADM = ADM(lx1 = A1, lx2 = A2, Age1 = B, Age2 = B, ...),
              RDM = RDM(lx1 = A1, lx2 = A2, Age1 = B, Age2 = B, ...)
  )
  
  AgeStart = AgeEnd <- NULL # hack CRAN note
  G <- data.frame(DataValue = E) %>%
        mutate(AgeID = NA,
               AgeStart = min(pop1$AgeStart), 
               AgeEnd = max(pop1$AgeEnd),
               AgeMid = sum(pop1$AgeMid - pop1$AgeStart),
               AgeSpan = AgeEnd - AgeStart, 
               AgeLabel = paste0(AgeStart, "-", rev(pop1$AgeLabel)[1]),
               DataTypeName = paste0("DemoTools::", fn),
               DataTypeID = paste(deparse(C), collapse = ""),
               ReferencePeriod = unique(pop1$ReferencePeriod),
               SexID = sex_id,
               SexName = sex_name)
  
  if (verbose) controlOutputMsg2(fn, arg_names)
  out <- formatOutputTable(pop1, G)
  return(out)  
}




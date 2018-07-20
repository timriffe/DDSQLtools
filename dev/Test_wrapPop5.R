# Fri Jul 20 10:07:56 2018 ------------------------------
# Marius Pascariu

remove(list = ls())

library(DemoTools)
library(readxl)
library(tidyverse)

# Load data
X <- read_excel("Pop5_Metadata.xlsx")
A <- read_excel("DemoData_Export_Pop5_Egypt+India_DB.xlsx")
# remove duplicate columns
A <- A %>% select(-IndicatorName__1, -LocName__1)
keys <- as.character(unlist(X[X$key == 1, 1]))

# A %>% select(AgeID, AgeLabel) %>% unique() %>% as.data.frame() %>% arrange(AgeID)


subsetUNdata <- function(x, locID, sexID, year, ages) {
  B1 <- x %>% filter(LocID == locID)
  if (!(locID %in% B1$LocID)) {
    stop("LocID = ", locID, " does not exist in the input data.")
  }
  B2 <- B1 %>% filter(SexID == sexID)
  if (!(sexID %in% B2$SexID)) {
    stop("SexID = ", sexID, " it is not available for LocID = ", locID)
  }
  B3 <- B2 %>% filter(ReferencePeriod == year)
  if (!(year %in% B3$ReferencePeriod)) {
    stop("Year = ", year, " it is not available for LocID = ", locID, 
         " and SexID = ", sexID)
  }
  B4 <- B3 %>% filter(AgeStart %in% ages, !(AgeLabel %in% c("Total", "Unknown"))) %>% 
    arrange(AgeStart)
  if (!all(ages %in% B4$AgeStart)) {
    stop("The specified ages are not available. ",
         "\nAvailable ages: ", paste(B4$AgeStart, collapse = " "))
  }
  return(B4)
}

#' Check whether the input data contains an open age interval 
#' @inheritParams wrapPop5
#' @keywords internal
#' 
isOAG <- function(X){
  any(X$AgeSpan == -1)
}

#' Wrap function for beers, grabill and sprague functions
#' 
#' @param X Input data. UN format.
#' @param fn Method to be called from DemoTools
#' @param ... Other arguments to be passed on to the \code{\link[DemoTools]{beers}}, 
#' \code{\link[DemoTools]{grabill}} and \code{\link[DemoTools]{sprague}} functions.
#' @return A data.frame having the same number of colums as input data. Different 
#' numbers of rows. UN format.
#' @export
#' 
wrapPop5 <- function(X, fn = c("beers", "grabill", "sprague"), ...) {
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
    mutate(AgeStart = as.numeric(rownames(.)), 
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

# Example
B <- subsetUNdata(A, locID = 818, sexID = 1, year = 1976, 
                  ages = c(0, 1, seq(5, 75, by = 5)))

W1 <- wrapPop5(B, fn = "beers") 
W2 <- wrapPop5(B, fn = "grabill")
W3 <- wrapPop5(B, fn = "sprague") 

# ----------------------------------------------
# Plot
B$AgeSpan

B %>% select(AgeSpan, DataValue)
plot(B$AgeStart, B$DataValue/abs(B$AgeSpan), 
     type = "s", col = "gray", xlab = "Age", ylab = "Count")
lines(W1$AgeStart,  W1$DataValue, col = 2, lty = 1)
lines(W2$AgeStart,  W2$DataValue, col = 3, lty = 2)
lines(W3$AgeStart,  W3$DataValue,, col = 4, lty = 3)
legend("topright", 
       lty = c(1, 1:3), 
       col = c("gray", 2:4), 
       lwd = 2, 
       legend = c("grouped","Beers" ,"Grabill", "Sprague"))

# Fri Jul 20 14:20:17 2018 ------------------------------

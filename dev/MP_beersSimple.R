# Thu Jul 19 14:09:08 2018 ------------------------------
# Marius Pascariu

remove(list = ls())

library(DemoTools)
library(readxl)
library(tidyverse)


A <- read_excel("Export_DB_AFG1979_Pop_MP.xlsx")

#' Wrap function for DDSQLtools - 1st try
#' 
#' @param data Input data. UN format.
#' @param fn Method to be called from DemoTools
#' @return A data.frame having the same number of colums as input data. Different 
#' numbers of rows. UN format.
wrap_demoTools <- function(data, fn, ...) {
  B <- data %>% 
    filter(SexID == 1, !(AgeLabel %in% c("Total", "Unknown"))) %>% 
    select(ReferencePeriod, AgeStart, DataValue) %>% 
    arrange(AgeStart) %>% spread(key = ReferencePeriod, value = DataValue) %>% 
    as.data.frame()
  rownames(B) <- B$AgeStart
  C <- B %>% select(-AgeStart)
  D <- eval(call(fn, C, OAG = TRUE))  # OAG must be specifie outside. "..." missing in beersSimple()
  E <- D %>% as.data.frame() %>% 
    mutate(AgeStart = as.numeric(rownames(.)), 
           AgeSpan = 1, 
           AgeEnd = AgeStart + AgeSpan,
           AgeMid = AgeStart + AgeSpan/2,
           AgeLabel = AgeStart) %>% 
    gather(key = ReferencePeriod, value = DataValue, -(AgeStart:AgeLabel)) %>% 
    mutate(ReferencePeriod = as.numeric(ReferencePeriod))
  
  G <- data.frame(matrix(NA, ncol = ncol(A), nrow = nrow(E)))
  colnames(G) <- colnames(A)
  CnameA <- colnames(A)
  CnameE <- colnames(E)
  
  for (i in 1:length(CnameE)) {
    ii <- CnameE[i]
    G[, ii] <- E[, ii]
  }
  
  for (j in 1:length(CnameA)) {
    jj <- CnameA[j]
    
    isEmpty <- !(jj %in% CnameE)
    if (isEmpty) {
      G[, jj] <- unlist(A[1, jj])
    }
    
    IdontKnowTheRule <- jj %in% c("PK_StructuredDataID", "AgeID")
    if (IdontKnowTheRule) {
      G[, jj] <- NA
    }
  }
  return(G)
}

# Example
wrap_demoTools(data = A, fn = "beersSimple", OAG = TRUE)



# Fri Jul 27 11:15:21 2018 --------- Marius D. Pascariu ---
remove(list = ls())
library(readxl)
library(tidyverse)

# Load data
file1 <- paste0(getwd(), "/devdata/", "Pop5_Metadata.xlsx")
file2 <- paste0(getwd(), "/devdata/", "DemoData_Export_Pop1_Egypt_DB.xlsx")
file3 <- paste0(getwd(), "/devdata/", "DemoData_Export_Pop5_Egypt_DB.xlsx")
P1 <- read_excel(file2)
P5 <- read_excel(file3)

# keys <- as.character(unlist(X[X$key == 1, 1]))


subsetUNdata <- function(x, locID, sexID, year, ages) {
  # remove duplicate columns
  B0 <- x %>% select(-IndicatorName__1, -LocName__1)
  B1 <- B0 %>% filter(LocID == locID)
  if (!(locID %in% B1$LocID)) {
    stop("LocID = ", locID, " does not exist in the input data.", call. = F)
  }
  B2 <- B1 %>% filter(SexID == sexID)
  if (!(sexID %in% B2$SexID)) {
    stop("SexID = ", sexID, " it is not available for LocID = ", locID, call. = F)
  }
  B3 <- B2 %>% filter(ReferencePeriod == year)
  if (!(year %in% B3$ReferencePeriod)) {
    stop("Year = ", year, " it is not available for LocID = ", locID, 
         " and SexID = ", sexID, call. = F)
  }
  B4 <- B3 %>% filter(AgeStart %in% ages, !(AgeLabel %in% c("Total", "Unknown"))) %>% 
    arrange(AgeStart)
  if (!all(ages %in% B4$AgeStart)) {
    stop("The specified ages are not available. ",
         "\nAvailable ages: ", paste(B4$AgeStart, collapse = " "), call. = F)
  }
  return(B4)
}

P1s <- subsetUNdata(P1, locID = 818, sexID = 1, year = 2006, 
                    ages = 0:99)
P1s <- P1s %>% filter(AgeLabel != "95+")


P5s <- subsetUNdata(P5, locID = 818, sexID = 1, year = 1976, 
                    ages = c(0, 1, seq(5, 75, by = 5)))

DDSQLtools.data <- list(Pop1_Egypt_DB = P1s, Pop5_Egypt_DB = P5s)

devtools::use_data(DDSQLtools.data, overwrite = TRUE)

# Fri Jul 27 11:24:06 2018 ------------------------------






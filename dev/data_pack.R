# Fri Jul 20 14:30:16 2018 ------------------------------
### This code was used to add data in the package

rm(list = ls())
remove(list = ls())
library(readxl)
library(tidyverse)

# Load data
file1 <- paste0(getwd(), "/dev/", "Pop5_Metadata.xlsx")
file2 <- paste0(getwd(), "/dev/", "DemoData_Export_Pop5_Egypt+India_DB.xlsx")
X <- read_excel(file1)
A <- read_excel(file2)
# remove duplicate columns
A <- A %>% select(-IndicatorName__1, -LocName__1)
keys <- as.character(unlist(X[X$key == 1, 1]))


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

P5_Egypt1976 <- subsetUNdata(A, locID = 818, sexID = 1, year = 1976, 
                             ages = c(0, 1, seq(5, 75, by = 5)))


devtools::use_data(P5_Egypt1976, overwrite = TRUE)

# Fri Jul 20 14:38:45 2018 ------------------------------






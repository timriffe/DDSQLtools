# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: GNU General Public License v3.0
# Last update: Tue Nov 27 18:28:12 2018
# --------------------------------------------------- #
remove(list = ls())
library(readxl)
library(tidyverse)

# Load data
file1 <- paste0(getwd(), "/devdata/", "Pop5_Metadata.xlsx")
file2 <- paste0(getwd(), "/devdata/", "DemoData_Export_Pop1_Egypt_DB.xlsx")
file3 <- paste0(getwd(), "/devdata/", "DemoData_Export_Pop5_Egypt_DB.xlsx")
P1 <- read_excel(file2)
P5 <- read_excel(file3)

P5$SexName

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

P1_M <- subsetUNdata(P1, locID = 818, sexID = 1, year = 2006, ages = 0:99)
P1_M <- P1_M %>% filter(AgeLabel != "95+")
P1_F <- subsetUNdata(P1, locID = 818, sexID = 2, year = 2006, ages = 0:99)
P1_F <- P1_F %>% filter(AgeLabel != "95+")

x = c(0, 1, seq(5, 75, by = 5))
P5_M <- subsetUNdata(P5, locID = 818, sexID = 1, year = 1976, ages = x)
P5_F <- subsetUNdata(P5, locID = 818, sexID = 2, year = 1976, ages = x)



# Tue Nov 27 18:28:26 2018 ------------------------------

Mx <- paste0(getwd(), "/devdata/", "LTabr_error1.csv") %>% read.csv()


DDSQLtools.data <- list(Pop1_Egypt_M_DB = P1_M, Pop1_Egypt_F_DB = P1_F,
                        Pop5_Egypt_M_DB = P5_M, Pop5_Egypt_F_DB = P5_F,
                        Mx5 = Mx)

devtools::use_data(DDSQLtools.data, overwrite = TRUE)





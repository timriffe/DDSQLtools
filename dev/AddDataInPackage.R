# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 16:28:01 2018
# --------------------------------------------------- #

remove(list = ls())
library(readxl)
library(tidyverse)

# Download Abridge Data
P5 <- getRecordDataDetail(dataProcess = 2,   # Estimate
                          indicatorType = 8, # Population by age and sex
                          loc = 818,         # Egypt
                          locAreaType = 2,   # Whole area
                          subGroup = 2,      # Total or All groups
                          isComplete = 0)    # Age Distribution: Abridged

ageID_5 <- c(701:715, 765)
P5_M <- P5 %>% filter(ReferencePeriod == 2006,
                      AgeID %in% ageID_5,
                      SexID == 1, 
                      DataTypeID == 58)

P5_F <- P5 %>% filter(ReferencePeriod == 2006, 
                      AgeID %in% ageID_5,
                      SexID == 2, 
                      DataTypeID == 58)
dim(P5_M)
dim(P5_F)


# Download Single Age Data
P1 <- getRecordDataDetail(dataProcess = 2,   # Estimate
                          indicatorType = 8, # Population by age and sex
                          loc = 818,         # Egypt
                          locAreaType = 2,   # Whole area
                          subGroup = 2,      # Total or All groups
                          isComplete = 1)    # Age Distribution: Complete

ageID_1 <- c(2001:2099, 3220) # Select all ages 0-99+
P1_M <- P1 %>% filter(ReferencePeriod == 2006, 
                      AgeID %in% ageID_1,
                      SexID == 1, 
                      DataTypeID == 58)

P1_F <- P1 %>% filter(ReferencePeriod == 2006, 
                      AgeID %in% ageID_1,
                      SexID == 2, 
                      DataTypeID == 58)
dim(P1_M)
dim(P1_F)


# Tue Nov 27 18:28:26 2018 ------------------------------

# This is a small table
Mx <- paste0(getwd(), "/devdata/", "LTabr_error1.csv") %>% 
  read.csv() %>% 
  rename(ReferencePeriod = year, DataValue = Mx, SexID = SexCode) %>% 
  mutate(SexID = 1, IndicatorID = "nMx")

# Save data
DDSQLtools.data <- list(Pop1_Egypt_M_DB = P1_M, Pop1_Egypt_F_DB = P1_F,
                        Pop5_Egypt_M_DB = P5_M, Pop5_Egypt_F_DB = P5_F,
                        Mx5 = Mx)

devtools::use_data(DDSQLtools.data, overwrite = TRUE)





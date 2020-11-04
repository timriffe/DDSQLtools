# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sat Dec 15 16:28:01 2018
# --------------------------------------------------- #

remove(list = ls())
library(readxl)
library(tidyverse)

# Download Abridge Data
P5 <- get_recorddata(dataProcessIds = 2,   # Estimate
                    indicatorTypeIds = 8, # Population by age and sex
                    locIds = 818,         # Egypt
                    locAreaTypeIds = 2,   # Whole area
                    subGroupIds = 2,      # Total or All groups
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
P1 <- get_recorddata(dataProcessIds = 2,   # Estimate
                    indicatorTypeIds = 8, # Population by age and sex
                    locIds = 818,         # Egypt
                    locAreaTypeIds = 2,   # Whole area
                    subGroupIds = 2,      # Total or All groups
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

# From Jorge, November 4th 2020: Trying to replicate this but I can't
# find the file anywhere. I found a reference to LTabr in old DemoTools
# code but I couldn't figure out how to produce the file below.

# I tried by following this https://github.com/timriffe/DemoTools/issues/64
# and tried this because LTabr is not exported from DemoTools anymore:

# x <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
# Mx <- c(0.029677111, 0.006652641, 0.003428760, 0.003779019, 0.007071163,
#         0.013404195, 0.018407346, 0.022259325, 0.025713290, 0.036088883,
#         0.048121808, 0.073429838, 0.107445906, 0.153227234, 0.192783422,
#         0.254067819, 0.349650365, 0.604515543, 0.705407913, 0.828063757)
# DemoTools::lt_abridged(nMx = Mx, Age = x)

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

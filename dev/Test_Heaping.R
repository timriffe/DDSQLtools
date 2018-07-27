# Fri Jul 27 14:13:33 2018 --------- Marius D. Pascariu ---
remove(list = ls())
library(DemoTools)
library(DDSQLtools)
library(tidyverse)

Value <- c(80626,95823,104315,115813,100796,105086,97266,116328,
           75984,89982,95525,56973,78767,65672,53438,85014,
           47600,64363,42195,42262,73221,30080,34391,29072,
           20531,66171,24029,44227,24128,23599,82088,16454,
           22628,17108,12531,57325,17220,28425,16206,17532,
           65976,11593,15828,13541,8133,44696,11165,18543,
           12614,12041,55798,9324,10772,10453,6773,28358,
           9916,13348,8039,7583,42470,5288,5317,6582,
           3361,17949,3650,5873,3279,3336,27368,1965,
           2495,2319,1335,12022,1401,1668,1360,1185,
           9167,424,568,462,282,6206,343,409,333,291,4137,133,169,157,89,2068,68,81,66,57)
Age <- 0:99


p1 <- P1$DataValue
x1 <- P1$AgeStart


P1 <- DDSQLtools.data$Pop1_Egypt_DB
W <- doHeaping(P1)
W %>% select(DataProcessType, DataValue)


Value = p1
Age = x1

Whipple(Value, Age, ageMin = 25, ageMax = 60, digit = c(0,5))
Myers(Value, Age, ageMin = 10, ageMax = 90) 
Bachi(Value, Age, ageMin = 20, ageMax = 80, pasex = TRUE) 
CoaleLi(Value, Age, ageMin = 65, ageMax = 95, terms = 5, digit = 3)
Noumbissi(Value, Age, ageMin = 20, ageMax = 65, digit = 1)
Spoorenberg(Value, Age, ageMin = 20, ageMax = 65)




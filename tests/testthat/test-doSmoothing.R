# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sun Dec 16 13:14:01 2018
# --------------------------------------------------- #

remove(list = ls())
library(testthat)
library(DDSQLtools)

## DATA
I <- DDSQLtools.data$Pop5_Egypt_M_DB

# ------------------------------------------
## TEST Design

#' @param I Input
#' @param O Output
validate_smoothing_method <- function(I, O) {
  test_that("Test heaping methods", {
    expect_output(print(O))                      # 1. We always expect an output;
    expect_true(any(class(O) == "data.frame"))   # 2. Output should be a data.frame;
    expect_identical(dim(O), dim(I))             # 3. Output dimensions are identical with input dimensions
    expect_true(is.numeric(O$DataValue))         # 4. DataValue is numeric;
    expect_true(all(O$DataValue >= 0))           # 5. And all positive;
    expect_identical(O$AgeStart, I$AgeStart)     # 6. Same AgeStart as in input
    expect_identical(O$AgeEnd,   I$AgeEnd)       # 7. Same AgeEnd as in input
    expect_identical(O$AgeLabel, I$AgeLabel)     # 8. Same AgeLabel as in input
  })
  
}

# ------------------------------------------
## We run the tests here:

for (i in c("Carrier-Farrag", 
            "KKN", 
            "Arriaga",
            "United Nations", 
            "Strong", 
            "Zigzag", 
            "MAV")) { 
  
  cat("Test", i, "method: ")
  O <- doSmoothing(X = I, method = i)
  validate_smoothing_method(I, O)       # Validate.
  cat("OK\n")
}


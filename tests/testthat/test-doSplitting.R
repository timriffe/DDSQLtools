# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sun Dec 16 14:42:15 2018
# --------------------------------------------------- #

remove(list = ls())
library(testthat)
library(DDSQLtools)

## DATA
I1 <- DDSQLtools.data$Pop1_Egypt_M_DB
I5 <- DDSQLtools.data$Pop5_Egypt_M_DB

# ------------------------------------------
## TEST Design

#' @param I Input
#' @param O Output
validate_splitting_method <- function(I, O) {
  test_that("Test splitting methods", {
    expect_output(print(O))                        # 1. We always expect an output;
    expect_true(any(class(O) == "data.frame"))     # 2. Output should be a data.frame;
    expect_identical(ncol(O), ncol(I))             # 3. Output the same number of colums as in input;
    # expect_gt(nrow(O), nrow(I))                    # 4. Output has a greater number of rows?? Not always for "beers".
    expect_true(is.numeric(O$DataValue))           # 5. DataValue is numeric;
    # expect_true(all(O$DataValue >= 0))             # 6. And all positive?? Not always for "beers".
  })
}

# ------------------------------------------
## We run the tests here:

for (i in c("beers", 
            "grabill", 
            "sprague")) { 
  
  cat("Test", i, "method: ")
  O1 <- doSplitting(X = I1, fn = i)
  O5 <- doSplitting(X = I5, fn = i)
  validate_splitting_method(I1, O1)       # Validate.
  validate_splitting_method(I5, O5)       # Validate.
  cat("OK\n")
}

# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: CC-BY-NC 4.0
# Last update: Sun Dec 16 14:47:22 2018
# --------------------------------------------------- #

remove(list = ls())
library(testthat)
library(DDSQLtools)

## DATA
I <- DDSQLtools.data$Mx5


# ------------------------------------------
## TEST Design


#' @param I Input
#' @param O Output
validate_LT_method <- function(I, O) {
  test_that("Test LifeTable methods", {
    expect_output(print(O))                      # 1. We always expect an output;
    expect_true(any(class(O) == "data.frame"))   # 2. Output should be a data.frame;
    # expect_true(ncol(O) == ncol(I))              # 3. No of columns as in input? Not yet.
    expect_true(nrow(O) == 9 * nrow(I))          # 4. The number of rows is 8 times the number of rows in input, because we stack life-table indices.
    expect_true(is.numeric(O$DataValue))         # 5. DataValue is always numeric;
    # expect_true(O$DataValue >= 0)                # 6. And positive?? There are NaN. The values should be tested in DemoTools, this is just a wrapper.
  })
  
}
# ------------------------------------------
## We run the tests here:
expect_message(O <- doLifeTable(X = I, verbose = TRUE))  # expect a message
validate_LT_method(I, O)  # validate

expect_message(doLifeTable(X = I, verbose = FALSE), regexp = NA) # expect no message




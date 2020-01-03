library(testthat)
library(DDSQLtools)

## DATA
P1m <- DDSQLtools.data$Pop1_Egypt_M_DB # 1-year age groups data
P1f <- DDSQLtools.data$Pop1_Egypt_F_DB 
P5m <- DDSQLtools.data$Pop5_Egypt_M_DB # 5-year age groups data
P5f <- DDSQLtools.data$Pop5_Egypt_F_DB

# ------------------------------------------
## TEST Design

#' @param P1 Input 1
#' @param P2 Input 2
#' @param X Output
validate_here <- function(P1, P2, X) {
  test_that("Test Wrapper Output", {
    expect_output(print(X))                      # 1. We always expect an output;
    expect_true(any(class(X) == "data.frame"))   # 2. Output should be a data.frame;
    expect_true(ncol(X) == ncol(P1))             # 3. No of columns as in input;
    expect_true(ncol(X) == ncol(P2))             # 4. No of columns as in the other input;
    expect_true(nrow(X) == 1)                    # 5. The output always has only 1 row;
    expect_true(class(X$DataValue) == "numeric") # 6. I don't know about the other columns, but DataValue is always numeric;
    expect_true(X$DataValue >= 0)                # 7. And positive;
    expect_true(X$DataValue <= 1e3)              # 8. If the value in DataValue is to large maybe somebody should come back here and do a manual inspection.
  })
  
}

# ------------------------------------------
## We run the tests here:

# Do doCompare
for (i in c("ID", "IRD") ) { 
  cat("Test", i, "method: ")
  expect_message(X1 <- doCompare(pop1 = P1m, pop2 = P1f, fn = i), regexp = NA) # Expect no message here;
  expect_message(X5 <- doCompare(pop1 = P5m, pop2 = P5f, fn = i), regexp = NA) # Expect no message here;
  validate_here(P1 = P1m, P2 = P1f, X1)                                        # Validate using 1-year-age data;
  validate_here(P1 = P5m, P2 = P5f, X5)                                        # Validate using abridged data;
  expect_error(doCompare(pop1 = P1m, pop2 = P5f, fn = i))                      # Expect error here;
  cat("OK\n")
}

# Do doQualityChecks
for (i in c("sexRatioScore", 
            "ageSexAccuracy", 
            "ageSexAccuracyDasGupta")) { 
  
  cat("Test", i, "method: ")
  expect_message(X5 <- doQualityChecks(XY = P5m, XX = P5f, fn = i))   # Expect a message here;
  expect_message(X5 <- doQualityChecks(XY = P5m, XX = P5f, fn = i,    # The same but no message here;
                                       verbose = FALSE), regexp = NA)
  validate_here(P1 = P5m, P2 = P5f, X5)                               # Validate only for abridged data;
  expect_error(doQualityChecks(XY = P5m, XX = P1f, fn = i))           # Expect error here.
  cat("OK\n")
}


















library(testthat)
library(DDSQLtools)

## DATA
Mx5 <- DDSQLtools.data$Mx5 
x1  <- c(75, 80, 85, 90)        # ages to be used in fitting
x2  <- c(90, 95, 100, 105, 110) # ages for which the extrapolation is applied
M <- c("kannisto",              # Models
       "kannisto_makeham", 
       "gompertz", 
       "ggompertz", 
       "makeham", 
       "beard", 
       "beard_makeham", 
       "quadratic")


# ------------------------------------------
## TEST Design

#' @param I Input data
#' @param O Output data
validate_here <- function(I, O) {
  test_that("Test Wrapper Output", {
    expect_true(all(I$DataValue >= 0))           # 1. The input should be positive;
    expect_output(print(O))                      # 2. We always expect an output;
    expect_true(any(class(O) == "data.frame"))   # 3. Output should be a data.frame;
    # expect_true(ncol(O) == ncol(I))            # 4. No of columns as in input;
    expect_true(nrow(O) >= nrow(I))              # 5. The output must have the same or more rows as the input;
    expect_true(class(O$DataValue) == "numeric") # 6. DataValue is always numeric;
    expect_true(all(O$DataValue >= 0))           # 7. And positive;
    expect_true(all(O$DataValue <= 1e2))         # 8. If the value in DataValue is to large maybe somebody should come back here and do a manual inspection.
  })
  
}



# ------------------------------------------
## We run the tests here:

for (i in M) { 
  cat("Test", i, "method: ")
  expect_message(E1 <- doExtrapolate(X = Mx5, 
                                     x_fit = x1, 
                                     x_extr = x2, 
                                     law = i,
                                     verbose = TRUE)) # Expect a message here;
  validate_here(I = Mx5, O = E1)                      # Validate;
  cat("OK\n")
}

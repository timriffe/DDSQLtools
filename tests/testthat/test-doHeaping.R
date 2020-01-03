library(testthat)
library(DDSQLtools)

## DATA
P1 <- DDSQLtools.data$Pop1_Egypt_M_DB # 1-year age groups data

# ------------------------------------------
## TEST Design


#' @param I Input
#' @param O Output
validate_heaping_method <- function(I, O) {
  test_that("Test heaping methods", {
    expect_output(print(O))                      # 1. We always expect an output;
    expect_true(any(class(O) == "data.frame"))   # 2. Output should be a data.frame;
    expect_true(ncol(O) == ncol(I))              # 3. No of columns as in input;
    expect_true(nrow(O) == 1)                    # 4. The output always has only 1 row;
    expect_true(is.numeric(O$DataValue))         # 5. I don't know about the other columns, but DataValue is always numeric;
    expect_true(O$DataValue >= 0)                # 6. And positive;
    expect_true(O$DataValue <= 1e3)              # 7. If the value in DataValue is to large maybe somebody should come back here and do a manual inspection.
  })
  
}

# ------------------------------------------
## We run the tests here:

# Do doQualityChecks
for (i in c("Whipple", 
            "Myers", 
            "Bachi", 
            "CoaleLi", 
            "Noumbissi",
            "Spoorenberg", 
            "ageRatioScore", 
            "KannistoHeap", 
            "Jdanov")) { 
  
  cat("Test", i, "method: ")
  expect_message(O <- doHeaping(X = P1, fn = i))                          # Expect a message here too;
  expect_message(doHeaping(X = P1, fn = i, verbose = FALSE), regexp = NA) # The same but no message here;
  validate_heaping_method(P1, O)                                          # Validate.
  cat("OK\n")
}

## Some more tests here:
# Expect no message here because all the possible arguments are specified.
expect_message(doHeaping(X = P1, 
                         fn = "Jdanov", 
                         verbose = TRUE, 
                         Agei = seq(95, 105, by = 5)),
               regexp = NA)

# Is sensitive to typos
expect_error(doHeaping(X = P1, fn = "Jdanovv"))











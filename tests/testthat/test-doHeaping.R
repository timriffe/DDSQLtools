## DATA
P1 <- DDSQLtools.data$Pop1_Egypt_M_DB # 1-year age groups data

validate_heaping_method <- function(I, O) {
  test_that("Test heaping methods", {
    # 1. We always expect an output;
    expect_output(print(O))
    # 2. Output should be a data.frame;
    expect_true(any(class(O) == "data.frame"))
    # 3. No of columns as in input;
    expect_true(ncol(O) == ncol(I))
    # 4. The output always has only 1 row;
    expect_true(nrow(O) == 1)
    # 5. I don't know about the other columns, but DataValue is always numeric;
    expect_true(is.numeric(O$DataValue))
    # 6. And positive;
    expect_true(O$DataValue >= 0)
    # 7. If the value in DataValue is to large maybe somebody should come back
    # here and do a manual inspection.
    expect_true(O$DataValue <= 1e3)
  })
  
}

# doQualityChecks tests
for (i in c("Whipple", 
            "Myers", 
            "Bachi", 
            "CoaleLi", 
            "Noumbissi",
            "Spoorenberg", 
            "ageRatioScore", 
            "KannistoHeap", 
            "Jdanov")) { 

  test_that(paste0("doHeaping with ", i), {
    # Expect a message here too;
    expect_message(O <- doHeaping(X = P1, fn = i))
    # The same but no message here;
    expect_message(doHeaping(X = P1, fn = i, verbose = FALSE), regexp = NA)
    # Validate.
    validate_heaping_method(P1, O)
  })
}

test_that("doHeaping is sensitive to typos and messages", {
  # Expect no message here because all the possible arguments are specified.
  expect_message(doHeaping(X = P1, 
                           fn = "Jdanov", 
                           verbose = TRUE, 
                           Agei = seq(95, 105, by = 5)),
                 regexp = NA)

  # Is sensitive to typos
  expect_error(doHeaping(X = P1, fn = "Jdanovv"))
})

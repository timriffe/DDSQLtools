# Data
I <- DDSQLtools.data$Pop5_Egypt_M_DB


validate_smoothing_method <- function(I, O) {
    # 1. We always expect an output;
    expect_output(print(O))
    # 2. Output should be a data.frame;
    expect_true(any(class(O) == "data.frame"))
    # 3. Output dimensions are identical with input dimensions
    expect_identical(dim(O), dim(I))
    # 4. DataValue is numeric;
    expect_true(is.numeric(O$DataValue))
    # 5. And all positive;
    expect_true(all(O$DataValue >= 0))
    # 6. Same AgeStart as in input
    expect_identical(O$AgeStart, I$AgeStart)
    # 7. Same AgeEnd as in input
    expect_identical(O$AgeEnd,   I$AgeEnd)
    # 8. Same AgeLabel as in input
    expect_identical(O$AgeLabel, I$AgeLabel)
}

for (i in c("Carrier-Farrag", 
            "KKN", 
            "Arriaga",
            "United Nations", 
            "Strong", 
            "Zigzag", 
            "MAV")) { 

  test_that(paste0("doSmoothing with ", i), {
    O <- doSmoothing(X = I, method = i)
    validate_smoothing_method(I, O)
  })
}


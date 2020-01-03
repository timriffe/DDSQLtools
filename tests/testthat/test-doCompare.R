# Data for comparison
P1m <- DDSQLtools.data$Pop1_Egypt_M_DB # 1-year age groups data
P1f <- DDSQLtools.data$Pop1_Egypt_F_DB 
P5m <- DDSQLtools.data$Pop5_Egypt_M_DB # 5-year age groups data
P5f <- DDSQLtools.data$Pop5_Egypt_F_DB


validate_data <- function(P1, P2, X) {
  test_that("Test Wrapper Output", {
    # 1. We always expect an output;
    expect_output(print(X))
    # 2. Output should be a data.frame;
    expect_true(any(class(X) == "data.frame"))   
    # 3. No of columns as in input;
    expect_true(ncol(X) == ncol(P1))
    # 4. No of columns as in the other input;    
    expect_true(ncol(X) == ncol(P2))
    # 5. The output always has only 1 row;
    expect_true(nrow(X) == 1)
    # 6. I don't know about the other columns, but DataValue is always numeric;
    expect_true(class(X$DataValue) == "numeric")
    # 7. And positive;
    expect_true(X$DataValue >= 0)
    # 8. If the value in DataValue is to large maybe somebody should come back
    # here and do a manual inspection.
    expect_true(X$DataValue <= 1e3)              
  })
  
}

# Tests doCompare
for (i in c("ID", "IRD") ) { 
  test_that(paste0("doCompare works correctly with ", i), {
    # IRD returns a message but ID does't. Here, by setting NULL, we let
    # expect_message know that we're expecing a message but with NA
    # we're not.
    msg <- if (i == "IRD") NULL else NA

    # Expect no message here;
    expect_message(X1 <- doCompare(pop1 = P1m, pop2 = P1f, fn = i),
                   regexp = msg)
    # Expect no message here;
    expect_message(X5 <- doCompare(pop1 = P5m, pop2 = P5f, fn = i),
                   regexp = msg)
    # Validate using 1-year-age data;
    validate_data(P1 = P1m, P2 = P1f, X1)
    # Validate using abridged data;
    validate_data(P1 = P5m, P2 = P5f, X5)
    # Expect error here;
    expect_error(doCompare(pop1 = P1m, pop2 = P5f, fn = i))
  })
}

# Tests doQualityChecks
for (i in c("sexRatioScore", 
            "ageSexAccuracy", 
            "ageSexAccuracyDasGupta")) {
  
  test_that(paste0("doQualityChecks works correctly with ", i), {
    # Expect a message here;
    expect_message(X5 <- doQualityChecks(XY = P5m, XX = P5f, fn = i))
    # The same but no message here;
    expect_message(
      X5 <- doQualityChecks(XY = P5m, XX = P5f, fn = i, verbose = FALSE),
      regexp = NA
    )
    # Validate only for abridged data;
    validate_data(P1 = P5m, P2 = P5f, X5)
    # Expect error here.
    expect_error(doQualityChecks(XY = P5m, XX = P1f, fn = i))
  })
  
}

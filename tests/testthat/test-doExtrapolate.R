# Data for comparison
Mx5 <- DDSQLtools.data$Mx5 
x1  <- c(75, 80, 85, 90)        # ages to be used in fitting
x2  <- c(90, 95, 100, 105, 110) # ages for which the extrapolation is applied
M <- c("kannisto",
       "kannisto_makeham", 
       "makeham",
       "gompertz",
       "ggompertz",
       "beard",
       "beard_makeham", 
       "quadratic")


validate_data <- function(I, O) {
    # 1. The input should be positive;
    expect_true(all(I$DataValue >= 0))
    # 2. We always expect an output;
    expect_output(print(O))
    # 3. Output should be a data.frame;
    expect_true(any(class(O) == "data.frame"))
    # 4. No of columns as in input;
    # expect_true(ncol(O) == ncol(I))
    # 5. The output must have the same or more rows as the input;
    expect_true(nrow(O) >= nrow(I))
    # 6. DataValue is always numeric;
    expect_true(class(O$DataValue) == "numeric")
    # 7. And positive;
    expect_true(all(O$DataValue >= 0))
    # 8. If the value in DataValue is to large maybe somebody should come back
    # here and do a manual inspection.
    expect_true(all(O$DataValue <= 1e2))
}

for (i in M) {
  test_that(paste0("do_extrapolate with ", i), {
    # Expect a message here;
    expect_message(
      E1 <- do_extrapolate(X = Mx5,
                          x_fit = x1,
                          x_extr = x2,
                          law = i,
                          verbose = TRUE)
    )

    validate_data(I = Mx5, O = E1)
  })
}

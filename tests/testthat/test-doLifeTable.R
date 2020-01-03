# Data
I <- DDSQLtools.data$Mx5

validate_LT_method <- function(I, O) {
  test_that("Test LifeTable methods", {
    # 1. We always expect an output;
    expect_output(print(O))
    # 2. Output should be a data.frame;
    expect_true(any(class(O) == "data.frame"))
    # 3. No of columns as in input? Not yet.
    # expect_true(ncol(O) == ncol(I))
    # 4. The number of rows is 8 times the number of
    # rows in input, because we stack life-table indices.
    expect_true(nrow(O) == 9 * nrow(I))
    # 5. DataValue is always numeric;
    expect_true(is.numeric(O$DataValue))
    # 6. And positive?? There are NaN. The values should be tested in DemoTools,
    # this is just a wrapper.
    # expect_true(O$DataValue >= 0)                
  })
  
}

test_that("doLifeTable doesn't expect message", {
  # Expect a message
  # Save result for use outside of this environment in
  # `validate_LT_method`.
  expect_message(O <<- doLifeTable(X = I, verbose = TRUE))

  # Expect no message
  expect_message(doLifeTable(X = I, verbose = FALSE), regexp = NA)
})

validate_LT_method(I, O)  # validate

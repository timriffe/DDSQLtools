# Data
I1 <- DDSQLtools.data$Pop1_Egypt_M_DB
I5 <- DDSQLtools.data$Pop5_Egypt_M_DB


validate_splitting_method <- function(I, O) {
    # 1. We always expect an output;
    expect_output(print(O))
    # 2. Output should be a data.frame;
    expect_true(any(class(O) == "data.frame"))
    # 3. Output the same number of colums as in input;
    expect_identical(ncol(O), ncol(I))
    # 4. Output has a greater number of rows?? Not always for "beers".
    # expect_gt(nrow(O), nrow(I))
    # 5. DataValue is numeric;
    expect_true(is.numeric(O$DataValue))
    # 6. And all positive?? Not always for "beers".
    # expect_true(all(O$DataValue >= 0))
}

for (i in c("beers", 
            "grabill", 
            "sprague")) { 

  test_that(paste0("do_splitting works with ", i), {
    O1 <- do_splitting(X = I1, fn = i)
    O5 <- do_splitting(X = I5, fn = i)
    validate_splitting_method(I1, O1)
    validate_splitting_method(I5, O5)
  })
}

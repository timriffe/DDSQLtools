
# Author: tim
###############################################################################
context("utils")

test_that("check_values throws errors as expected", {
			x <- runif(10)
			x <- log(x/(1-x)) # logit
			
			# this is a set of expectations that are checked. The numbers 
			# 5.5 and 6.7 are pulled from the book example.
			expect_error(check_values(x,c(0,1),categorical = FALSE))
		
			}
)


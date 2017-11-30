
# Author: tim
###############################################################################
context("utils")

# Table 7.20 in Siegel and Swanson (2004). Make sure cited in IRDID.R
# under @references tag (using \insertRef{bibtextag}{DemoTools}

test_that("check_values throws errors as expected", {
			x <- runif(10)
			x <- log(x/(1-x)) # logit
			
			# this is a set of expectations that are checked. The numbers 
			# 5.5 and 6.7 are pulled from the book example.
			expect_error(check_values(x,c(0,1),categorical = FALSE))
		
			}
)


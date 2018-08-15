#install_github("timriffe/TimUtils/TimUtils")
#install_github("hadley/devtools")
# install.packages("devtools")
shhh <- function(expr){
	capture.output(x <- suppressPackageStartupMessages(
					suppressMessages(suppressWarnings(expr))))
	invisible(x)
}
#library(DemoTools)
library(devtools)
library(TimUtils)
# MP, you can do something like this so it works for both of us
if (system("whoami",intern=TRUE) == "tim"){
	# if I'm on the laptop
	setwd("/home/tim/git/DDSQLtools")
} 

# do this whenever new functions are added to /R, or whenever roxygen is updated
document()
versionIncrement(
		major = FALSE,       # only for releases
		mid = FALSE,         # major functionality added
		minor = TRUE,        # whenever documentation renewed, any patch, tweak, or fix
		maxdigits = c(2,2,3),# maybe 4 required?
		README = TRUE)       # update README dev version badge

# run this to get access to already-written functions
load_all()

# do this whenever major changes happen
check()

# try github install
install_github("timriffe/DDSQLtools")



versionIncrement(
		major = FALSE,       # only for releases
		mid = TRUE,         # major functionality added
		minor = FALSE,        # whenever documentation renewed, any patch, tweak, or fix
		maxdigits = c(2,2,3),# maybe 4 required?
		README = TRUE)       # update README dev version badge



library(devtools)

# MP, you can do something like this so it works for both of us
if (system("whoami",intern=TRUE) == "tim"){
	# if I'm on the laptop
	setwd("/home/tim/git/DDSQLtools")
} 

#install_github("hadley/devtools")
# do this whenever new functions are added to /R, or whenever roxygen is updated
document()

# run this to get access to already-written functions
load_all()

# do this whenever major changes happen
check()

# try github install
install_github("timriffe/DDSQLtools")

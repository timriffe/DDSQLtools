library(devtools)
#install_github("hadley/devtools")
# do this whenever new functions are added to /R, or whenever roxygen is updated
document("/home/tim/git/DDSQLtools")

# run this to get access to already-written functions
load_all("/home/tim/git/DDSQLtools")

# do this whenever major changes happen
check("/home/tim/git/DDSQLtools")

# try github install
install_github("timriffe/DDSQLtools")
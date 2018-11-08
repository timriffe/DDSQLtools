# Author: tim
###############################################################################

# code to do API queries on values + labels and
# convert them into lookup tables for the package.

#install.packages("rjson")
library(rjson)
library(reshape2)
# url from Patrick
this.url <- "http://24.239.36.16:9654/un2/api/country?addDefault=false&includeDependencies=false&includeFormerCountries=false"
Clist <- fromJSON(file = this.url)
# get tidy data.frame
Countries <- do.call("rbind",lapply(Clist, as.data.frame))
head(Countries)
# Indicators:
# trial and error url
this.url <- "https://popdiv.dfs.un.org/demodata/api/Indicator?addDefault=false"
Ilist   <- fromJSON(file = this.url)
# get tidy data.frame
Indicators <- do.call("rbind",lapply(Ilist, as.data.frame))
head(Indicators)

# however other urls have not been so easy with trial and error, many other code lists needed

this.url <-"http://24.239.36.16:9654/un2/api/recordDataDetail?dataProcess=2&indicatorType=8&isComplete=0&loc=4&locAreaType=2&subGroup=2"

X <- fromJSON(file = this.url)
library(httr)
install.packages("jsonlite", repos="http://cran.r-project.org")
X <- jsonlite::fromJSON(this.url)





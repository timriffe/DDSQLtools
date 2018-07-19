# Author: tim
###############################################################################

# code to do API queries on values + labels and
# convert them into lookup tables for the package.

#install.packages("rjson")
library(rjson)
library(reshape2)
# url from Patrick
this.url <- "https://popdiv.dfs.un.org/demodata/api/country?addDefault=false&includeDependencies=false&includeFormerCountries=false"
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
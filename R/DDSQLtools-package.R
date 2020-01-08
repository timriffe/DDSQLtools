# DDSQLtools: Wrappers for DemoTools functions to work with UNPD SQL internal 
# data format

# Do not remove DemoTools from here. If you refer
# to functions with DemoTools:: there will be
# warnings given that all DemoTools functions
# which have been renamed to snake_case extract
# the first part of the function being called for
# to figure out if you're using the old or new function.
# this means that it extracts `DemoTools::`, for example,
# instead of "extra.mortality". For this reason, we export
# DemoTools as a whole and do not refer to it with `DemoTools::`.
#' @import DemoTools
#' @name DDSQLtools
#' @docType package
"_PACKAGE"

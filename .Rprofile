if (interactive()) {

  library(devtools)

  check <- function(...) {
    install_github("timriffe/DemoTools")
    usethis::use_package("DemoTools", min_version = packageVersion("DemoTools"))
    devtools::check()
  }

  cat("When checking the package don't run `devtools::check()` just run check(); check has been rewritten to install and update minimum version in the DESCRIPTION file for `DemoTools`.")
}


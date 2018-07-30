

#' Wrap function for the age-heaping methods
#' 
#' Wrap function for the age-heaping methods: 
#' \code{\link[DemoTools]{Whipple}},
#' \code{\link[DemoTools]{Myers}},
#' \code{\link[DemoTools]{Bachi}},
#' \code{\link[DemoTools]{CoaleLi}},
#' \code{\link[DemoTools]{Noumbissi}} and
#' \code{\link[DemoTools]{Spoorenberg}}.
#' @inheritParams doSplitting
#' @inheritParams DemoTools::Myers
#' @examples 
#' P1 <- DDSQLtools.data$Pop1_Egypt_DB
#' W <- doHeaping(P1)
#' 
#' W[, c("DataProcessType", "DataValue")]
#' 
#' @export
#' 
doHeaping <- function(X, ageMin = 10, ageMax = 90, ...) {
  AgeStart = AgeSpan <- NULL # hack CRAN note
  
  A <- X$DataValue
  B <- X$AgeStart
  fn <- c("Whipple", "Myers", "Bachi", "CoaleLi", "Noumbissi", "Spoorenberg")
  
  G1 <- Whipple(A, B, ageMin, ageMax, ...)
  G2 <- Myers(A, B, ageMin, ageMax) 
  G3 <- Bachi(A, B, ageMin, ageMax, ...) 
  G4 <- CoaleLi(A, B, ageMin, ageMax, ...)
  G5 <- Noumbissi(A, B, ageMin, ageMax, ...)
  G6 <- Spoorenberg(A, B, ageMin, ageMax)
  
  G <- c(G1, G2, G3, G4, G5, G6) %>% 
    as.data.frame() %>% dplyr::rename(DataValue = ".") %>%  
    mutate(AgeStart = min(B), 
           AgeSpan = max(B) - min(B) + 1, 
           AgeEnd = max(B),
           AgeMid = AgeStart + AgeSpan/2,
           AgeLabel = paste0(min(B), "-", max(B)),
           DataProcessType = fn,
           ReferencePeriod = unique(X$ReferencePeriod))
  
  G$DataProcess <- deparse(match.call())
  out <- formatOutputTable(X, G)
  return(out)
}

